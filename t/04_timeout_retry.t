#!/usr/bin/env perl

# Tests whether timeout & retry artificially inflates the measured
# round-trip time.  Thanks to Ralph Schmitt, who reported this
# problem.

use warnings;
use strict;
use POE qw(Component::Client::Ping);

use Test::More tests => 2;

POE::Component::Client::Ping->spawn(
	Alias               => "pingthing",  # defaults to "pinger"
	Retry               => 2,            # defaults to 1 attempt
	Parallelism         => 64,           # defaults to autodetect
	BufferSize          => 65536,        # defaults to undef
);

POE::Session->create(
	inline_states => {
		_start => sub {
			$_[KERNEL]->post( pingthing => ping => pong => "127.0.0.1" );
		},
		pong => sub {
			my ($req, $rsp) = @_[ARG0, ARG1];
			my $round_trip = $rsp->[1];
			return unless defined $round_trip; # final timeout
			ok( $round_trip < 1, "response time not affected by timeout" );
		},
	},
);

POE::Kernel->run();
exit;
