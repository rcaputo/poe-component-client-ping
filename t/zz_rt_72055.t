#!/usr/bin/env perl

# Something in the request queue is discarding responses.
# Reported in https://rt.cpan.org/Ticket/Display.html?id=72055
#
# Losing a number of pings in the initial phase occurs when special
# conditions are meet:
#
# * Pings are sent to the same host.
# * Parallelism is 2 or more.
# * Al least 2 ping events are created in a row (initial pings).
#
# The number of lost pings: min( Parallelism, initial pings ) - 1.

use strict;
use warnings;

use Test::More tests => 1;

use POE qw( Component::Client::Ping );

POE::Component::Client::Ping->spawn(Parallelism => 3);

POE::Session->create(
	inline_states => {
		_start => sub {
			$_[HEAP]{got} = $_[HEAP]{expected} = 0;

			# It's bad technique to send all the requets at once, but we're
			# doing this to expose a bug in the module's queuing logic.

			my @hosts = ( ('127.0.0.1') x 5 );
			foreach (@hosts) {
				++$_[HEAP]{expected};
				$_[KERNEL]->post('pinger', 'ping', 'pong', $_);
			}
		},

		_stop => sub {
			is(
				$_[HEAP]{got}, $_[HEAP]{expected},
				"got the right number of responses"
			);
		},

		pong => sub {
			++$_[HEAP]->{got} if defined $_[ARG1]->[0];
			$_[KERNEL]->yield('ping');
		},
	},
);

POE::Kernel->run;
