#!/usr/bin/env perl
# vim: ts=2 sw=2 expandtab

# Tests whether timeout & retry artificially inflates the measured
# round-trip time.  Thanks to Ralph Schmitt, who reported this
# problem.

# Unfortunately this test relies upon the assumption that one can ping
# 127.0.0.1.  If you're on Mac OS X 10.5 for example, turn off
# "stealth mode":
#
# System Preferences > Security > Firewall > Advanced > disable
# "Enable stealth mode".

use warnings;
use strict;

use POE qw(Component::Client::Ping);

BEGIN {
  $| = 1;
  unless (POE::Component::Client::Ping->can_open_socket()) {
    print "1..0 # skipped: ICMP ping requires root privilege\n";
    exit 0;
  }
};

use Test::More tests => 6;
my $retries = 5;

POE::Component::Client::Ping->spawn(
  Alias               => "pingthing",  # defaults to "pinger"
  Retry               => $retries,     # defaults to 1 attempt
  Parallelism         => 64,           # defaults to autodetect
  BufferSize          => 65536,        # defaults to undef
);

my $count = 0;

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
      $count++;
    },
  },
);

POE::Kernel->run();
is( $count, $retries, "$retries retries ok");
exit;
