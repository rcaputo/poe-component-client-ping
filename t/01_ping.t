#!/usr/bin/perl -w
# $Id$

use strict;

use lib '/home/troc/perl/poe';
sub POE::Kernel::ASSERT_DEFAULT () { 1 }
use POE qw(Component::Client::Ping);

$| = 1;
print "1..4\n";

sub PING_TIMEOUT () { 1 }; # seconds between pings
sub PING_COUNT   () { 1 }; # ping repetitions
sub DEBUG        () { 0 }; # display more information

#------------------------------------------------------------------------------
# A bunch of addresses to ping.

my @addresses =
  qw( 10.0.0.0 172.16.0.0 192.168.0.0 127.0.0.0
      204.152.190.72 64.209.200.100 206.86.0.23 128.138.129.31
      206.132.27.156 192.116.253.10 207.69.200.132 207.138.35.60
      166.84.185.32
    );

#------------------------------------------------------------------------------
# This session uses the ping component to resolve things.

sub client_start {
  my ($kernel, $session, $heap) = @_[KERNEL, SESSION, HEAP];

  DEBUG and warn($session->ID, ": starting pinger client session...\n");

  # Set up recording.
  $heap->{requests}    = 0;
  $heap->{answers}     = 0;
  $heap->{dones}       = 0;
  $heap->{ping_counts} = { };

  # Start pinging.
  foreach my $address (@addresses) {
    $heap->{ping_counts}->{$address} = 0;
    $kernel->call( $session, ping => $address );
  }
}

sub client_send_ping {
  my ($kernel, $session, $heap, $address) = @_[KERNEL, SESSION, HEAP, ARG0];

  DEBUG and warn( $session->ID, ": pinging $address...\n" );

  $heap->{requests}++;
  $heap->{ping_counts}->{$address}++;
  $kernel->post( 'pinger',     # Post the request to the 'pinger'.
                 'ping',       # Ask it to 'ping' an address.
                 'pong',       # Have it post an answer to my 'pong' state.
                 $address,     # This is the address we want it to ping.
                 PING_TIMEOUT  # This is the optional time to wait.
               );
}

sub client_got_pong {
  my ($kernel, $session, $heap, $request_packet, $response_packet) =
    @_[KERNEL, SESSION, HEAP, ARG0, ARG1];

  my ($request_address, $request_timeout, $request_time) = @{$request_packet};
  my ($response_address, $roundtrip_time, $reply_time)   = @{$response_packet};

  if (defined $response_address) {
    DEBUG and warn
      sprintf( "%d: ping to %-15.15s at %10d. pong from %-15.15s in %6.3f s\n",
               $session->ID,
               $request_address, $request_time,
               $response_address, $roundtrip_time
             );

    $heap->{answers}++ if $roundtrip_time <= $request_timeout;
  }
  else {
    DEBUG and warn( $session->ID, ": time's up for $request_address...\n" );

    $kernel->yield(ping => $request_address)
      if $heap->{ping_counts}->{$request_address} < PING_COUNT;

    $heap->{dones}++;
  }
}

sub client_stop {
  my ($session, $heap) = @_[SESSION, HEAP];
  DEBUG and warn( $session->ID, ": pinger client session stopped...\n" );

  print 'not ' unless ( $heap->{requests} == $heap->{dones} and
                        $heap->{answers}
                      );
  print 'ok ', $session->ID, "\n";
}

#------------------------------------------------------------------------------

# Create a pinger component.
POE::Component::Client::Ping->spawn
  ( Alias   => 'pinger',     # This is the name it'll be known by.
    Timeout => PING_TIMEOUT, # This is how long it waits for echo replies.
  );

# Create two sessions that will use the pinger.  This tests
# concurrency against the same addresses.
for (my $session_index = 0; $session_index < 2; $session_index++) {
  POE::Session->create
    ( inline_states =>
      { _start => \&client_start,
        _stop  => \&client_stop,
        pong   => \&client_got_pong,
        ping   => \&client_send_ping,
      }
    );
}

print "ok 1\n";

# Run it all until done.
$poe_kernel->run();

print "ok 4\n";

exit;
