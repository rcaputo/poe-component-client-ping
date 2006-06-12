#!/usr/bin/perl
# $Id$
# vim: filetype=perl

use strict;
use warnings;

use POE qw(Component::Client::Ping);
use Test::More tests => 1;

$|=1;

POE::Component::Client::Ping->spawn( Alias => 'pinger', OneReply => 1 );

POE::Session->create(
  package_states => [
    'main' => [ qw(_start pong) ],
  ],
  options => { trace => 0 },
);

POE::Kernel->run();
exit 0;

sub _start {
  my ($kernel,$heap) = @_[KERNEL,HEAP];
  $kernel->post( 'pinger', 'ping', [ 'pong', 'foo' ], "poe.perl.org" );
}

sub pong {
  my ($heap, $request, $response) = @_[HEAP, ARG0, ARG1];
  $request->[3] = "(undef)" unless defined $request->[3];
  ok($request->[3] eq "foo", "got arbitrary data: $request->[3]");
}

