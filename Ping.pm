# $Id$
# License and documentation are after __END__.

package POE::Component::Client::Ping;

use Exporter;
@ISA = qw(Exporter);
@EXPORT_OK = qw(REQ_ADDRESS REQ_TIMEOUT REQ_TIME REQ_USER_ARGS
		RES_ADDRESS RES_ROUNDTRIP RES_TIME);
%EXPORT_TAGS = ('const' => [ qw(REQ_ADDRESS REQ_TIMEOUT REQ_TIME REQ_USER_ARGS
				RES_ADDRESS RES_ROUNDTRIP RES_TIME) ]
		);
use strict;

use vars qw($VERSION);
$VERSION = '0.97';

use Carp qw(croak);
use Symbol qw(gensym);
use Socket;
use Time::HiRes qw(time);

use POE::Session;

sub DEBUG        () { 0 }; # Enable more information.
sub DEBUG_SOCKET () { 0 }; # Watch the socket open and close.

# Spawn a new PoCo::Client::Ping session.  This basically is a
# constructor, but it isn't named "new" because it doesn't create a
# usable object.  Instead, it spawns the object off as a session.

sub spawn {
  my $type = shift;

  croak "$type requires an even number of parameters" if @_ % 2;
  my %params = @_;

  croak "$type requires root privilege"
    if $> and ($^O ne 'VMS') and not defined $params{Socket};

  my $alias = delete $params{Alias};
  $alias = 'pinger' unless defined $alias and length $alias;

  my $timeout = delete $params{Timeout};
  $timeout = 1 unless defined $timeout and $timeout >= 0;

  my $onereply = delete $params{OneReply};

  my $socket = delete $params{Socket};

  croak( "$type doesn't know these parameters: ",
         join(', ', sort keys %params)
       ) if scalar keys %params;

  POE::Session->create
    ( inline_states =>
      { _start   => \&poco_ping_start,
        ping     => \&poco_ping_ping,
        clear    => \&poco_ping_clear,
        got_pong => \&poco_ping_pong,
        _default => \&poco_ping_default,
      },
      args => [ $alias, $timeout, $socket, $onereply ],
    );

  undef;
}

# ping_by_seq structure offsets.

sub PBS_POSTBACK     () { 0 };
sub PBS_SESSION      () { 1 };
sub PBS_ADDRESS      () { 2 };
sub PBS_REQUEST_TIME () { 3 };

# request_packet offsets
sub REQ_ADDRESS       () { 0 };
sub REQ_TIMEOUT       () { 1 };
sub REQ_TIME          () { 2 };
sub REQ_USER_ARGS     () { 3 };

# response_packet offsets
sub RES_ADDRESS       () { 0 };
sub RES_ROUNDTRIP     () { 1 };
sub RES_TIME          () { 2 };

# "Static" variables which will be shared across multiple instances.

my $pid = $$ & 0xFFFF;
my $seq = 0;

# Start the pinger session.  Record running stats, and create the
# socket which will be used to ping.

sub poco_ping_start {
  my ($kernel, $heap, $alias, $timeout, $socket, $onereply) =
    @_[KERNEL, HEAP, ARG0..ARG3];

  $heap->{data}          = 'Use POE!' x 7;        # 56 data bytes :)
  $heap->{data_size}     = length($heap->{data});
  $heap->{timeout}       = $timeout;
  $heap->{onereply}      = $onereply;
  $heap->{ping_by_seq}   = { };  # keyed on sequence number
  $heap->{addr_to_seq}   = { };  # keyed on request address, then sender
  $heap->{socket_handle} = $socket if defined $socket;
  $kernel->alias_set($alias);
}

# ICMP echo constants. Types, structures, and fields.  Cribbed
# mercilessly from Net::Ping.

sub ICMP_ECHOREPLY () { 0 }
sub ICMP_ECHO      () { 8 }
sub ICMP_STRUCT    () { 'C2 S3 A' }
sub ICMP_SUBCODE   () { 0 }
sub ICMP_FLAGS     () { 0 }
sub ICMP_PORT      () { 0 }

# Request a ping.  This code borrows heavily from Net::Ping.

sub poco_ping_ping {
  my ($kernel, $heap, $sender, $event, $address, $timeout) =
    @_[KERNEL, HEAP, SENDER, ARG0, ARG1, ARG2];

  # No current pings.  Open a socket.
  unless (exists $heap->{socket_handle}) {
    DEBUG_SOCKET and warn "opening a raw socket for icmp";

    my $protocol = (getprotobyname('icmp'))[2]
      or die "can't get icmp protocol by name: $!";

    my $socket = gensym();
    socket($socket, PF_INET, SOCK_RAW, $protocol)
      or die "can't create icmp socket: $!";

    $kernel->select_read($heap->{socket_handle} = $socket, 'got_pong');
  }

  # Get the timeout, or default to the one set for the component.
  $timeout = $heap->{timeout} unless defined $timeout and $timeout > 0;

  # Find an unused sequence number.
  while (1) {
    $seq = ($seq + 1) & 0xFFFF;
    last unless exists $heap->{ping_by_seq}->{$seq};
  }

  my $checksum = 0;

  # Build the message without a checksum.
  my $msg = pack( ICMP_STRUCT . $heap->{data_size},
                  ICMP_ECHO, ICMP_SUBCODE,
                  $checksum, $pid, $seq, $heap->{data}
                );

  ### Begin checksum calculation section.

  # Sum up short integers in the packet.
  my $shorts = int(length($msg) / 2);
  foreach my $short (unpack "S$shorts", $msg) {
    $checksum += $short;
  }

  # If there's an odd byte, add that in as well.
  $checksum += ord(substr($msg, -1)) if length($msg) % 2;

  # Fold the high short into the low one twice, and then complement.
  $checksum = ($checksum >> 16) + ($checksum & 0xFFFF);
  $checksum = ~( ($checksum >> 16) + $checksum) & 0xFFFF;

  ### Cease checksum calculation section.

  # Rebuild the message with the checksum this time.
  $msg = pack( ICMP_STRUCT . $heap->{data_size},
               ICMP_ECHO, ICMP_SUBCODE,
               $checksum, $pid, $seq, $heap->{data}
             );

  # Record the message's length.  This is constant, but we do it here
  # anyway.  It's also used to flag when we start requesting replies.
  $heap->{message_length} = length($msg);

  # Record information about the ping request.
  my @user_args = ();
  if (ref($event) eq "ARRAY") {
      @user_args = @{ $event };
      $event = shift @user_args;
  }

  # Build an address to send the ping at.
  my $usable_address = ( (length($address) == 4)
                         ? $address
                         : inet_aton($address)
                       );

  # Return failure if an address was not resolvable.  This simulates
  # the postback behavior.
  unless (defined $usable_address) {
    $kernel->post( $sender, $event,
                   [ $address, $timeout, time() ],
                   [ undef, undef, time() ],
                 );
    return;
  }

  my $socket_address = pack_sockaddr_in(ICMP_PORT, $usable_address);

  # Send the packet.  If send() fails, then we bail with an error.
  unless (send($heap->{socket_handle}, $msg, ICMP_FLAGS, $socket_address)) {
    $kernel->post( $sender, $event,
                   [ $address, $timeout, time() ],
                   [ undef, undef, time() ],
                 );
    return;
  }

  # Set a timeout based on the sequence number.
  $kernel->delay( $seq => $timeout );

  $heap->{ping_by_seq}->{$seq} =
    [ # PBS_POSTBACK
      $sender->postback($event, $address, $timeout, time(), @user_args),
      "$sender",   # PBS_SESSION (stringified to weaken reference)
      $address,    # PBS_ADDRESS
      time()       # PBS_REQUEST_TIME
    ];

  $heap->{addr_to_seqr}->{$sender}->{$address} = $seq;
}

# Clear a ping postback by address.  The sender+address pair are a
# unique ID into the pinger's data.

sub poco_ping_clear {
  my ($kernel, $heap, $sender, $address) = @_[KERNEL, HEAP, SENDER, ARG0];

  # Is the sender still waiting for anything?
  return unless exists $heap->{addr_to_seq}->{$sender};

  # Try to clear a single ping if an address was specified.
  if (defined $address) {

    # Don't bother if we don't have it.
    return unless exists $heap->{addr_to_seq}->{$sender}->{$address};

    # Stop mapping the sender+address pair to that sequence number.
    my $seq = delete $heap->{addr_to_seq}->{$sender}->{$address};

    # Stop tracking the sender if that was the last address.
    delete $heap->{addr_to_seq}->{$sender}
      unless scalar(keys %{$heap->{addr_to_seq}->{$sender}});

    # Discard the postback for the discarded sequence number.
    delete $heap->{ping_by_seq}->{$seq};

    # Stop the timeout, if any, for the ping.
    $kernel->delay($seq);
  }

  # No address was specified.  Clear all the pings for this session.
  else {
    # First discard all the ping records.
    foreach my $seq (values %{$heap->{addr_to_seq}->{$sender}}) {
      delete $heap->{ping_by_seq}->{$seq};
      $kernel->delay($seq);
    }

    # Now clear all the postbacks for the sender.
    delete $heap->{addr_to_seq}->{$sender};
  }

  # No more pings waiting.  Close the socket.
  unless (scalar keys %{$heap->{ping_by_seq}}) {
    DEBUG_SOCKET and warn "closing the raw icmp socket";
    $kernel->select_read( delete $heap->{socket_handle} );
  }
}

# Something has arrived.  Try to match it against something being
# waited for.

sub poco_ping_pong {
  my ($kernel, $heap, $socket) = @_[KERNEL, HEAP, ARG0];

  # Record the receive time for possible use later.
  my $now = time();

  # Receive a message on the ICMP port.
  my $recv_message = '';
  my $from_saddr = recv($socket, $recv_message, 1500, ICMP_FLAGS);

  # We haven't yet sent a message, so don't bother with whatever we've
  # received.
  return unless defined $heap->{message_length};

  # Unpack the packet's sender address.
  my ($from_port, $from_ip) = unpack_sockaddr_in($from_saddr);

  # Unpack the packet itself.
  my ( $from_type, $from_subcode,
       $from_checksum, $from_pid, $from_seq, $from_message
     )  = unpack( ICMP_STRUCT . $heap->{data_size},
                  substr($recv_message, -$heap->{message_length})
                );

  DEBUG and do {
    warn ",----- packet from ", inet_ntoa($from_ip), ", port $from_port\n";
    warn "| type = $from_type / subtype = $from_subcode\n";
    warn "| checksum = $from_checksum, pid = $from_pid, seq = $from_seq\n";
    warn "| message: $from_message\n";
    warn "`------------------------------------------------------------\n";
  };

  # Not an ICMP echo reply.  Move along.
  return unless $from_type == ICMP_ECHOREPLY;

  DEBUG and warn "it's an ICMP echo reply";

  # Not from this process.  Move along.
  return unless $from_pid == $pid;

  DEBUG and warn "it's from this process";

  # Not waiting for a response with that sequence number.  Move along.
  return unless exists $heap->{ping_by_seq}->{$from_seq};

  DEBUG and warn "it's one we're waiting for";

  # This is the response we're looking for.  Calculate the round trip
  # time, and map it to a postback.
  my $trip_time = $now - $heap->{ping_by_seq}->{$from_seq}->[PBS_REQUEST_TIME];
  $heap->{ping_by_seq}->{$from_seq}->[PBS_POSTBACK]->
    ( inet_ntoa($from_ip), $trip_time, $now
    );

  # clear the timer
  $kernel->delay($from_seq) if $heap->{onereply};
}

# Default's used to catch ping timeouts, which are named after the
# packed socket addresses being pinged.  We always send the timeout so
# the other session knows that a ping period has ended.

sub poco_ping_default {
  my ($kernel, $heap, $seq) = @_[KERNEL, HEAP, ARG0];

  # Record the receive time for possible use later.
  my $now = time();

  # Are we waiting for this sequence number?  We should be!
  if (exists $heap->{ping_by_seq}->{$seq}) {

    # Delete the ping information, but cache a copy for other work.
    my $ping_info = delete $heap->{ping_by_seq}->{$seq};

    # Post a timer tick back to the session.  This marks the end of
    # the request/response transaction.
    $ping_info->[PBS_POSTBACK]->( undef, undef, $now );

    # Stop mapping the session+address to this sequence number.
    delete( $heap->{addr_to_seq}->
            {$ping_info->[PBS_SESSION]}->{$ping_info->[PBS_ADDRESS]}
          );

    # Stop tracking the session if that was the last address.
    delete $heap->{addr_to_seq}->{$ping_info->[PBS_SESSION]}
      unless scalar(keys %{$heap->{addr_to_seq}->{$ping_info->[PBS_SESSION]}});

    # Close the socket if there are no sessions waiting for responses.
    unless (scalar keys %{$heap->{ping_by_seq}}) {
      DEBUG_SOCKET and warn "closing the raw icmp socket";
      $kernel->select_read( delete $heap->{socket_handle} );
    }

    return 1;
  }
  else {
    DEBUG and warn "this shouldn't technically be displayed";

    # Let unhandled signals pass through so we do not block SIGINT, etc.
    return 0;
  }
}

1;

__END__

=head1 NAME

POE::Component::Client::Ping - an ICMP ping client component

=head1 SYNOPSIS

  use POE qw(Component::Client::Ping);

  POE::Component::Client::Ping->spawn(
    Alias       => 'pingthing',   # defaults to 'pinger'
    Timeout     => 10,            # defaults to 1 second
    OneReply    => 1             # defaults to disabled
  );

  $kernel->post( 'pingthing', # Post the request to the 'pingthing'.
                 'ping',      # Ask it to 'ping' an address.
                 'pong',      # Have it post an answer to my 'pong' state.
                 $address,    # This is the address we want to ping.
                 $timeout,    # An optional timeout.  It overrides the default.
               );

  # This is the sub which is called when the session receives a 'pong'
  # event.  It handles responses from the Ping component.
  sub got_pong {
    my ($request_packet, $response_packet) = @_[ARG0, ARG1];

    my ($req_address, $req_timeout, $req_time)      = @{$request_packet};
    my ($resp_address, $roundtrip_time, $resp_time) = @{$response_packet};

    # The response address is defined if this is a response.
    # Otherwise, an undefined response address indicates that the
    # timeout period has ended.
    if (defined $resp_address) {
      printf( "ping to %-15.15s at %10d. pong from %-15.15s in %6.3f s\n",
              $req_address, $request_time,
              $resp_address, $roundtrip_time
            );
    }
    else {
      printf( "ping to %-15.15s is done.\n",
              $req_address
            );
    }
  }

  or

  use POE::Component::Client::Ping ':const';

  # post an array ref as the callback to get data back to you
  $kernel->post('pinger', 'ping', [ 'pong', @user_data ]);

  # use the REQ_USER_ARGS constant to get to your data
  sub got_pong {
      my ($request, $response) = @_;
      my @req_data = @{$reqest}[REQ_USER_ARGS..$#{$request}];
  }

=head1 DESCRIPTION

POE::Component::Client::Ping is non-blocking ICMP ping client session.
It lets several other sessions ping through it in parallel, and it
lets them continue doing other things while they wait for responses.
Ping client components are not proper objects.  Instead of being
created, as most objects are, they are "spawned" as separate sessions.
To avoid confusion (and hopefully not cause other confusion), they
must be spawned with a C<spawn> method, not created anew with a C<new>
one.

PoCo::Client::Ping's C<spawn> method takes a few named parameters:

=over 2

=item Alias => $session_alias

C<Alias> sets the name by which the session will be known.  If no
alias is given, the component defaults to "pinger".  The alias lets
several sessions interact with resolver components without keeping (or
even knowing) hard references to them.  It is possible to spawn
several Ping components with different names, but the author can find
no point in doing this.

=item Socket => $raw_socket

C<Socket> allows for the caller to open an existing raw socket rather
than letting PoCo::Client::Ping try and open one itself.  This can be
used to let startup code create the socket while running with enhanced
privileges, which are dropped.  People who don't want to audit POE for
security holes will find this useful since they won't need to run POE
as root.

=item Timeout => $ping_timeout

C<Timeout> specifies the default amount of time a Ping component will
wait for an ICMP echo reply.  C<$ping_timeout> contains a real number
indicating how many seconds to wait.  It's possible and meaningful to
set the timeout to a fractional number of seconds.

The default timeout is only used for ping requests that don't include
their own timeouts.

=item OneReply => 0|1

C<OneReply>, if set, tells the Ping component to clear the timeout upon
successful ping.  The Ping component will thus only post one reply back
to the calling module, upon either success or timeout, but not both.
This is useful if you are checking if one host is reachable or not as
you will only receive one reply.  By default, this setting is disabled,
meaning that you will get the ping timeout in addition to any ping 
replies.  This is useful if you are expecting multiple replies from one
ping, such as when pinging a subnet.
 
=back

Sessions communicate asynchronously with PoCo::Client::Ping.  They
post ping requests to it, and it posts responses back.

Requests are posted to the component's "ping" state.  They include the
name of a state to post responses back to, an address to ping, and an
optional amount of time to wait for responses.  The address may be a
numeric dotted quad, a packed inet_aton address, or a host name.  Host
names aren't recommended: they're looked up for every ping request,
and that's slow.  The optional timeout overrides the one set when
C<spawn> is called.

Ping responses come with two list references:

  my ($request_packet, $response_packet) = @_[ARG0, ARG1];

C<$request_packet> contains information about the original request:

  my ($request_address, $request_timeout, $request_time) = @{$request_packet};

=over 2

=item C<$request_address>

This is the original request address.  It matches the address posted
along with the original 'ping' request.

=item C<$request_timeout>

This is the original request timeout.  It's either the one passed with
the 'ping' request or the default timeout set with C<spawn>.

=item C<$request_time>

This is the time that the 'ping' event was received by the Ping
component.  It is a real number based on the current system's time()
epoch.

=back

C<$response_packet> contains information about the ICMP ping response.
There may be multiple responses for a single ping.

  my ($response_address, $roundtrip_time, $reply_time) = @{$response_packet};

=over 2

=item C<$response_address>

This is the address that responded to the ICMP echo request.  It may
be different than C<$request_address>, especially if the request
address was a subnet.

C<$response_address> will be undefined if C<$request_timeout> seconds
have elapsed.  This marks the end of responses for a given request.
Programs can assume that a request address will not send more
responses after this, and they may use the end-of-responses marker to
initiate another ping request.

=item C<$roundtrip_time>

This is the number of seconds that elapsed between the ICMP echo
request's transmission and its corresponding response's receipt.  It's
a real number.

=item C<$reply_time>

This is the time when the ICMP echo response was received.  It is a
real number based on the current system's time() epoch.

=back

If the :const tagset is imported the following constants will be exported:

REQ_ADDRESS, REQ_TIMEOUT, REQ_TIME, REQ_USER_ARGS, RES_ADDRESS, 
RES_ROUNDTRIP, RES_TIME

=head1 SEE ALSO

This component's ICMP ping code was lifted from Net::Ping, which is an
excellent module times when you don't mind pinging single hosts and
only knowing whether they're alive.

See POE, of course, which includes a lot of documentation about how
POE does things.

Also see the test program, t/01_ping.t, in the PoCo::Client::Ping
distribution.

=head1 BUGS

None currently known.

=head1 AUTHOR & COPYRIGHTS

POE::Component::Client::Ping is Copyright 1999-2000 by Rocco Caputo.
All rights are reserved.  POE::Component::Client::Ping is free
software; you may redistribute it and/or modify it under the same
terms as Perl itself.

=cut
