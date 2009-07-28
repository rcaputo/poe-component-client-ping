#!/usr/bin/perl
# $Id$

use ExtUtils::MakeMaker;

# Touch CHANGES so it exists.
open(CHANGES, ">>CHANGES") and close CHANGES;

WriteMakefile(
  NAME         => 'POE::Component::Client::Ping',
  AUTHOR       => 'Rocco Caputo <rcaputo@cpan.org>',
  ABSTRACT     => 'POE component for non-blocking/concurrent ICMP ping.',
  LICENSE      => 'perl',
  VERSION_FROM => 'Ping.pm',
  META_ADD     => {
    resources  => {
      license    => 'http://dev.perl.org/licenses/',
      repository => 'http://thirdlobe.com/svn/poco-client-ping/trunk'
    },
  },
  PM           => { 'Ping.pm'   => '$(INST_LIBDIR)/Ping.pm' },
  PREREQ_PM    => {
    'POE'         => 1.007,
    'Time::HiRes' => 1.20,
  },
  dist         => {
    COMPRESS   => 'gzip -9f',
    SUFFIX     => 'gz',
    PREOP      => (
      'svn-log.perl --repo https://thirdlobe.com/svn/poco-client-ping| ' .
      'tee ./$(DISTNAME)-$(VERSION)/CHANGES > ./CHANGES'
    ),
  },
);
