#!/usr/bin/perl
# $Id$

use ExtUtils::MakeMaker;

WriteMakefile
  ( NAME         => 'POE::Component::Client::Ping',
    AUTHOR       => 'Rocco Caputo <troc+poe@netrus.net>',
    ABSTRACT     => 'POE component for non-blocking/concurrent ICMP ping.',
    VERSION_FROM => 'POE/Component/Client/Ping.pm',
    dist         => { 'COMPRESS' => 'gzip -9f',
                      'SUFFIX'   => 'gz',
                    },
    PREREQ_PM    => { POE         => 0.11,
                      Time::HiRes => 1.20,
                    },
  );
