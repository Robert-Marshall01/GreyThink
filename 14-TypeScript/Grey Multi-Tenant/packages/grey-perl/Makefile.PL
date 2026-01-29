use strict;
use warnings;
use ExtUtils::MakeMaker;

WriteMakefile(
    NAME             => 'Grey::SDK',
    VERSION_FROM     => 'lib/Grey/SDK.pm',
    ABSTRACT         => 'Grey Multi-Tenant SDK for Perl',
    AUTHOR           => 'Grey Team',
    LICENSE          => 'mit',
    MIN_PERL_VERSION => '5.020',
    
    PREREQ_PM => {
        'LWP::UserAgent'     => '6.0',
        'HTTP::Request'      => '6.0',
        'JSON'               => '4.0',
        'URI'                => '5.0',
        'Try::Tiny'          => '0.30',
    },
    
    TEST_REQUIRES => {
        'Test::More'         => '1.0',
        'Test::Exception'    => '0.43',
        'Test::MockObject'   => '1.20',
    },
    
    META_MERGE => {
        'meta-spec' => { version => 2 },
        resources   => {
            repository => {
                type => 'git',
                url  => 'https://github.com/grey/grey-perl.git',
                web  => 'https://github.com/grey/grey-perl',
            },
            bugtracker => {
                web => 'https://github.com/grey/grey-perl/issues',
            },
        },
    },
    
    dist  => { COMPRESS => 'gzip -9f', SUFFIX => 'gz', },
    clean => { FILES => 'Grey-SDK-*' },
);
