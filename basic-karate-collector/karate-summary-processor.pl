#!/usr/bin/perl

use strict;

use Getopt::Long;

# NOTE: JSON is available in git bash Perl but not on
# # a generic stripped Linux vanilla box (solvable though cpan)
# for pure Perl JSON  use JSON::Tiny
# or JSON:PP
# https://metacpan.org/release/JSON-PP/source/lib/JSON/PP.pm
BEGIN {
    use constant RELEASE => 0;
    use constant HOME    => (
        do { $_ = $ENV{HOME}; /\/([^\/]+)$/ }
    );
    use constant SCRIPT_DIR => (
        do { my $s = `dirname $0`; chomp $s; $s }
    );
    if (RELEASE) {

        # TODO: set extra lib path in RELEASE
    }
    else {
        unshift( @INC, `pwd` );
        unshift( @INC, SCRIPT_DIR );
    }
}

use JSON::PP;
use Data::Dumper;

# NOTE: do not expect  Data::Dumper to be available
# e.g. in git bash Perl install
# use Data::Dump;

my $inputfile  = undef;
my $outputfile = undef;
my $debug      = 0;
my $dump       = 0;

GetOptions(
    'input=s'  => \$inputfile,
    'output=s' => \$outputfile,
    'debug'    => \$debug,
    'dump'     => \$dump
);
my $content;
my $error;

if ($inputfile) {
    $content = '';
    open( FH, '<', $inputfile ) or die $!;
    while (<FH>) {
        $content .= $_;
    }
    close(FH);
}

if ($debug) {
    print STDERR Dumper($content);
}

our $json_pp = JSON::PP->new->ascii->pretty->allow_nonref;
local $@;
my $data = eval {
    if ($debug) {
        print STDERR Dumper($content);
    }
    my $result = $json_pp->decode($content);
    return $result;
};
$error = $@;

if ($data) {
    $error = $data->{error};
}

if ( !$error ) {
    print STDERR Dumper($data) if $debug;
    print $data->{'featureSummary'}->[0]->{'failedCount'};
}
