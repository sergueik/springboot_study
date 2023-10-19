#!/usr/bin/perl

use strict;

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
        unshift( @INC, SCRIPT_DIR );
        unshift( @INC, `pwd` );
    }
}
use JSON::PP;
use LWP::Simple;
use Data::Dumper;
use Getopt::Long;

use vars qw($url $response $debug $outputfile $json_pp);

$json_pp = JSON::PP->new->ascii->pretty->allow_nonref;
$outputfile = undef;
$debug    = 0;


# $url = "http://$HOST:80/cgi-bin/file_hash_status.cgi?inputfile=example_config.json&hash=9f8377db38593544a5e994006fe4e9e4";
GetOptions(
    'debug'    => \$debug,
    'url=s'    => \$url,
    'output=s' => \$outputfile,
);

if ($debug) {
    print "url = $url\n";
    print "outputfile = $outputfile\n";
    print "debug = $debug\n";
}
$response = mirror($url, $outputfile);

print STDERR 'Status: ', $response, $/;
