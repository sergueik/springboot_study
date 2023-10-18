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

use Data::Dumper;
use HTTP::Tiny;
use Getopt::Long;

use vars qw($url $response $page $data $debug);
our $json_pp = JSON::PP->new->ascii->pretty->allow_nonref;
my $outputfile = undef;

$url = 'http://192.168.99.100:9090/cgi-bin/statuscode.cgi?code=304';
$url =
'http://192.168.99.100:9090/cgi-bin/file_hash.cgi?inputfile=example_config.json&hash=9f8377db38593544a5e994006fe4e9e4';

# $url = 'http://192.168.99.100:9090/cgi-bin/file_hash_status.cgi?inputfile=example_config.json&hash=9f8377db38593544a5e994006fe4e9e4';
$debug    = 0;
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
use LWP::Simple;
my $response = mirror($url, $outputfile);

print STDERR Dumper($response);