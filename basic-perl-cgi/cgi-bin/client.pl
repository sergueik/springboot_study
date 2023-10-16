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
# NOTE: with HTTP::Tiny mirror method
# includes the timestamp of the $outputfile in the request
# If-Modified-Since header
# Alternatively caller can provide If-Modified-Since header
#
# $response = HTTP::Tiny->new->mirror($url,$outputfile, {});
# and considers status code 304 to be a success,
# # but does not attempt to create local file if received a 304
# the more traditional is to use a get method
$response = HTTP::Tiny->new->get($url);
if ( !$response->{success} ) {
    print STDERR Dumper($response), $/ if $debug;
    print 'ERROR: ', $response->{status}, $/;
    print 'REASON ', $response->{reason}, $/;
}
else {
    # https://metacpan.org/pod/HTTP::Tiny
    # while (my ($k, $v) = each %{$response->{headers}}) {
    #    for (ref $v eq 'ARRAY' ? @$v : $v) {
    #        print "$k: $_\n";
    #    }
    #}
    print STDERR 'Success:', $/;
    $page = $response->{content};
    if ( length $page ) {
	print STDERR  'Page: ' , $/;
        $data = $json_pp->decode($page);
        if ($debug) {
            print STDERR Dumper($data);
        }
        if ($outputfile) {

            # Initialize the configuration into the file
            open( DATA, '>', $outputfile ) or die $!;
            print DATA $page;
            flush DATA;
            close(DATA);
        }
        if ( exists $data->{status} ) {
            if ( $data->{status} =~ /error/ ) {
                print 'ERROR: ', $data->{result}, $/;
            }
        }
        else {
            print $page;
        }
    }
}
