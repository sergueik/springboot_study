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

use vars qw($url $response $page $data $DEBUG);
our $json_pp = JSON::PP->new->ascii->pretty->allow_nonref;

$url = 'http://192.168.99.100:9090/cgi-bin/statuscode.cgi?code=304';
$url =
'http://192.168.99.100:9090/cgi-bin/file_hash.cgi?inputfile=example_config.json&hash=9f8377db38593544a5e994006fe4e9e4';

# $url = 'http://192.168.99.100:9090/cgi-bin/file_hash_status.cgi?inputfile=example_config.json&hash=9f8377db38593544a5e994006fe4e9e4';
GetOptions(
    'debug' => \$DEBUG,
    'url=s' => \$url,

);

$DEBUG    = 0;
$response = HTTP::Tiny->new->get($url);

if ( !$response->{success} ) {
    print STDERR Dumper($response), $/ if $DEBUG;
    print 'ERROR: ', $response->{reason}, $/;
}
else {

    # while (my ($k, $v) = each %{$response->{headers}}) {
    #    for (ref $v eq 'ARRAY' ? @$v : $v) {
    #        print "$k: $_\n";
    #    }
    #}
    $page = $response->{content};
    if ( length $page ) {
        $data = $json_pp->decode($page);
        if ($DEBUG) {
            print STDERR Dumper($data);
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
