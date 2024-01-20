#!/usr/bin/perl

use strict;
use warnings;

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
use CGI::Tiny;
use Getopt::Long;
use Data::Dumper;
use JSON::PP;
use Time::HiRes qw( gettimeofday);
use Sys::Hostname;
use utf8;

use vars qw($query $method $remote_addr $body $data $headers $hostname);
our $json_pp = JSON::PP->new->ascii->pretty->allow_nonref;
cgi {
    $query  = $_;
    # Override API being blocked by CORS policy:  
    # Request header field referrer-policy, content-type is not allowed by Access-Control-Allow-Headers in preflight response
    # NOTE: can not add multiple headers in one call 
    $query = $query->add_response_header('Access-Control-Allow-Origin' => '*');
    $query = $query->add_response_header('Access-Control-Allow-Headers' => '*');

    $headers = $query->headers;
    $method = $query->method;
#    if ( $method eq 'GET' ) {
        $hostname = $query->query_param('hostname') || 'hostname';  
        $remote_addr = $query->remote_addr;
 
        $body = "{\"hostname\": \"$hostname\"}";

        # NOTE: the body_json does not appear to work
        # https://metacpan.org/pod/CGI::Tiny#body_json
        # $data = $query->body_json;
        $data = $json_pp->decode($body);
        $data->{'status'} = 'OK';
        $data->{'environment'} = 'PRODUCTION';
        print STDERR Dumper($data), $/;
        $query->set_response_type('application/json');

        $query->render( html => $json_pp->encode($data) );
#    }
}
