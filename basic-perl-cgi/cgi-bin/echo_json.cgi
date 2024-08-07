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

use vars qw($cgi $method $remote_addr $body $data $headers);
our $json_pp = JSON::PP->new->ascii->pretty->allow_nonref;
cgi {
    $cgi  = $_;
    # Override API being blocked by CORS policy:  
    # Request header field referrer-policy, content-type is not allowed by Access-Control-Allow-Headers in preflight response
    # NOTE: can not add multiple headers in one call 
    # NOTE: Apache may be already sending the header
    # Access-Control-Allow-Origin: * 
    # uncommenting the below leads to browser error
    # The 'Access-Control-Allow-Origin' header contains multiple values '*, *', but only one is allowed.
    # $cgi->add_response_header('Access-Control-Allow-Origin' => '*');
    # $cgi->add_response_header('Access-Control-Allow-Headers' => '*');

    $headers = $cgi->headers;
    $method = $cgi->method;
    if ( $method eq 'POST' ) {

        $remote_addr = $cgi->remote_addr;
        $body        = $cgi->body;

        # NOTE: the body_json does not appear to work
        # https://metacpan.org/pod/CGI::Tiny#body_json
        # $data = $cgi->body_json;
        $data = $json_pp->decode($body);
        $data->{'remote_addr'} = $remote_addr;
        $data->{'referer'} = $headers->{Referer};
        print STDERR Dumper($data), $/;
        $cgi->set_response_type('application/json');

        $cgi->render( html => $json_pp->encode($data) );

    }
    else {
        $cgi->set_response_type('text/html');

        $cgi->render( html => <<EOF);
<!DOCTYPE html>
<html>
<head>
<meta http-equiv="Content-Type" content="text/html; charset=utf-8" />
<title>Thanks!</title>
</head>
<body>
<p>Use POST for uploading data</p>
</body>
</html>

EOF
    }
}
