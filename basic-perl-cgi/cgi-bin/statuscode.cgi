#!/usr/bin/perl

use strict;
use warnings;
use utf8;


BEGIN {
    use constant RELEASE => 0;
    use constant HOME    => (

        # NOTE: logged Use of uninitialized value $_ in pattern match (m//)
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

use vars qw($cgi $method $remote_addr $body $data $code %nodata);
our $json_pp = JSON::PP->new->ascii->pretty->allow_nonref;
%nodata = ( 'status', 'error' );
cgi {
    $cgi    = $_;
    $method = $cgi->method;
    print STDERR "method=$method", $/;
    $cgi->set_error_handler(
        sub {
            my ( $cgi, $error, $rendered ) = @_;
            print STDERR "in error handler: $error", $/;
            $nodata{'result'} = $error;
            $cgi->set_response_status(406)
              ->render( html => $json_pp->encode( \%nodata ) );
        }
    );

    # NOTE: curl -I is sending HEAD and -X GET does not override that
    if ( $method eq 'GET' || $method eq 'HEAD' ) {
        $code = $cgi->query_param('code');
        print STDERR "code=$code", $/;
        $remote_addr = $cgi->remote_addr;
        $data        = {};
        if ( $code eq '200' || !$code ) {
            $data->{'status'}      = 'OK';
            $data->{'remote_addr'} = $remote_addr;
            $data->{'result'}      = ();
            $cgi->set_response_type('application/json');
            $cgi->set_response_status(200);
            $cgi->render( html => $json_pp->encode($data) );
        }
        else {
            print STDERR "returning HTTP Status $code", $/;
            print STDERR Dumper( \%nodata ), $/;

            $cgi->set_response_status($code)
              ->render( html => $json_pp->encode( \%nodata ) );

            # https://www.softwaretestinghelp.com/rest-api-response-codes/:
            # 304  Not Modified
            # 200  OK
            # 208  Already Reported
            # 404  Not Found
            # 208  Already Reported
            # 406 - Not Acceptable
        }

    }
    else {
        print STDERR 'Unsupported method', $/;
        $cgi->set_response_status(405)
          ->render( html => $json_pp->encode( \%nodata ) );
    }
}

