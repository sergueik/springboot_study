#!/usr/bin/perl

use strict;

BEGIN {
    use constant RELEASE => 0;
    use constant HOME    => (
        do { $_ = $ENV{HOME}; /\/([^\/]+)$/ }
    );
    use constant SCRIPT_DIR => (
        do { my $s = qx|dirname $0|; chomp $s; $s }
    );
    if (RELEASE) {

        # TODO: set extra lib path in RELEASE
    }
    else {
        unshift( @INC, SCRIPT_DIR );
        unshift( @INC, qx|pwd| );
    }
}
use CGI::Tiny;
use Getopt::Long;
use Data::Dumper;
use JSON::PP;
use strict;
use warnings;
use utf8;

use vars qw($cgi $filename $loadtype );
use vars qw($content $data $line $value );

cgi {
    $cgi = $_;
    $cgi->{multipart_form_options} = { parse_as_files => 0 };
    if ( $cgi->method ne 'POST' ) {
        $cgi->set_response_status(405); # METHOD_NOT_ALLOWED
        exit;
    }

    # $cgi->_body_multipart();
    if ( $cgi->param('type') !~ /send/ ) {
        $cgi->set_response_status(400); # BAD_REQUEST
        exit;
    }
    $data = $cgi->upload('data');
    $filename = $data->{filename} || 'unknown file';
    print STDERR 'filename: ', $filename, $/;
    my $content = $data->{content};
    print STDERR 'payload: ', $/;
    foreach $line ( split /\r?\n/, $content ) {
        print STDERR $line, $/;
    }

    $cgi->set_response_type('text/html');

    $cgi->render( html => <<EOF);
<!DOCTYPE html>
<html>
<head>
<meta http-equiv="Content-Type" content="text/html; charset=utf-8" />
<title>Thanks!</title>
<style type="text/css">
img {border: none;}
</style>
</head>
<body>
<p>Thanks for uploading data</p>
<p><img src="/upload/$filename" alt="data" /></p>
</body>
</html>

EOF
}
