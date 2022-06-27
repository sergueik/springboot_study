#!/usr/bin/perl

use strict;

use Getopt::Long;

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
use Data::Dumper;
use strict;
use warnings;
use utf8;

use vars qw($query $filename $loadtype $method $new );
cgi {
    $query = $_;
    $query->{multipart_form_options} = { parse_as_files => 0 };
    $loadtype = $query->param('type');
    $new      = $query->param('new');
    $method   = $query->method;
    if ( $method eq 'POST' ) {

        # $query->_body_multipart();
        if ( $loadtype =~ /send/ ) {
            my $upload       = $query->upload('data');
            $filename = $upload->{filename} || 'unknown file';
            my $file_content = $upload->{content};
            print STDERR "upload content: ", $file_content, $/;

            if ($new) {
                {
                    print STDERR "do ingestion of $file_content", $/;

                    # TODO: implement original processing:
                    # print to some local file handle
                    # do print to console for demo
                    print STDERR $file_content;
                }
            }
            else {
                # does not work right with CGI::Tiny
                my $upload_filehandle = $upload;

                # Not a GLOB reference ?
                # https://metacpan.org/pod/File::Temp
                # https://www.perlmonks.org/?node_id=708846
                while (<$upload_filehandle>) {

                    # original processing:
                    # print to some local file handle
                    # do print to console for demo
                    print STDERR;
                }
            }
        }

    }
    $query->set_response_type('text/html');

    $query->render( html => <<EOF);
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

