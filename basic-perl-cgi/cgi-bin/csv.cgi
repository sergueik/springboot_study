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
use utf8; 
use CGI::Tiny;
use Getopt::Long;
use Text::CSV_PP;
use JSON::PP;
use Data::Dumper;

# my $file = $ARGV[0];
my $csv = Text::CSV_PP->new( { binary => 1 } );

use vars qw($query $body );

cgi {

    $query = $_;
    $query->set_response_type('text/html');
    $body = $query->body();
    print STDERR Dumper(  $body ), $/;
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
<p>Thanks for uploading data:</p>
$body
</body>
</html>

EOF

}
