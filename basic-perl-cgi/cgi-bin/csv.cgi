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

use utf8;
use CGI::Tiny;
use Text::CSV_PP;
use JSON::PP;
use Data::Dumper;
use File::Temp qw/ tempdir tempfile /;

use vars qw|$cgi $csv|;

cgi {

    $cgi = $_;
    if ( $cgi->method ne 'POST' ) {
        $cgi->set_response_status(405);    # METHOD_NOT_ALLOWED
        exit;
    }

    my $tmpdir = tempdir( CLEANUP => 1 );
    $| = 1;
    my ( $fh, $tmpfile ) = tempfile( DIR => $tmpdir );

    print STDERR "tmpfile: ${tmpfile}\n";
    # NOTE: should not print $fh $cgi->body(); 
    my $body = $cgi->body();
    # print STDERR Dumper(  $body ), $/;
    print $fh $body; 

    close($fh);
    $csv = Text::CSV_PP->new( { binary => 1 } );
    my $csv_data = [];
    open $fh, '<', $tmpfile or die "tmpfile: ${tmpfile} error: $!";
    my @column_names = @{ $csv->getline($fh) };

    $csv->column_names(@column_names);
    while ( my $row = $csv->getline_hr($fh) ) {
        push @$csv_data, $row;
    }
    my $json_pp = JSON::PP->new->ascii->pretty->allow_nonref;
    $cgi->set_response_type('application/json');
    $cgi->render( html => $json_pp->encode($csv_data) );
}
