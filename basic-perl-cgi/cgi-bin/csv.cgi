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
use File::Temp qw/ tempdir tempfile /;

use vars qw($query $body );

cgi {

    $query = $_;
    $query->set_response_type('application/json');
    $body = $query->body();

    my $dir = tempdir( CLEANUP => 0 );
    my ( $fh, $filename ) = tempfile( DIR => $dir );

    # print STDERR "filename: ${filename}\n";
    my $csv = Text::CSV_PP->new( { binary => 1 } );
    print $fh $body;

    # print STDERR Dumper(  $body ), $/;
    $| = 1;

    # flush($fh);
    close($fh);
    my $json_pp = JSON::PP->new->ascii->pretty->allow_nonref;
    my $data    = [];
    open $fh, '<', $filename or die "filename: ${filename} error: $!";
    my @cols = @{ $csv->getline($fh) };

    # print STDERR Dumper(\@cols);
    $csv->column_names(@cols);
    while ( my $row = $csv->getline_hr($fh) ) {
        push @$data, $row;
    }
    $query->render( html => $json_pp->encode($data) );
}
