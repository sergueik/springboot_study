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
use CGI::Tiny::Multipart
  qw(extract_multipart_boundary parse_multipart_form_data);
use Text::CSV_PP;
use JSON::PP;
use Data::Dumper;
use File::Temp qw/ tempdir tempfile /;

use vars qw|$cgi $payload $csv_data|;

cgi {

    $cgi = $_;
    if ( $cgi->method ne 'POST' ) {
        $cgi->set_response_status(405);    # METHOD_NOT_ALLOWED
        exit;
    }
    $payload = '';

    # detect multipart header
    my $content_type = $cgi->content_type;
    # print STDERR 'Content-Type: ' . $content_type . $/;
    if (extract_multipart_boundary($content_type)) {
        my $pairs = $cgi->uploads;
        # print STDERR 'uploads ' . Dumper($pairs) . $/;
        # NOTE: limit to processing only one uploaded file
        my $name = $pairs->[0]->[0];
        my $data = $cgi->upload($name);
        my $filename = $data->{filename} || 'unknown';
        # print STDERR 'filename: ' . $filename . $/;
        my $fh = $data->{file};

        while (<$fh>) {
            $payload .= $_;
            # my $line = $_;
            # chomp $line;
            # print STDERR $line . $/;
        }
    }
    else {
        # NOTE: should not attmept do in single operation: print $fh $cgi->body();
        $payload = $cgi->body();
    }
    # print STDERR 'payload: ' . $payload . $/;
    my $tmpdir = tempdir( CLEANUP => 1 );
    $| = 1;
    my ( $fh, $tmpfile ) = tempfile( DIR => $tmpdir );
    # print STDERR 'tmpfile: ' . $tmpfile . $/;
    print $fh $payload;
    close($fh);
    $csv_data = [];
    my $csv = Text::CSV_PP->new( { binary => 1 } );
    open $fh, '<', $tmpfile or die "tmpfile: ${tmpfile} error: $!";
    my @column_names = @{ $csv->getline($fh) };

    $csv->column_names(@column_names);
    while ( my $row = $csv->getline_hr($fh) ) {
        push @$csv_data, $row;
    }
    # print STDERR 'csv data: ' . Dumper($csv_data) . $/;
    my $json_pp = JSON::PP->new->ascii->pretty->allow_nonref;
    my $json_data = $json_pp->encode($csv_data);
    # print STDERR 'json data: ' . Dumper($json_data) . $/;
    $cgi->set_response_type('application/json');
    $cgi->render( html => $json_data );
}
