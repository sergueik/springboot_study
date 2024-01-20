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
use Data::Dumper;
use Text::CSV_PP;
use JSON::PP;
use File::Temp qw/ tempdir tempfile /;
use strict;
use warnings;
use utf8;

use vars qw|$cgi $filename $fh $csv|;

cgi {
    $cgi = $_;
    $cgi->{multipart_form_options} = { parse_as_files => 0 };
    if ( $cgi->method ne 'POST' ) {
        $cgi->set_response_status(405);    # METHOD_NOT_ALLOWED
        exit;
    }

    # $cgi->_body_multipart();
    if ( $cgi->param('type') !~ /send/ ) {
        $cgi->set_response_status(400);    # BAD_REQUEST
        exit;
    }
    my $data = $cgi->upload('data');
    $filename = $data->{filename} || 'unknown';
    print STDERR 'filename: ', $filename, $/;
    my $content = $data->{content};
    $| = 1;
    my $tmpfile = '/tmp/' . $filename;
    open $fh, '>' , $tmpfile or die "tmpfile ${tmpfile} for writing error: $!";
    print $fh $content;

    print STDERR 'content: ', $/;
    foreach my $line ( split /\r?\n/, $content ) {
        print STDERR $line, $/;
    }

    close($fh);
    my $csv = Text::CSV_PP->new( { binary => 1 } );
    my $csv_data = [];
    open $fh, '<', $tmpfile or die "tmpfile ${tmpfile} for reading error: $!";
    my @column_names = @{ $csv->getline($fh) };

    # print STDERR Dumper(\@column_names);
    $csv->column_names(@column_names);
    while ( my $row = $csv->getline_hr($fh) ) {
        push @$csv_data, $row;
    }
    my $json_pp = JSON::PP->new->ascii->pretty->allow_nonref;
    $cgi->set_response_type('application/json');
    $cgi->render( html => $json_pp->encode($csv_data) );

}
