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
# NOTE: without initializing, use leads to
# cgi exited without rendering a response
# Status: 500 Internal Server Error
# Date: Fri, 12 Jan 2024 21:43:49 GMT
# Content-Type: text/plain
# Content-Length: 25
 
# use CGI::Tiny;
use Getopt::Long;
use Text::CSV_PP;
use JSON::PP;
use Data::Dumper;

my $file = $ARGV[0];
my $csv = Text::CSV_PP->new( { binary => 1 } );
open my $fh, '<', $file or die "$file: $!";
my $debug = 0;
my $content;
my $error;
my $json_pp = JSON::PP->new->ascii->pretty->allow_nonref;
my $data = [];

# TODO: explore reading the data into the hash wih keys
while ( my $row = $csv->getline($fh) ) {
   push @$data, $row;
}
#
close($fh);
open my $fh, '<', $file or die "$file: $!";
my @cols = @{$csv->getline ($fh)};
$csv->column_names (@cols);
while (my $row = $csv->getline_hr ($fh)) {
    push @$data, $row;
    }
print "Result:\n", $json_pp->encode($data);
if ($debug){
    print Dumper($data);
}
