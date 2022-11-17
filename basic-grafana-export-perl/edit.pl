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
use JSON::PP;
use Data::Dumper;

my $inputfile  = undef;
my $outputfile = undef;
my $debug      = 0;
my $dump       = 0;

GetOptions(
    'input=s'  => \$inputfile,
    'output=s' => \$outputfile,
    'debug'    => \$debug,
    'dump'     => \$dump
);

our $json_pp = JSON::PP->new->ascii->pretty->allow_nonref;
open my $fh, '<', $inputfile or die "Can't open file $!";
my $content = do { local $/; <$fh> };
my $root = eval { $json_pp->decode($content) };
print STDERR 'Decoded Grafana Dashboard JSON:' . $/ if $debug;
print STDERR Dumper($root) if $debug;
my $panel_index = 0;
my $panel = $root->{'panels'}->[$panel_index]; 
my $type = $panel->{'type'};
print STDERR "panel: ${type}", $/;
my $target_index = 0;
my $target = $panel->{'targets'}->[$target_index];
my $expr = $target->{'expr'};
my $datasource = $target->{'datasource'};
my $datasource_type = $datasource->{'type'};
print STDERR "detail: expr: \"${expr}\", datasource: \"${datasource_type}\"", $/;
# modify values through valid element reference. 
# This is to avoid using the template
# which is another valid  option to consider
$root->{'panels'}->[$panel_index]->{'type'} = 'modified';
$root->{'panels'}->[$panel_index]->{'targets'}->[$target_index]->{'datasource'}->{'type'} = 'new datasource type';
print "Result:\n", $json_pp->encode($root);
