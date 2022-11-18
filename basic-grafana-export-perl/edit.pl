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
if ( !$inputfile ) {
    help();
}
our $json_pp = JSON::PP->new->ascii->pretty->allow_nonref;
open my $fh, '<', $inputfile or die "Can't open file $!";
my $content = do { local $/; <$fh> };
my $root = eval { $json_pp->decode($content) };
if ($dump) {
    print STDERR 'Grafana Dashboard JSON:' . $/;
    print STDERR Dumper($root);
}
my $panels        = $root->{'panels'};
my $panel_indexes = 0 .. $#$panels;

foreach my $cnt1 ($panel_indexes) {
    my $panel = $panels->[$cnt1];
    print STDERR 'type: ', $panel->{'type'}, $/;

    my $targets        = $panel->{'targets'};
    my $target_indexes = 0 .. $#$targets;

    foreach my $cnt2 ($target_indexes) {
        my $target = $targets->[$cnt2];
        print STDERR 'target: ', $/, 'expr:', $target->{'expr'}, $/;
        my $datasource = $target->{'datasource'};
        print STDERR 'datasource: ', $/, 'type:', $datasource->{'type'}, $/;
    }

}

# NOTE: to update the JSON, need to write the properties of $root
# which would lead to long chains of key and index references

my $panel_index  = 0;
my $target_index = 0;

# modify values through valid element reference.
# This is to avoid using the template
# which is another valid  option to consider
$root->{'panels'}->[$panel_index]->{'type'} = 'modified';
$root->{'panels'}->[$panel_index]->{'targets'}->[$target_index]->{'datasource'}
  ->{'type'} = 'new datasource type';
print "Result:\n", $json_pp->encode($root);

sub help() {
    print STDERR <<EOF; exit 0;
Usage: edit.pl [--debug] [--dump] --input <JSON>
EOF
}
1;
__END__

