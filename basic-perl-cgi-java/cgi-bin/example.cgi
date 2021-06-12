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

use YAML::Tiny;
use JSON::PP;
my $data = { 'fruit' => [ 'apple', 'pear', 'orange', 'plum' ] };
our $debug   = 0;
our $json_pp = JSON::PP->new->ascii->pretty->allow_nonref;
my $options = $ARGV[0];
print "Content-Type: application/json\n\n" unless $options =~ /\Qno-headers\E/;
print $json_pp->encode($data);
