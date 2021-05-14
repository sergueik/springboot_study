#!/usr/bin/perl

use strict;

use Getopt::Long;

BEGIN {
    use constant RELEASE => 0;
    use constant HOME    => (
        do { $_ = $ENV{HOME}; /\/([^\/]+)$/ }
    );
    if (RELEASE) {

        # TODO: set extra lib path in RELEASE
    }
    else {
        unshift( @INC, `pwd` );
        unshift( @INC, '.' );
        unshift( @INC, '/web/cgi-bin' );
    }
}
use YAML::Tiny;
use JSON::PP;
my $data = [ 'apple', 'pear', 'orange', 'plum' ];
our $debug   = 0;
our $json_pp = JSON::PP->new->ascii->pretty->allow_nonref;
print "Content-Type: application/json\n\n", $json_pp->encode($data);
