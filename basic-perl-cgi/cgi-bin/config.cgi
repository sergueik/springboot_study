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
	chomp( my $script_dir = `dirname $0`);
        unshift( @INC, $script_dir );
        unshift( @INC, `pwd` );
    }
}

use YAML::Tiny;
use JSON::PP;
my $data = {
          'sergueik53' => {
                            'PORTS' => [
                                         22,
                                         443,
                                         3306
                                       ]
                          },
          'sergueik71' => {
                            'PORTS' => [
                                         5432
                                       ]
                          },
          'sergueik119' => {}
        };

our $json_pp = JSON::PP->new->ascii->pretty->allow_nonref;
print "Content-Type: application/json\n\n", $json_pp->encode($data);

