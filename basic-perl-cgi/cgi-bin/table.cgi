#!/usr/bin/perl

use strict;

use Getopt::Long;

BEGIN
{
    use constant RELEASE => 0;
    use constant HOME => (
                          do { $_ = $ENV{HOME}; /\/([^\/]+)$/ }
                         );
    if (RELEASE)
    {
        # TODO: set extra lib path in RELEASE
    }
    else
    {
        unshift(@INC, `pwd`);
        unshift(@INC, '.');
        unshift(@INC, '/web/cgi-bin');
    }
}
use YAML::Tiny;
use JSON::PP;
my $data = {
            'results' => [
                          {
                           'column1' => 'row 1 column 1',
                           'column2' => 'row 1 column 2',
                           'column3' => 'row 1 column 3'
                          },
                          {
                           'column1' => 'row 2 column 1',
                           'column2' => 'row 2 column 2',
                           'column3' => 'row 2 column 3'
                          },
                          {
                           'column1' => 'row 3 column 1',
                           'column2' => 'row 3 column 2',
                           'column3' => 'row 3 column 3'
                          }
                         ]
           };
our $json_pp = JSON::PP->new->ascii->pretty->allow_nonref;
print "Content-Type: application/json\n\n", $json_pp->encode($data);

