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
my $data = {
    'results' => [
        { 'text' => 'Lorem ipsum dolor sit amet' },
        { 'text' => 'consectetur adipiscing elit' },
        { 'text' => 'sed do eiusmod tempor incididunt' },
        { 'text' => 'ut labore et dolore magna aliqua' },
        { 'text' => 'ut enim ad minim veniam' },
        { 'text' => 'quis nostrud exercitation ullamco' },
        { 'text' => 'laboris nisi ut aliquip ex ea commodo consequat' },
        { 'text' => 'Duis aute irure dolor in reprehenderit in' },
        {
            'text' =>
              'voluptate velit esse cillum dolore eu fugiat nulla pariatur'
        },
        { 'text' => 'excepteur sint occaecat cupidatat non proident' }
    ]
};

sub result {
    my ( $size, $max ) = @_;
    my @indexes = ();
    for ( 0 .. $size ) {
        push @indexes, int rand($max);
    }
    @indexes;
}
our $debug   = 0;
our $json_pp = JSON::PP->new->ascii->pretty->allow_nonref;
our @index   = result( 4, 10 );

our @random_lines = map { $data->{'results'}->[$_]->{'text'} } @index;
if ($debug) {
    our $json_pp = JSON::PP->new->ascii->pretty->allow_nonref;
    print STDERR "Random index: ", $json_pp->encode( \@index ), "\n";
}
our $response = { 'results' => [] };
map { push @{ $response->{'results'} }, { 'text' => $_ } } @random_lines;
print "Content-Type: application/json\n\n", $json_pp->encode($response);
