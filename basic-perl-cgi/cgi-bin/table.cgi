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

sub result {
    my ( $size, $max ) = @_;
    my @indexes = ();
    for ( 0 .. $size ) {
        push @indexes, chr( 97 + int rand(25) );
    }
    join '', @indexes;
}
my $results = $data->{results};
foreach ( 0 .. $#$results ) {
    my $data   = $results->[$_];
    my $server = &result(8);
    foreach my $column ( keys %$data ) {
        $data->{$column} =~ s|row \d|$server|g;
    }
}

our $json_pp = JSON::PP->new->ascii->pretty->allow_nonref;
print "Content-Type: application/json\n\n", $json_pp->encode($data);

