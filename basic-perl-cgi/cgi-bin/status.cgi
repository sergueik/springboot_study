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

# NOTE: the encoding pragma is no longer supported
# use encoding 'utf8';
use POSIX qw (locale_h);
use locale;

our $json_pp = JSON::PP->new->ascii->pretty->allow_nonref;
my $buffer;

# https://stackoverflow.com/questions/2224158/how-can-i-send-post-and-get-data-to-a-perl-cgi-script-via-the-command-line
# read payload into $buffer
read( STDIN, $buffer, $ENV{'CONTENT_LENGTH'} );
my $data = $json_pp->decode($buffer);

# TODO: modify $data
$data->{'remote_addr'} = $ENV{'REMOTE_ADDR'};
print "Content-Type: application/json\r\n\r\n", $json_pp->encode($data);

1;

