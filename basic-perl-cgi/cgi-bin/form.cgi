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
# NOTE: the encoding pragma is no longer supported
# use encoding 'utf8';
use POSIX qw (locale_h);
use locale;

our %FORM;
our $json_pp = JSON::PP->new->ascii->pretty->allow_nonref;
my $buffer;
# https://stackoverflow.com/questions/2224158/how-can-i-send-post-and-get-data-to-a-perl-cgi-script-via-the-command-line
# https://habr.com/ru/post/163439/ encodings (in Russian)
#
read( STDIN, $buffer, $ENV{'CONTENT_LENGTH'} );
my @pairs = split( /&/, $buffer );
foreach my $pair (@pairs) {
    my ( $name, $value ) = split( /=/, $pair );
    $value =~ tr/+/ /;
    $value =~ s/%([a-fA-F0-9] [a-fA-F0-9])/pack("C", hex($1))/eg;
    $value =~ s/~!/ ~!/g;
    $FORM{$name} = $value;
}
# my $need_locale='ru_RU.UTF-8';
# my $locale = setlocale(LC_ALL, $need_locale) || die "no locale $need_locale\n";
print "Content-type:application/json\r\n\r\n";
print "Content-Type: application/json\n\n", $json_pp->encode( \%FORM );

1;

