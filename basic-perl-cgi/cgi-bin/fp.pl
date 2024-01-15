#!/usr/bin/perl

use strict;

# Origin: TPL
my $funcs = {
    'x' => sub { sprintf 'xxx %s', shift },
    'y' => sub { sprintf 'yyy %d', shift },
};
my $data = { 'x' => 10, 'y' => 20 };
map { print $_ . ': ' . $funcs->{$_}->( $data->{$_} ) . $/ } keys %$data;

