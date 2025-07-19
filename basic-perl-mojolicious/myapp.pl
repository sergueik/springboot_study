#!/usr/bin/perl

use strict;

BEGIN {
    # NOTE: use environment to construct @INC
    use constant HOME    => (
        do { $_ = $ENV{HOME}; /\/([^\/]+)$/ }
    );
    use constant SCRIPT_DIR => (
        do { my $s = `dirname $0`; chomp $s; $s }
    );
        unshift( @INC, SCRIPT_DIR . '/lib' );
        unshift( @INC, `pwd` . '/lib' );
}
use JSON::PP;
use Mojolicious::Lite;

get '/api/greeting' => sub {
    my $c = shift;
    $c->render(json => { message => 'Hello from Mojolicious' });
};

app->start;

# our $json_pp = JSON::PP->new->ascii->pretty->allow_nonref;
# my $buffer;

# 
# https://stackoverflow.com/questions/75999522/perl-mojolicious-whats-the-correct-way-to-render-a-response-from-a-promise

1;

