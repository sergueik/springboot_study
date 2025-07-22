#!/usr/bin/perl

use strict;

BEGIN {
    # NOTE: use environment to construct @INC
    use constant HOME => (
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

get '/data' => sub {
    my $c  = shift;
    my $ts = $c->param('ts');
    app->log->info( "Received timestamp: " . ( $ts // " data missing" ) );

    # || checks if the value is truthy (e.g., 0 is false).
    # // checks if the value is defined (allows 0 to pass through).
    # // is available in Perl 5.10

    my %render_data =
      !defined $ts
      ? (
        status => 400,
        json   => { error => "Missing required 'ts' query parameter" }
      )
      : $ts !~ /^\d{10}$/ ? (
        status => 405,
        json   => { error => "'ts' must be a valid Unix timestamp in seconds" }
      )
      : ( json => { metric_value => 42, timestamp => $ts } );

    $c->render(%render_data);

};

app->start;

1;

