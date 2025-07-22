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

get '/data' => sub {
  my $c  = shift;
  my $ts = $c->param('ts');
  app->log->info("Received timestamp: ". ( defined $ts ?   $ts : " data missing"));
  
  unless (defined $ts) {
    return $c->render(
      status => 400,
      json   => { error => "Missing required 'ts' query parameter" }
    );
  }

  unless ($ts =~ /^\d{10}$/) {
    return $c->render(
      status => 405,
      json   => { error => "'ts' must be a valid Unix timestamp in seconds" }
    );
  }

  $c->render(json => {
    metric_value => 42,
    timestamp    => $ts
  });

};

app->start;

1;


