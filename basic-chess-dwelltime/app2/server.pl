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

our $json_pp = JSON::PP->new->ascii->pretty->allow_nonref;

# CORS headers
app->hook(before_dispatch => sub {
  my $c = shift;
  $c->res->headers->header('Access-Control-Allow-Origin'  => '*');
  $c->res->headers->header('Access-Control-Allow-Methods' => 'GET, POST, OPTIONS');
  $c->res->headers->header('Access-Control-Allow-Headers'=> 'Content-Type');
});

options '/data' => sub {
   $_[0]->render(text => '', status => 200);
};

post '/data' => sub {

	my $c = shift;
	my $body = $c->req->body || '{"status": "no data"}';
	my $payload = eval { decode_json($body) } || {};
	# Asume RAW DATA - headers seems to be ignored
	app->log->info('Received headers: ' . $c->dumper($c->req->headers));
	app->log->info("Received payload: " . $c->dumper($body));
	 $c->render(json => {status => 'ok'});
};

get '/health' => sub {
    shift->render(json => { message => 'Hello from Mojolicious' });
};

app->start;

1;


__END__

etch('/upload', {
  method: 'POST',
  headers: {'Content-Type': 'application/json'},
  body: JSON.stringify({foo: 'bar'})
})
.then(res => {
  console.log(res.headers.get('X-Intended-Flag'));
});
