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

# CORS headers
app->hook(before_dispatch => sub {
  my $c = shift;
  $c->res->headers->header('Access-Control-Allow-Origin'  => '*');
  $c->res->headers->header('Access-Control-Allow-Methods' => 'GET, POST, OPTIONS');
  $c->res->headers->header('Access-Control-Allow-Headers'=> 'Content-Type');
});
post '/data' => sub {

	my $c = shift;
	$c->reply->static('inline/index.html');
# our $json_pp = JSON::PP->new->ascii->pretty->allow_nonref;
# my $buffer;
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
