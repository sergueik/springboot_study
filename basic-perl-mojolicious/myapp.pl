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
# use Mojo::Template;
# push @{$app->static->paths}, app->home->rel_file('build')->to_string;
app->static->paths(['./build', '/public']);  # React build folder
app->renderer->paths(['/templates']);

get '/inline' => sub {

	my $c = shift;
	$c->reply->static('inline/index.html');

};

get '/old' => sub {

	$_[0]->reply->static('old/index.html');
	# my $c = shift;
	# my $mt = Mojo::Template->new;
	# say $mt->render_file('/build/index.html');
};

get '/jsx' => sub {
  my $c = shift;
  $c->render(template => 'jsx');
};

get '/api/greeting' => sub {
    shift->render(json => { message => 'Hello from Mojolicious' });
};

app->start;

# our $json_pp = JSON::PP->new->ascii->pretty->allow_nonref;
# my $buffer;

# 
# https://stackoverflow.com/questions/75999522/perl-mojolicious-whats-the-correct-way-to-render-a-response-from-a-promise

1;


