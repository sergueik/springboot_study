use Test::More;
use Test::Mojo;
use Mojo::JSON 'decode_json';

my $t = Test::Mojo->new('server.pl');

$t->options_ok('/data')
  ->status_is(200)
  ->header_like('access-control-allow-origin' => qr/http:\/\/localhost:9090/)
  ->header_like('access-control-allow-methods' => qr/POST/)
  ->header_like('access-control-allow-headers' => qr/Content-Type/);

$t->post_ok('/data' => json => { square => 'b2', dwell => 1.23 })
  ->status_is(200)
  ->json_has('/status')
  ->json_is('/status' => 'ok');

done_testing;
1;
__END__
