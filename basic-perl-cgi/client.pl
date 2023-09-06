use Data::Dumper;
use HTTP::Tiny;

$url = 'http://192.168.99.100:9090/cgi-bin/statuscode.cgi?code=304';
$url =
'http://192.168.99.100:9090/cgi-bin/file_hash.cgi?inputfile=example_config.json&hash=9f8377db38593544a5e994006fe4e9e4';

# $url = 'http://192.168.99.100:9090/cgi-bin/file_hash_status.cgi?inputfile=example_config.json&hash=9f8377db38593544a5e994006fe4e9e4';
my $DEBUG    = 0;
my $response = HTTP::Tiny->new->get($url);

print "$response->{status} $response->{reason}\n";

if ( !$response->{'success'} ) {
    print STDERR 'Failed!', Dumper($response), $/ if $DEBUG;
}
else {

    # while (my ($k, $v) = each %{$response->{headers}}) {
    #    for (ref $v eq 'ARRAY' ? @$v : $v) {
    #        print "$k: $_\n";
    #    }
    #}

    print $response->{content} if length $response->{content};
}
