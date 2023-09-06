use Data::Dumper;
use HTTP::Tiny;

$url = 'http://localhost:80/cgi-bin/statuscode.cgi?code=304';
my $debug    = 0;
my $response = HTTP::Tiny->new->get($url);

if ( !$response->{'success'} ) {
    print STDERR 'Failed!', Dumper($response), $/ if $debug;
    print "$response->{status} $response->{reason}\n";

}
else {


    # while (my ($k, $v) = each %{$response->{headers}}) {
    #    for (ref $v eq 'ARRAY' ? @$v : $v) {
    #        print "$k: $_\n";
    #    }
    #}

    print $response->{content} if length $response->{content};
}
