use InfluxDB::Client::Simple;
use Data::Dumper; 
########################## TCP ##########################
my $client = InfluxDB::Client::Simple->new( host => 'localhost', port => 8086, protocol => 'tcp' ) or die "Can't instantiate client";
 
# Check server connectivity
my $result = $client->ping();
die "No pong" unless $result;
 
# You can also get the server version
print $result->{version};

# Write
$result = $client->write("testing,host=containment,repo=cadi-libs,file=testfile statement=42,pod=85", database => 'example');
print print Dumper($result);
# not trying UDP - may  temporarily remove to covert to pure Perl
