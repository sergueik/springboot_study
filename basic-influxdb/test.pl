use InfluxDB::Client::Simple;
use Data::Dumper;
use Getopt::Long;

use warnings;
use strict;

my $metric    = 'testing';
my $value    = 1;
my $database = 'example';
my $defer    = undef;
my $debug    = undef;
my $host     = 'localhost';
my $port     = '8086';

GetOptions(
    'value=s'    => \$value,
    'database=s' => \$database,
    'metric=s'   => \$metric,
    'debug'      => \$debug,
    'port=s'     => \$port,
    'host=s'     => \$host,
    'defer'      => \$defer

);

my $client = InfluxDB::Client::Simple->new(
    host     => $host,
    port     => $port,
    protocol => 'tcp'
) or die "Can't instantiate client";

# Check server connectivity
my $result = $client->ping();
die "No pong" unless $result;

# get the server version
print $result->{version};
our $reporting_host = lc( $ENV{'COMPUTERNAME'} );
our $environment    = 'UAT';

# Write
$result = $client->write(
    "${metric},host=${reporting_host},env=${environment} value=${value}",
    database => $database );
print Dumper($result);

# not trying UDP
# may even remove to covert to pure Perl

