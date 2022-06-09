use InfluxDB::Client::Simple;
use Data::Dumper;
use Getopt::Long;
use JSON::PP;

use warnings;
use strict;

my $measurement = 'testing';
my $value       = 1;
my $database    = 'example';
my $defer       = undef;
my $debug       = undef;
my $host        = '192.168.0.29';
my $port        = '8086';

GetOptions(
    'value=s'       => \$value,
    'database=s'    => \$database,
    'measurement=s' => \$measurement,
    'debug'         => \$debug,
    'port=s'        => \$port,
    'host=s'        => \$host,
    'defer'         => \$defer

);

my $client = InfluxDB::Client::Simple->new(
    host     => $host,
    port     => $port,
    protocol => 'tcp'
) or die "Can't instantiate client";

$client->debug($debug);

# Check server connectivity
print 'Check server connectivity' . "\n";
my $result = $client->ping();
die 'No response' unless $result;

# get the server version
print $result->{version};
our $reporting_host = lc( $ENV{'COMPUTERNAME'} );
our $environment    = 'UAT';

# Write
print 'Write' . "\n";
$result = $client->write("${measurement},host=${reporting_host},env=${environment} value=${value}", database => $database );
print Dumper($result);

# Send Data
print 'Send Data' . "\n";
my $tags = { host => $reporting_host, env => $environment };
my $fields = { value => $value };
my %options = ( 'database' => $database, 'precision' => 's' );
my $timestamp = time();

$client->send_data( $measurement, $tags, $fields, $timestamp, %options );

# Query
print 'query' . "\n";
$result = $client->query(
    ["SELECT * FROM ${measurement}"],
    database  => $database,
    epoch     => 'm',
    chunksize => 0
);
print 'series:' . "\n";

print Dumper($result->{data}->{results}->[0]->{series});

# not trying UDP
# may even remove to covert to pure Perl

