use InfluxDB::Client::Simple;
use Data::Dumper;
use Getopt::Long;
use JSON::PP;
use Time::HiRes qw( gettimeofday);
use Sys::Hostname;
use warnings;
use strict;

my $measurement = 'testing';
my $value       = 1;
my $database    = 'example';
my $defer       = undef;
my $debug       = undef;
my $host        = '192.168.0.29';
my $port        = '8086';
my $precision   = 's';

GetOptions(
    'value=s'       => \$value,
    'database=s'    => \$database,
    'measurement=s' => \$measurement,
    'debug'         => \$debug,
    'port=s'        => \$port,
    'precision=s'   => \$precision,
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
print 'Check server connectivity' . $/;
my $result = $client->ping();
die 'No response' unless $result;

# get the server version
print $result->{version};

our $reporting_host = exists $ENV{'COMPUTERNAME'} ? lc( $ENV{'COMPUTERNAME'} ) : hostname;
print "Reporting host: " . $reporting_host . $/;
our $environment = 'UAT';

my $timestamp;
my $timestamp_seconds = time();
my $periods = { 'm' => 60, 'h' => 3600 };
if ( !$precision cmp 's' ) {
    $timestamp = $timestamp_seconds;
}
elsif ( $precision =~ /\b(?:m|h)\b/ ) {
    my $period = $periods->{$precision};
    $timestamp = int( $timestamp_seconds / $period ) * $period;
}
else {
    my ( $seconds, $microseconds ) = gettimeofday();
    my $timestamp_nanoseconds = $seconds . $microseconds;
    $timestamp = $timestamp_nanoseconds;
}

# Write
print 'Write' . $/;

# https://docs.influxdata.com/influxdb/v1.8/write_protocols/line_protocol_tutorial/
# https://perldoc.perl.org/Time::HiRes
# see also:
# https://stackoverflow.com/questions/55308595/how-to-round-up-a-timestamp-to-nearest-5-minute-value
# NOTE - cannot enclose tag values with spaces
# using URL encoding protects from error in runtime (not really)
# but the tag is not seen in the influx
# my $tag_set = "host=${reporting_host},env=${environment},description=\"long%20description\"";
my $tag_set   = "host=${reporting_host},env=${environment}";
my $field_set = "value=${value}";
$result = $client->write(
    "${measurement},${tag_set} ${field_set} ${timestamp}",
    database    => $database,
    'precision' => $precision
);
print Dumper($result) if $debug;

# Send Data
print 'Send Data' . $/;
$tag_set = { host => $reporting_host, env => $environment };
$field_set = { value => $value };
my %options = ( 'database' => $database, 'precision' => $precision );
$client->send_data( $measurement, $tag_set, $field_set, $timestamp, %options );

# Query
print 'Query' . $/;
$result = $client->query(
    ["SELECT * FROM ${measurement}"],
    database  => $database,
    epoch     => $precision,
    chunksize => 0
);
print 'series:' . $/;

print Dumper( $result->{data}->{results}->[0]->{series} );

# not trying UDP
# may even remove to covert to pure Perl

