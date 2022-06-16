use InfluxDB::Client::Simple;
use Data::Dumper;
use Getopt::Long;
use JSON::PP;
use Time::HiRes qw( gettimeofday);
use Sys::Hostname;
use warnings;
use strict;

my $measurement = 'testing';
my $value       = '42.0';
my $database    = 'example';
my $defer       = undef;
my $debug       = undef;
my $host        = '192.168.0.29';
my $port        = '8086';
my $appid       = 'FOO,BAR,BAZ';
my $precision   = 's';

GetOptions(
    'appid=s'       => \$appid,
    'database=s'    => \$database,
    'debug'         => \$debug,
    'defer'         => \$defer,
    'host=s'        => \$host,
    'measurement=s' => \$measurement,
    'port=s'        => \$port,
    'precision=s'   => \$precision,
    'value=s'       => \$value,

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

our $reporting_host =
  exists $ENV{'COMPUTERNAME'} ? lc( $ENV{'COMPUTERNAME'} ) : hostname;
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

# https://docs.influxdata.com/influxdb/v1.8/write_protocols/line_protocol_tutorial/
# https://perldoc.perl.org/Time::HiRes
# see also:
# https://stackoverflow.com/questions/55308595/how-to-round-up-a-timestamp-to-nearest-5-minute-value
# NOTE - cannot enclose tag values with spaces
# using URL encoding protects from error in runtime (not really)
# but the tag is not seen in the influx
# my $tag_set = "host=${reporting_host},env=${environment},description=\"long%20description\"";
# NOTE attempt to add multiple tag values with same tag name, leads to the server status
# 400  (BAD REQUEST)
# unable to parse ... : duplicate tags'

my $field_set = "value=${value}";
my $appid_tag;
my $tag_set;
my $measurements = [];
foreach $appid_tag ( split( /,/, $appid ) ) {
    $tag_set =
"host=${reporting_host},env=${environment},appid=${appid_tag},operation=write";
    push( @$measurements,
        "${measurement},${tag_set} ${field_set} ${timestamp}" );
}

# Write
print 'Write' . $/;
my $do_write = 1;
if ($do_write) {
    $result = $client->write(
        $measurements,
        database  => $database,
        precision => $precision
    );
    print Dumper($result) if $debug;

}

# 'partial write: field type conflict: input field "value" on measurement "testing" is type float, already exists as type integer dropped=3',

my $do_send = 1;
if ($do_send) {
    $field_set = { value => $value };
    my %options = ( 'database' => $database, 'precision' => $precision );
    foreach $appid_tag ( split( /,/, $appid ) ) {

        # Send Data
        print 'Send Data' . $/;
        $tag_set = {
            host      => $reporting_host,
            env       => $environment,
            appid     => $appid_tag,
            operation => 'send'
        };
        $result =
          $client->send_data( $measurement, $tag_set, $field_set, $timestamp,
            %options );
        print Dumper($result) if $debug;
    }
}

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

print 'count' . $/;
foreach my $operation (qw|write send|) {
    $result = $client->query(
        [
"SELECT COUNT(*) FROM ${measurement} WHERE operation = '${operation}'"
        ],
        database  => $database,
        epoch     => $precision,
        chunksize => 0
    );
    print 'Result for ' . $operation . ':' . $/;

    # print Dumper($result);
    print Dumper(
        $result->{data}->{results}->[0]->{series}->[0]->{values}->[0]->[1] );
}

# not trying UDP
# may even remove to covert to pure Perl

