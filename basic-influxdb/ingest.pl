use InfluxDB::Client::Simple;
use Data::Dumper;
use Getopt::Long;
use JSON::PP;
use Time::HiRes qw( gettimeofday);
use Time::Local qw(timelocal);
use Sys::Hostname;
use warnings;
use strict;

use vars qw($month_alpha $months_alpha $mon $mday $hour $min $sec $year);

my $measurement = 'testing';
my $value       = '42.0';
my $database    = 'example';
my $defer       = undef;
my $now         = undef;
my $debug       = undef;
my $host        = '192.168.0.29';
my $timestamp   = undef;
my $port        = '8086';
my $appid       = 'FOO,BAR,BAZ';
my $precision   = 's';
my $environment = 'UAT';

GetOptions(
    'appid=s'       => \$appid,
    'database=s'    => \$database,
    'debug'         => \$debug,
    'defer'         => \$defer,
    'now'           => \$now,
    'host=s'        => \$host,
    'timestamp=s'   => \$timestamp,
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
print STDERR 'Check server connectivity' . $/;
my $result = $client->ping();
die 'No response' unless $result;

# get the server version
print STDERR $result->{version} . $/;

our $reporting_host =
  exists $ENV{'COMPUTERNAME'} ? lc( $ENV{'COMPUTERNAME'} ) : hostname;
print STDERR "Reporting host: " . $reporting_host . $/;
if ( !defined($timestamp) ) {
    print STDERR 'generating timestamp' . $/;

    my $timestamp_seconds = time();
    my $periods = { 'm' => 60, 'h' => 3600 };
    if ( !( $precision cmp 's' ) ) {
        print STDERR 'using presision SECONDS' . $/;
        $timestamp = $timestamp_seconds;
        print STDERR $timestamp . $/ if $debug;
    }
    elsif ( $precision =~ /\b(?:m|h)\b/ ) {
        my $period = $periods->{$precision};
        $timestamp = (int( $timestamp_seconds / $period ) * $period);
        print STDERR 'using presision MINUTES' . $/;
        # TODO: invalid timestamp:
        # error from InfluxDB 
        # error time outside range -9223372036854775806 - 9223372036854775806"
        print STDERR $timestamp . $/ if $debug;
    }
    else {
        my ( $seconds, $microseconds ) = gettimeofday();
        my $timestamp_nanoseconds = $seconds . sprintf('%06d',$microseconds) . '000';
        print STDERR 'using presision NANOSECONDS' . $/;
        $timestamp             = $timestamp_nanoseconds;
        print STDERR $timestamp . $/ if $debug;
    }
}
else {
    # get timestamp from caller provided argument
    print STDERR 'use caller provided timestamp ' . $timestamp . $/ if $debug;
    ( undef, $month_alpha, $mday, $hour, $min, $sec, undef, $year ) =
      split /(?:\s+|:)/, $timestamp;
    $months_alpha = {
        'Jan' => 1,
        'Feb' => 2,
        'Mar' => 3,
        'Apr' => 4,
        'May' => 5,
        'Jun' => 6,
        'Jul' => 7,
        'Aug' => 8,
        'Sep' => 9,
        'Oct' => 10,
        'Nov' => 11,
        'Dec' => 12

    };
    $mon = $months_alpha->{$month_alpha} - 1;

    # Note the reverse order of the arguments and that January is month 0
    # https://stackoverflow.com/questions/95492/how-do-i-convert-a-date-time-to-epoch-time-unix-time-seconds-since-1970-in-per
    $timestamp =
      ( timelocal( $sec, $min, $hour, $mday, $mon, $year ) ) . '000000000';
    print STDERR 'timestamp: ',$/, $timestamp, $/;

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
    push(
        @$measurements,
        (
            $now
            ? "${measurement},${tag_set} ${field_set}"
            : "${measurement},${tag_set} ${field_set} ${timestamp}"
        )
    );
}

# Write
print STDERR 'Write' . $/;
my $do_write = 1;
if ($do_write) {
    $result = $client->write(
        $measurements,
        database  => $database,
        precision => $precision
    );
    print STDERR Dumper($result) if $debug;

}

# 'partial write: field type conflict: input field "value" on measurement "testing" is type float, already exists as type integer dropped=3',
# TODO: check if send_data allows null timestamp argument
my $do_send = 0;
if ($do_send) {
    $field_set = { value => $value };
    my %options = ( 'database' => $database, 'precision' => $precision );
    foreach $appid_tag ( split( /,/, $appid ) ) {

        # Send Data
        print STDERR 'Send Data' . $/;
        $tag_set = {
            host      => $reporting_host,
            env       => $environment,
            appid     => $appid_tag,
            operation => 'send'
        };
        $result =
          $client->send_data( $measurement, $tag_set, $field_set, $timestamp,
            %options );
        print STDERR Dumper($result) if $debug;
    }
}

# Query
print STDERR 'Query' . $/;
$result = $client->query(
    ["SELECT * FROM ${measurement}"],
    database  => $database,
    epoch     => $precision,
    chunksize => 0
);
print STDERR 'series:' . $/;

print STDERR Dumper( $result->{data}->{results}->[0]->{series} );

print STDERR 'count' . $/;
foreach my $operation (qw|write send|) {
    $result = $client->query(
        [
"SELECT COUNT(*) FROM ${measurement} WHERE operation = '${operation}'"
        ],
        database  => $database,
        epoch     => $precision,
        chunksize => 0
    );
    print STDERR 'Result for ' . $operation . ':' . $/;

    # print STDERR Dumper($result);
    print STDERR Dumper(
        $result->{data}->{results}->[0]->{series}->[0]->{values}->[0]->[1] );
}

# not trying UDP
# may even remove to covert to pure Perl

