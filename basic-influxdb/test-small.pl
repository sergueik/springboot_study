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
my $host        = '127.0.01';
my $port        = '8085';
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

my $reporting_host =
  exists $ENV{'COMPUTERNAME'} ? lc( $ENV{'COMPUTERNAME'} ) : hostname;
print "Reporting host: " . $reporting_host . $/;
our $environment = 'UAT';
my $result;
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

    # TODO: support "precision"
    $result = $client->write( $measurements, database => $database );
    print Dumper($result) if $debug;

}

# 'partial write: field type conflict: input field "value" on measurement "testing" is type float, already exists as type integer dropped=3',

my $do_send = 1;
if ($do_send) {
    $field_set = { value => $value };
    my %options = ( 'database' => $database );
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