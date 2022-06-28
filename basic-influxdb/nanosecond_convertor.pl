use Data::Dumper;
use Getopt::Long;
use Time::HiRes qw( gettimeofday);
use Time::Local qw(timelocal);
use warnings;
use strict;

use vars qw($month_alpha $months_alpha $mon $mday $hour $min $sec $year);

# to compare with InfluxDB generated value - to be implemented
my $now         = undef;
my $debug       = undef;
my $timestamp   = undef;
my $precision   = 'ns';
GetOptions(
    'debug'         => \$debug,
    'now'           => \$now,
    'timestamp=s'   => \$timestamp,
    'precision=s'   => \$precision
);

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
        $timestamp = int( $timestamp_seconds / $period ) * $period;
        print STDERR 'using presision MINUTES' . $/;
        print STDERR $timestamp . $/ if $debug;
    }
    else {
        my ( $seconds, $microseconds ) = gettimeofday();
        my $timestamp_nanoseconds = $seconds . sprintf('%06d',$microseconds) . '000';

        print STDERR 'using presision NANOSECONDS' . $/;
        # $timestamp_nanoseconds = $seconds . '000000000';
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

