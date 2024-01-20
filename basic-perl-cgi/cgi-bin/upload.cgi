#!/usr/bin/perl

use strict;

BEGIN {
    use constant RELEASE => 0;
    use constant HOME    => (
        do { $_ = $ENV{HOME}; /\/([^\/]+)$/ }
    );
    use constant SCRIPT_DIR => (
        do { my $s = `dirname $0`; chomp $s; $s }
    );
    if (RELEASE) {

        # TODO: set extra lib path in RELEASE
    }
    else {
        unshift( @INC, SCRIPT_DIR );
        unshift( @INC, `pwd` );
    }
}
use CGI::Tiny;
use Getopt::Long;
use Data::Dumper;
use Time::Local qw(timelocal);
use InfluxDB::Client::SimpleAlpine;
use JSON::PP;
use Time::HiRes qw( gettimeofday);
use Sys::Hostname;
use strict;
use warnings;
use utf8;

use vars qw($cgi $filename $line );
use vars
  qw($payload $data $metrics $value $collect_metrics $month_alpha $months_alpha $mon $mday $hour $min $sec $year);

cgi {
    $cgi = $_;
    $cgi->{multipart_form_options} = { parse_as_files => 0 };

    if ( $cgi->method ne 'POST' ) {
        $cgi->set_response_status(405);    # METHOD_NOT_ALLOWED
        exit;
    }

    # $cgi->_body_multipart();
    if ( $cgi->param('type') !~ /send/ ) {
        $cgi->set_response_status(400);    # BAD_REQUEST
        exit;
    }
    my $upload  = $cgi->upload('data');
    my $content = $upload->{content};

    if ( $cgi->param('new') ) {
        print STDERR "do ingestion of $content", $/;
        $data = read_data($content);
        print STDERR Dumper($data), $/;
        $metrics         = {};
        $value           = undef;
        $collect_metrics = [ 'mem', 'swap', 'load_average', 'disk' ];
        foreach my $metric (@$collect_metrics) {

            # print STDERR "loading ${metric}", $/;
            if ( $metric eq 'cpu' ) {
            }
            elsif ( $metric eq 'swap' ) {

                # print STDERR "splitting '" . $data->{swap} . "'" . $/;
                my ( $m1, $m2, undef ) = split /\s+/, $data->{swap};

                # print STDERR Dumper( { m1 => $m1, m2 => $m2 } ), $/;
                if ( $m2 != 0 ) {
                    $value = $m2 * 100.0 / $m1;
                    $metrics->{$metric} = $value;
                }
            }
            elsif ( $metric eq 'disk' ) {
            }
            elsif ( $metric eq 'load_average' ) {

                ( undef, $value, undef, undef, undef ) =
                  split /\s+/,
                  $data->{load_average};
                $metrics->{$metric} = $value;
            }

        }

        # get timestamp from payload data. For testing only
        if ( $data->{date} != '' ) {

            ( undef, $month_alpha, $mday, $hour, $min, $sec, undef, $year ) =
              split /(?:\s+|:)/, $data->{date};
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

            # print STDERR "month_alpha:" . $month_alpha, $/;
            $mon = $months_alpha->{$month_alpha} - 1;

# print STDERR "mon:" . $mon, $/;
# Note the reverse order of the arguments and that January is month 0
# https://stackoverflow.com/questions/95492/how-do-i-convert-a-date-time-to-epoch-time-unix-time-seconds-since-1970-in-per
            $metrics->{time} =
              timelocal( $sec, $min, $hour, $mday, $mon, $year );
        }

        # get timestamp from payload data
        $metrics->{computer} = $data->{computer};
        print STDERR Dumper($metrics), $/;

        my $measurement = 'testing';
        my $value       = '42.0';
        my $database    = 'example';
        my $now         = undef;
        my $host        = 'boring_williams';
        my $port        = '8086';
        my $appid       = 'FOO,BAR,BAZ';
        my $precision   = 'ns';
        my $client      = InfluxDB::Client::SimpleAlpine->new(
            host     => $host,
            port     => $port,
            protocol => 'tcp'
        ) or die "Can't instantiate client";

        my $time_seconds   = $metrics->{time};
        my $timestamp      = $time_seconds . '000000000';
        my $reporting_host = $metrics->{computer};
        my $field_set      = 'value=' . $metrics->{swap};
        my $appid_tag;
        my $tag_set;
        my $measurements = [];
        foreach $appid_tag ( split( /,/, $appid ) ) {
            $tag_set =
              "host=${reporting_host},appid=${appid_tag},operation=write";
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
        print STDERR Dumper($measurements), $/;
        my $do_write = 1;
        if ($do_write) {
            my $result = $client->write(
                $measurements,
                database  => $database,
                precision => $precision
            );
            print STDERR Dumper($result), $/;

        }

        # TODO: implement original processing:
        # print to some local file handle
        # do print to console for demo
        print STDERR $content;
    }
    else {
        foreach $line ( split /\r?\n/, $content ) {
            print STDERR $line, $/;
        }

    }

    $cgi->set_response_type('text/html');

    $filename = $upload->{filename} || 'unknown file';
    $cgi->render( html => <<EOF);
<!DOCTYPE html>
<html>
<head>
<meta http-equiv="Content-Type" content="text/html; charset=utf-8" />
<title>Thanks!</title>
<style type="text/css">
img {border: none;}
</style>
</head>
<body>
<p>Thanks for uploading data</p>
<p><img src="/upload/$filename" alt="data" /></p>
</body>
</html>

EOF

    sub read_data {

        my $payload = shift;
        my $data    = {};

        foreach my $line ( split /\r?\n/, $payload ) {
            my ( $first, $rest ) = split( ' ', $line, 2 );
            my ( $key, ) = split '[:=]', $first;
            my $value = $rest;
            $key = lc $key;
            $data->{$key} = $value;
        }
        return $data;
    }

}
