use Data::Dumper;
use Time::Local qw(timelocal);
my $collect_metrics = [ 'mem', 'swap', 'load_average', 'disk' ];

sub read_data {

    my $payload = shift;
    my %data    = ();

    foreach my $line ( split /\r?\n/, $payload ) {
        my ( $keyx, $value ) = split( ' ', $line, 2 );
        my ( $key, ) = split '[:=]', $keyx;
        $key = lc $key;
        $data{$key} = $value;
    }
    return \%data;
}

sub get_payload {
    my $payload = <<EOF;

mem:        1863756      665656      157580
swap:       2720764       24832     2695932
load_average: 0.16 0.08 0.08 1/460 32100
rpm: 104
date: Sun Jun 26 18:54:31 EDT 2022
computer: lenovo120S.private.org
uptime: 18:56:03 up 1 day,  3:44,  3 users,  load average: 0.07, 0.10, 0.09
disk: /dev/sda1 27G 22G 3.6G 86% /
EOF
    return $payload;
}
my $payload = &get_payload;
my $data    = read_data($payload);
# print STDERR Dumper($data);
# print STDERR Dumper($collect_metrics);
my $metrics = {};
my $value   = undef;
foreach my $metric (@$collect_metrics) {
    # print STDERR "loading ${metric}", $/;
    if ( $metric eq 'cpu' ) {
    }
    elsif ( $metric eq 'swap' ) {
        # print STDERR "splitting '" . $data->{swap} . "'" . $/;
        my ( $m1, $m2, undef ) = split /\s+/, $data->{swap};
        # print STDERR Dumper( { m1 => $m1, m2 => $m2 } ), $/;
        $value = $m2 * 100.0 / $m1;
        $metrics->{$metric} = $value;
    }
    elsif ( $metric eq 'disk' ) {
    }
    elsif ( $metric eq 'load_average' ) {

        # print STDERR "splitting '" . $data->{load_average} . "'" . $/;
        ( undef, $value, undef, undef, undef ) = split /\s+/,
          $data->{load_average};
        $metrics->{$metric} = $value;
    }

}

# get timestamp from payload data. For testing only
# Sun Jun 26 18:54:31 EDT 2022
$data->{date} = 'Sun Jun 26 18:54:31 EDT 2022';

my $month_alpha, $mday, $hour, $min, $sec, $year;
my @fields = 
  split /(?:\s+|:)/, $data->{date};
# print STDERR Dumper \@fields, $/;
( undef, $month_alpha, $mday, $hour, $min, $sec, undef, $year ) =
  split /(?:\s+|:)/, $data->{date};
my $months_alpha = {
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
my $mon = $months_alpha->{$month_alpha};
# print STDERR "mon:" . $mon, $/;

$metrics->{time} = timelocal( $sec, $min, $hour, $mday, $mon, $year );

# get timestamp from payload data
$metrics->{computer} = $data->{computer};
print STDERR Dumper($metrics), $/;
