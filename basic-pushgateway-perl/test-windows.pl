use Pushgateway::TinyActiveState;
use Getopt::Long;
use Sys::Hostname;
use Socket;
use Data::Dumper;
use strict;

my $value = 1;
my $file  = undef;
my $defer = undef;
my $debug = undef;
my $host = 'pushgateway';
my $port = '9091';

GetOptions(
    'value=s' => \$value,
    'file=s'  => \$file,
    'debug'   => \$debug,
    'port=s'  => \$port,
    'host=s'  => \$host,
    'defer'   => \$defer

);

# NOTE non-standard param key names inherited from Net::Prometheus::Pushgateway

my $job_name      = 'my_custom_metrics';
my $addr          = inet_ntoa( ( gethostbyname(hostname) )[4] );
my $instance_name = hostname . '(' . $addr . ')';

# https://stackoverflow.com/questions/4510550/using-perl-how-do-i-decode-or-create-those-encodings-on-the-web
print STDERR $addr if $debug;
$instance_name =~ s/([^^A-Za-z0-9\-_.!~*'()])/ sprintf "%%%0x", ord $1 /eg;
print STDERR $instance_name if $debug;
my $team = 'test';
my $opt  = {
    'instance_name' => $instance_name,
    '-host' => $host,
    '-port' => $port,
    '-path' =>
      "/metrics/job/${job_name}/instance/${instance_name}/team/${team}",
    '-timeout' => 10
};
if ($defer) {
    $opt->{'-defer'} = 1;
}
my $o      = Pushgateway::TinyActiveState->new(%$opt);
my $custom = {
    '-metric_name' => 'test',
    '-label'       => {},
    '-value'       => 42
};

# NOTE: the leading slash is required
# '/test'  404
# '/'      405
# 'test'   599

$o->add(%$custom);
print STDERR "\n" . 'Payload:' . "\n" . $o->{raw_str} if $debug;

# Send counter metric
$o->increment(
    -metric_name => 'perl_counter',
    -label       => { 'perl_label' => 'custom label' },
    -value       => $value
);
print STDERR "\n" . 'Payload:' . "\n" . $o->{raw_str} if $debug;
my $gauge = {
    '-metric_name' => 'perl_gauge',
    '-value'       => 10,
    '-label'       => {
        'instance2' => 'node1'
    }
};

# Send gauge metric
$o->gauge(%$gauge);
print STDERR "\n" . 'Payload:' . "\n" . $o->{raw_str} if $debug;

# Send histogram metric
$o->histogram(
    -metric_name => 'perl_metric_histogram',
    -label       => { 'perl_label' => 5 },
    -value       => 15,
    -buckets     => [qw/1 2 3 4 5/]
);

print STDERR "\n" . 'Payload:' . "\n" . $o->{raw_str} if $debug;

if ($defer) {
    my $raw_data = join "\n", @{ $o->{raw_data} };
    print STDERR "\n" . 'Payload:' . "\n" . $raw_data if $debug;
    $o->{defer} = 0;
    $o->_send_to_prometheus($raw_data);
}
__END__
