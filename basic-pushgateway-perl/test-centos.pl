use Net::Prometheus::Pushgateway;
use Getopt::Long;
use Sys::Hostname;
use Socket;
use Data::Dumper;
use strict;

my $host = 'pushgateway';
my $value  = 1;
my $job = 'my_custom_metrics';
my $team = 'test';
my $metric = 'perl_counter'; 
GetOptions(
  'value=i' => \$value,
  'job=s' => \$job,
  'host=s' => \$host,
  'team=s' => \$team,
  'metric=s' => \$metric
);

# NOTE non-standard param key names inherited from Net::Prometheus::Pushgateway

my $addr = inet_ntoa((gethostbyname(hostname))[4]);
my $instance = hostname . '(' . $addr . ')';
# https://stackoverflow.com/questions/4510550/using-perl-how-do-i-decode-or-create-those-encodings-on-the-web
print STDERR $addr;
$instance =~ s/([^^A-Za-z0-9\-_.!~*'()])/ sprintf "%%%0x", ord $1 /eg;
print STDERR $instance;
my $opt =  {
  '-host' => $host,
  '-port' => 9091,
  '-path' => "/metrics/job/${job}/instance/${instance}/team/${team}",
  '-timeout' => 10 
};
my $o = Net::Prometheus::Pushgateway->new(%$opt );
my $custom = {
  '-metric_name' => $metric,
  '-label' => {},
  '-value' => $value
};
 # NOTE: the leading slash is required
 # '/test'  404
 # '/'      405
 # 'test'   599

$o -> add(%$custom);
print STDERR "\n" . 'Payload:' . "\n" . $o->{raw_str};

$o->increment(
  -metric_name => $metric,
  -label => {
    'perl_label' => 'custom label'
  }, 
  -value => $value); 

__END__

