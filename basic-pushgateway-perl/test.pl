use Pushgateway::Tiny;
use Sys::Hostname;
use Socket;
use Data::Dumper;
use strict;
# NOTE non-standard param key names inherited from Net::Prometheus::Pushgateway

my $job_name = 'my_custom_metrics';
my $addr = inet_ntoa((gethostbyname(hostname))[4]);
my $instance_name = hostname . '(' . $addr . ')';
# https://stackoverflow.com/questions/4510550/using-perl-how-do-i-decode-or-create-those-encodings-on-the-web
print STDERR $addr;
$instance_name =~ s/([^^A-Za-z0-9\-_.!~*'()])/ sprintf "%%%0x", ord $1 /eg;
print STDERR $instance_name;
my $team = 'test';
my $opt =  {
  '-host' => 'pushgateway',
  '-port' => 9091,
  '-path' => "/metrics/job/${job_name}/instance/${instance_name}/team/${team}",
  '-timeout' => 10 };
my $o = Pushgateway::Tiny->new(%$opt );
my $custom = {
  '-metric_name' => 'test',
  '-label' => {},
  '-value' => 42
 };
 # NOTE: the leading slash is required
 # '/test'  404
 # '/'      405
 # 'test'   599

$o -> add(%$custom);
print STDERR "\n" . 'Payload:' . "\n" . $o->{raw_str};


$o->summary(-metric_name => 'perl_metric_summary', -label => {'perl_label' => 5}, -value => 15);
print STDERR "\n" . 'Payload:' . "\n" . $o->{raw_str};
__END__