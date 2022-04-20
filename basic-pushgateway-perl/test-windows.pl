use Pushgateway::TinyActiveState;

my $job_name      = 'my_custom_metrics';
my $instance_name = $ENV{'COMPUTERNAME'};
my $team          = 'test';
my $opt           = {
    'instance_name' => $instance_name,
    '-host'         => '192.168.0.64',
    '-port'         => 9091,
    '-path' =>
      "/metrics/job/${job_name}/instance/${instance_name}/team/${team}",
    '-timeout' => 10
};
my $o = Pushgateway::TinyActiveState->new(%$opt);

# NOTE: setter does not work
# $o->{'instance_name'} = $instance_name;

my $custom = {
    '-metric_name' => 'test',
    '-label'       => {},
    '-value'       => 42
};
$o->add(%$custom);

