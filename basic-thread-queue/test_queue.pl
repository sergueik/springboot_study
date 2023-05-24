# origin: https://www.perlmonks.org/?node_id=1068673
#!/usr/bin/perl

use strict;
use warnings;

use Getopt::Long;
use Data::Dumper;
use threads;

use Thread::Queue;

my $num   = 5;
my $cnt   = 10;
my $inputfile = undef;
my $debug      = 0;
my $servers    = 0;

GetOptions(
    'input=s' => \$inputfile,
    'debug'    => \$debug,
    'num=s'    => \$num,
    'cnt=s'    => \$cnt,
    'servers=s'  => \$servers
);

# NOTE: alternatively, ($inputfile,$inputfile,) = @ARGV;
if ($debug) {
    print "inputfile = $inputfile" . $/;
    print "debug = $debug" . $/;
    print "servers = $servers" . $/;
}

my $process_q = Thread::Queue->new();
my $failed_q  = Thread::Queue->new();

# this worker subroutine will be runs 'as a thread'.
# like with fork, when it starts, it inherits the program state 'as is':
# my variable declarations all apply - but changes to
# values within the program are 'thread local' unless the
# variable is defined as 'shared'.
# Behind the scenes - Thread::Queue are 'shared' arrays.

sub worker {

    # NOTE - runs indefinite loop,
    # need you close the queue.
    # using $process_q -> end
    while ( my $server = $process_q->dequeue() ) {
        chomp($server);
        print threads->self()->tid() . ": pinging $server\n";
        my $result = `/bin/ping -c $cnt $server`;
        if ($?) { $failed_q->enqueue($server) }
        print $result;
    }
}

#insert tasks into thread queue.
open( my $input_fh, "<", "$inputfile" ) or die $!;
$process_q->enqueue(<$input_fh>);
close($input_fh);

#we 'end' process_q  - when we do, no more items may be inserted,
#and 'dequeue' returns 'undefined' when the queue is emptied.
#this means our worker threads (in their 'while' loop) will then exit.
$process_q->end();

#  start some threads
for ( 1 .. $num ) {
    threads->create( \&worker );
}

#Wait for threads to all finish processing.
foreach my $thr ( threads->list() ) {
    $thr->join();
}

#collate results. ('synchronise' operation)
while ( my $server = $failed_q->dequeue_nb() ) {
    print "$server failed to ping\n";
}

