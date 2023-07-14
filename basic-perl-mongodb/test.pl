#!/usr/bin/perl

no strict;

use Getopt::Long;

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
use Mango;
use JSON::PP;
use Data::Dumper;

print 'Connect', $/;
my $mango = Mango->new('mongodb://mongo_server:27017');
print 'Insert document', $/;
my $oid   = $mango->db('test')->collection('foo')->insert({bar => 'baz'});
print 'Object Id: ', $oid->to_string, $/;

print 'Update document', $/;
$mango->db('test')->collection('foo')
  ->update({bar => 'baz'}, {bar => 'yada'});

print 'Find document', $/;
my $doc = $mango->db('test')->collection('foo')->find_one({bar => 'yada'});
our $json_pp = JSON::PP->new->ascii->pretty->allow_nonref->allow_blessed;
print 'JSON::PP', $/;
eval {print $json_pp->encode($doc)};
# NOTE:  encountered object '64b1c22b54f5860018cc9add', 
# but neither allow_blessed, convert_blessed nor allow_tags settings are enabled (or TO_JSON/FREEZE method missing)
print 'Data::Dumper', $/;
print Dumper($doc);
# Remove document
# $mango->db('test')->collection('foo')->remove({bar => 'yada'});

