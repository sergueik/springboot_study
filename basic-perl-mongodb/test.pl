#!/usr/bin/perl

# no strict;

use Getopt::Long;
use Mango;
use Data::Dumper;

print 'Connect', $/;
my $mango = Mango->new('mongodb://mongo_server:27017');
print 'Insert document', $/;
my @a = (0..1000);
my $cnt = 0;
for my $cnt (@a) {
$item = 'bar' . $cnt;
my $oid   = $mango->db('test')->collection('foo')->insert({
        item => $item,
        qty  => 100,
        tags => ["cotton"],
        size => { h => 28, w => 35.5, uom => "cm" }
    });
print 'Object Id: ', $oid->to_string, $/;
}
print 'Find document', $/;
my $doc = $mango->db('test')->collection('foo')->find_one({item => 'bar0'});
print Dumper($doc);
# Remove document
# $mango->db('test')->collection('foo')->remove({bar => 'yada'});

