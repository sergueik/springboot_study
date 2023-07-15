#!/usr/bin/perl

use strict;

use Getopt::Long;
use Mango;
use Data::Dumper;

print 'Connect', $/;
my $mango = Mango->new('mongodb://mongo_server:27017');
my @a = (0..10000);
print "Insert $#a documents", $/;
my $cnt = 0;
for my $cnt (@a) {
my $item = 'bar' . $cnt;
my $oid   = $mango->db('test')->collection('foo')->insert({
        item => $item,
        qty  => 100,
        tags => ["cotton"],
        size => { h => 28, w => 35.5, uom => "cm" }
    });
# print 'Object Id: ', $oid->to_string, $/;
}
print 'Find document', $/;
my $doc = $mango->db('test')->collection('foo')->find_one({item => 'bar9999'});
print Dumper($doc);
# Remove document
# $mango->db('test')->collection('foo')->remove({bar => 'yada'});

