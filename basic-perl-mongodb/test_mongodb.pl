#!/usr/bin/perl

use strict;

use Getopt::Long;
use MongoDB;
use Data::Dumper;

print 'Connect', $/;

my $mongodb = MongoDB::MongoClient->new( host => 'mongo_server', port => 27017 );
my $database = $mongodb->get_database('test');
my $collection = $database->get_collection('foo');
my @a = (0..10000);
my $cnt = 0;
print "Insert $#a documents", $/;
for my $cnt (@a) {
my $item = 'bar' . $cnt;
my $insert_result   = $collection->insert_one({
        item => $item,
        qty  => 100,
        tags => ["cotton"],
        size => { h => 28, w => 35.5, uom => "cm" }
    });
#   print 'Object Id: ', $insert_result->inserted_id->hex, $/;
}
print 'Find document', $/;
my $doc = $mongodb->get_database('test')->get_collection('foo')->find_one({item => 'bar9999'});
print Dumper($doc);


