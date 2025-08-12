#!/usr/bin/perl
my$c="".localtime()."\n";
print "Content-Type: text/plain\n";
print "Content-Length: ".length($c)."\n";
print "\n";
print $c;
