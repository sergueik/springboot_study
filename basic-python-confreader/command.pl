#!/usr/bin/perl

# mock up of the filter.pl script

my $script_dir = `dirname $0`; chomp $script_dir;
my $file = $ARGV[0];
open CONF, "$script_dir/$file" or die $!;
while (<CONF>) {
  print $_
}
