#!/usr/bin/perl
use FCGI;
my $count = 0;
my $req = FCGI::Request();
while($req->Accept() >= 0) {
  my $env=$ENV=$req->GetEnvironment();
  my $content;
  eval{
    $content = ++$count."   ".localtime()."\n";
    #...do stuff...
  };
  if(    $@=~/tegn på serverfeil, feks base-trøbbel/ ){ print "Status: 500 Internal Server Error\r\n" }
  elsif( $@=~/tegn på klientfeil, feks gal input/    ){ print "Status: 400 Bad Request\r\n" }
#  print "Content-Encoding: gzip\r\n" if $$env{HTTP_ACCEPT_ENCODING}=~/\b gzip \b/x and $content=gzip($content);
  print "Content-Type: text/plain\r\n";
  print "Content-Length: ".length($content)."\r\n\r\n";
  print $content;
  $req->Flush();
  #$req->Finish();
}
