package Crypt::PBE;

use strict;
use warnings;
use utf8;

use Carp;

use Crypt::PBE::PBES1;

our $VERSION = '0.102';

use Exporter qw(import);

# trimmed for debugging
my @JCE_PBE_ALGORITHMS = qw(

  PBEWithMD5AndDES

);

our @EXPORT_OK = @JCE_PBE_ALGORITHMS;

our %EXPORT_TAGS = ( 'jce' => \@JCE_PBE_ALGORITHMS );

# JCE algorithm PBEWith<digest>And<encryption>

my $pbes1_map =
  { 'PBEWithMD5AndDES' => { hash => 'md5', encryption => 'des' }, };

# PBES1 + PBDKF1

foreach ( keys %{$pbes1_map} ) {

    my $params   = $pbes1_map->{$_};
    my $sub_name = $_;

    no strict 'refs';    ## no critic

    *{$sub_name} = sub {
        my ( $password, $count, $debug ) = @_;
        my $pbes1 = Crypt::PBE::PBES1->new(
            password   => $password,
            count      => $count || 1_000,
            hash       => $params->{hash},
            encryption => $params->{encryption},
        );
        $pbes1->debug($debug);
        return $pbes1;
    };

}
1;
