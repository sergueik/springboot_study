package Crypt::PBE;

use strict;
no strict 'refs';
use warnings;
use utf8;

use Carp;

use Crypt::PBE::PBES1;
use Crypt::PBE::PBES2;
use Data::Dumper;

our $VERSION = '0.102';

use Exporter qw(import);

# trimmed for debugging. Keeping two default arguments
# matching two releases of
my @JCE_PBE_ALGORITHMS = qw(

  PBEWithMD5AndDES
  PBEWithHmacSHA512AndAES_256

);

our @EXPORT_OK = @JCE_PBE_ALGORITHMS;

our %EXPORT_TAGS = ( 'jce' => \@JCE_PBE_ALGORITHMS );

# JCE algorithm PBEWith<digest>And<encryption>

my $pbes1_map =
  { 'PBEWithMD5AndDES' => { hash => 'md5', encryption => 'des' }, };
my $pbes2_map =
  { 'PBEWithHmacSHA512AndAES_256' =>
      { hmac => 'hmac-sha512', encryption => 'aes-256' }, };

print STDERR Dumper( \$pbes1_map );
print STDERR Dumper( \$pbes2_map );

# PBES1 + PBDKF1

foreach ( keys %{$pbes1_map} ) {

    my $sub_name = $_;
    my $params   = $pbes1_map->{$sub_name};

    print STDERR $sub_name, $/;
    *{$sub_name} = sub {
        my ( $password, $count, $debug ) = @_;
        my $o = Crypt::PBE::PBES1->new(
            password   => $password,
            count      => $count || 1_000,
            hash       => $params->{hash},
            encryption => $params->{encryption},
        );
        $o->debug($debug);
        return $o;
    };

}

# PBES2 + PBDKF2

foreach ( keys %{$pbes2_map} ) {

    my $sub_name = $_;
    my $params   = $pbes2_map->{$sub_name};
    print STDERR $sub_name, $/;

    *{$sub_name} = sub {
        my ( $password, $count, $debug ) = @_;
        my $o = Crypt::PBE::PBES2->new(
            password   => $password,
            count      => ( $count || 1_000 ),
            hmac       => $params->{hmac},
            encryption => $params->{encryption},
        );
        $o->debug($debug);
        return $o;
    };

}
1;
__END__
