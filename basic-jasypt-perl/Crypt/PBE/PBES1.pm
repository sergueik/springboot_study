package Crypt::PBE::PBES1;

use strict;
use warnings;
use utf8;

use Carp;
use Crypt::CBC;
use Exporter qw(import);

use Crypt::PBE::PBKDF1;

our $VERSION = '0.102';

use constant ENCRYPTION => { 'des' => 'Crypt::DES', };

sub new {

    my ( $class, %params ) = @_;

    my $self = {
        password   => $params{password}   || croak('Specify password'),
        count      => $params{count}      || 1_000,
        hash       => $params{hash}       || croak('Specify hash digest algorithm'),
        encryption => $params{encryption} || 'des',                                    # TODO add support for RC2
        dk_len     => 0,
    };

    my $dk_len = 16;

    $self->{dk_len} = $dk_len;

    return bless $self, $class;

}

sub encrypt {

    my ( $self, $data ) = @_;

    my $salt = Crypt::CBC->random_bytes(8);
    my $salt_string = 'A21B12C212AD12FF';
    my @salt = map hex, $salt_string =~ /../g;
    # origin: https://www.perlmonks.org/?node_id=1054876

    my $DK   = pbkdf1(
        hash     => $self->{hash},
        password => $self->{password},
        salt     => $salt,
        count    => $self->{count},
        dk_len   => $self->{dl_len}
    );
   # TODO: add debug property
   print STDERR "DK:", $/, pbkdf1_hex(
        hash     => $self->{hash},
        password => $self->{password},
        salt     => $salt,
        count    => $self->{count},
        dk_len   => $self->{dl_len}
), $/;
   print STDERR "DK (2):", $/, byte_hex( $DK ), $/;
    my $key = substr( $DK, 0, 8 );
    my $iv  = substr( $DK, 8, 8 );

    my $crypt = Crypt::CBC->new(
        -key         => $key,
        -iv          => $iv,
        -literal_key => 1,
        -header      => 'none',
        -cipher      => 'Crypt::DES',
    );

    my @result = ( $salt, $crypt->encrypt($data) );

    return wantarray ? @result : join( '', @result );

}

sub decrypt {

    my ( $self, $salt, $encrypted ) = @_;

    if ( !$encrypted ) {
        my $data = $salt;
        $salt      = substr( $data, 0, 8 );
        $encrypted = substr( $data, 8 );
    }

    my $DK = pbkdf1(
        hash     => $self->{hash},
        password => $self->{password},
        salt     => $salt,
        count    => $self->{count},
        dk_len   => $self->{dl_len}
    );

    my $key = substr( $DK, 0, 8 );
    my $iv  = substr( $DK, 8, 8 );

    my $ciper = Crypt::CBC->new(
        -key         => $key,
        -iv          => $iv,
        -literal_key => 1,
        -header      => 'none',
        -cipher      => 'Crypt::DES',
    );

    my $decrypted = $ciper->decrypt($encrypted);

    return $decrypted;

}

1;
