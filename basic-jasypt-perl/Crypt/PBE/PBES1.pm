package Crypt::PBE::PBES1;

use strict;
use warnings;
use utf8;

use Carp;
use Crypt::CBC;
use Exporter qw(import);
use Data::Dumper;
use Crypt::PBE::PBKDF1;

our $VERSION = '0.102';

use constant ENCRYPTION => { 'des' => 'Crypt::DES', };

sub new {

    my ( $class, %params ) = @_;

    my $self = {
        password => $params{password} || croak('Specify password'),
        count    => $params{count}    || 1_000,
        hash     => $params{hash}     || croak('Specify hash digest algorithm'),
        encryption => $params{encryption} || 'des',   # TODO add support for RC2
        debug  => undef,
        dk_len => 0,
    };

    my $dk_len = 16;

    $self->{dk_len} = $dk_len;

    return bless $self, $class;

}

sub debug {
    my ( $self, $value ) = @_;
    $self->{debug} = $value;
}

sub encrypt {

    my ( $self, $data ) = @_;

    my $salt = Crypt::CBC->random_bytes(8);

    print STDERR 'Salt (random): ', byte_hex($salt), $/ if ( $self->{debug} );

    my $salt_string = 'fd2b12742f751d0b';

    my @hex = ( $salt_string =~ /(..)/g );
    my @dec = map { hex($_) } @hex;
    print Dumper( \@dec ) if ( $self->{debug} );
    my @bytes = map { pack( 'C', $_ ) } @dec;

    $salt = join '', @bytes;
    print STDERR 'Salt (fixed): ', byte_hex($salt), $/ if ( $self->{debug} );

    # print Dumper($salt);
    # origin: https://www.perlmonks.org/?node_id=1054876

    my $DK = pbkdf1(
        hash     => $self->{hash},
        password => $self->{password},
        salt     => $salt,
        count    => $self->{count},
        dk_len   => $self->{dl_len}
    );

    # TODO: add debug property
    print STDERR 'DK: ', byte_hex($DK), $/ if ( $self->{debug} );
    my $key = substr( $DK, 0, 8 );
    my $iv  = substr( $DK, 8, 8 );

    my $crypt = Crypt::CBC->new(
        -key         => $key,
        -iv          => $iv,
        -literal_key => 1,
        -header      => 'none',
        -cipher      => 'Crypt::DES',
    );

    my $encrypted = $crypt->encrypt($data);
    my @result = ( $salt, $encrypted );
    if ( $self->{debug} ) {
        print STDERR 'Salt: ',      byte_hex($salt),      $/;
        print STDERR 'Encrypted: ', byte_hex($encrypted), $/;
    }

    return wantarray ? @result : join( '', @result );

}

sub decrypt {

    my ( $self, $salt, $encrypted ) = @_;

    if ( !$encrypted ) {
        my $data = $salt;
        $salt = substr( $data, 0, 8 );
        $encrypted = substr( $data, 8 );
    }

    if ( $self->{debug} ) {
        print STDERR 'Salt: ',      byte_hex($salt),      $/;
        print STDERR 'Encrypted: ', byte_hex($encrypted), $/;
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
__END__
