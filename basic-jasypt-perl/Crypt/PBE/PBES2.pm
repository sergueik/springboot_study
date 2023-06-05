package Crypt::PBE::PBES2;

use strict;
use warnings;
use utf8;

use Carp;
use Crypt::CBC;
use Exporter qw(import);

use Crypt::PBE::PBKDF2;

use Data::Dumper;
our $VERSION = '0.103';

use constant KEY_SIZE => { 'aes-256' => 32, };

use constant ENCRYPTION => { 'aes-256' => 'Crypt::OpenSSL::AES', };

use constant HMAC =>
  qw( hmac-sha1 hmac-224 hmac-sha256 hmac-sha384 hmac-sha512 );

sub new {

    my ( $class, %params ) = @_;

    my $self = {
        password => $params{password} || croak('Specify password'),
        hmac     => $params{hmac}     || croak('Specify HMAC digest algorithm'),
        encryption => $params{encryption} || croak('Specify encryption method'),
        count      => $params{count}      || 1_000,
        dk_len     => 0,
        debug      => undef,
    };

    my $dk_len = KEY_SIZE->{ $params{encryption} };

    if ( !$dk_len ) {
        croak('Unknown encryption method');
    }

    $self->{dk_len} = $dk_len;

    return bless $self, $class;

}

sub debug {
    my ( $self, $value ) = @_;
    $self->{debug} = $value;
}

sub encrypt {

    my ( $self, $data ) = @_;

    my $salt = Crypt::CBC->random_bytes(16);
    my $iv   = Crypt::CBC->random_bytes(16);

    print STDERR 'Salt (random): ', byte_hex($salt), $/ if ( $self->{debug} );

    my $salt_string = '0432cf3a41052a6592900c2e6c0b5c79';

    my @hex = ( $salt_string =~ /(..)/g );
    my @dec = map { hex($_) } @hex;
    print STDERR Dumper( \@dec ) if ( $self->{debug} );
    my @bytes = map { pack( 'C', $_ ) } @dec;

    $salt = join '', @bytes;
    print STDERR 'Salt (fixed): ', byte_hex($salt), $/ if ( $self->{debug} );

    # print Dumper($salt);
    # origin: https://www.perlmonks.org/?node_id=1054876
    my $key = pbkdf2(
        prf      => $self->{hmac},
        password => $self->{password},
        salt     => $salt,
        dk_len   => $self->{dk_len},
        count    => $self->{count}
    );

    # TODO: add debug property
    print STDERR 'key: ', byte_hex($key), $/ if ( $self->{debug} );
    my $cipher = Crypt::CBC->new(
        -key         => $key,
        -keysize     => length($key),
        -iv          => $iv,
        -header      => 'none',
        -literal_key => 1,
        -cipher      => ENCRYPTION->{ $self->{encryption} }
    );

    my $encrypted = $cipher->encrypt($data);

    my @result = ( $salt, $iv, $encrypted );
    if ( $self->{debug} ) {
        print STDERR 'Salt: ',      byte_hex($salt),      $/;
        print STDERR 'Iv: ',        byte_hex($iv),        $/;
        print STDERR 'Encrypted: ', byte_hex($encrypted), $/;
    }

    return wantarray ? @result : join( '', @result );

}

sub decrypt {

    my ( $self, $salt, $iv, $encrypted ) = @_;

    if ( !$iv && !$encrypted ) {

        my $data = $salt;

        $salt = substr( $data, 0,  16 );
        $iv   = substr( $data, 16, 16 );
        $encrypted = substr( $data, 32 );

    }

    if ( $self->{debug} ) {
        print STDERR 'Salt: ',      byte_hex($salt),      $/;
        print STDERR 'Iv: ',        byte_hex($iv),        $/;
        print STDERR 'Encrypted: ', byte_hex($encrypted), $/;
    }
    my $key = pbkdf2(
        prf      => $self->{hmac},
        password => $self->{password},
        salt     => $salt,
        dk_len   => $self->{dk_len},
        count    => $self->{count}
    );
    print STDERR 'key: ', byte_hex($key), $/ if ( $self->{debug} );

    my $cipher = Crypt::CBC->new(
        -key         => $key,
        -keysize     => length($key),
        -iv          => $iv,
        -header      => 'none',
        -literal_key => 1,
        -cipher      => ENCRYPTION->{ $self->{encryption} }
    );

    return $cipher->decrypt($encrypted);

}

1;
__END__
