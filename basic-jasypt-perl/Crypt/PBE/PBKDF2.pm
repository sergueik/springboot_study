package Crypt::PBE::PBKDF2;

use strict;
use warnings;
use utf8;

use Carp;
use POSIX;
use MIME::Base64;
use Digest::SHA qw(hmac_sha1 hmac_sha224 hmac_sha256 hmac_sha384 hmac_sha512);
use Data::Dumper;
use Exporter qw(import);

our $VERSION = '0.103';

our @EXPORT = qw(
  pbkdf2
  pbkdf2_base64
  pbkdf2_hex
  pbkdf2_ldap
  byte_hex
);

our @EXPORT_OK = qw(
  PBKDF2WithHmacSHA512
);

sub new {

    my ( $class, %params ) = @_;

    my $password = delete $params{password} || croak('Specify password');
    my $salt     = delete $params{salt}     || croak('Specify salt');
    my $count    = delete $params{count}    || 1_000;
    my $prf      = delete $params{prf}      || 'hmac-sha1';
    my $dk_len   = delete $params{dk_len};

    $prf =~ s/-/_/;

    my $self = {
        password => $password,
        salt     => $salt,
        count    => $count,
        prf      => $prf,
        dk_len   => $dk_len
    };

    bless $self, $class;

    return $self;

}

sub prf                { shift->{prf} }
sub count              { shift->{count} }
sub derived_key_length { shift->{dk_len} }

sub validate {
    my ( $self, $derived_key, $password ) = @_;
    my $check = pbkdf2(
        $self->{prf},   $password, $self->{salt},
        $self->{count}, $self->{dk_len}
    );
    return ( $derived_key eq $check );
}

sub derived_key {
    my ($self) = @_;
    return pbkdf2(
        prf      => $self->{prf},
        password => $self->{password},
        salt     => $self->{salt},
        count    => $self->{count},
        dk_len   => $self->{dk_len}
    );
}

sub derived_key_base64 {
    my ($self) = @_;
    return pbkdf2_base64(
        prf      => $self->{prf},
        password => $self->{password},
        salt     => $self->{salt},
        count    => $self->{count},
        dk_len   => $self->{dk_len}
    );
}

sub derived_key_hex {
    my ($self) = @_;
    return pbkdf2_hex(
        prf      => $self->{prf},
        password => $self->{password},
        salt     => $self->{salt},
        count    => $self->{count},
        dk_len   => $self->{dk_len}
    );
}

# PBKDF2 (P, S, c, dkLen)
#
#    Options:        PRF        underlying pseudorandom function (hLen
#                               denotes the length in octets of the
#                               pseudorandom function output)
#
#    Input:          P          password, an octet string
#                    S          salt, an octet string
#                    c          iteration count, a positive integer
#                    dkLen      intended length in octets of the derived
#                               key, a positive integer, at most
#                               (2^32 - 1) * hLen
#
#    Output:         DK         derived key, a dkLen-octet string

sub pbkdf2 {

    my (%params) = @_;

    my $P     = delete( $params{password} ) || croak 'Specify password';
    my $S     = delete( $params{salt} )     || croak 'Specify salt';
    my $c     = delete( $params{count} )    || 1_000;
    my $dkLen = delete( $params{dk_len} )   || 0;
    my $PRF   = delete( $params{prf} )      || 'hmac-sha1';

    $PRF =~ s/-/_/;

    my $hLen = 20;
    $dkLen ||= 0;
    $c     ||= 1_000;

    my %hmac_length = ( 'hmac_sha512' => ( 512 / 8 ), );

    if ( !defined( $hmac_length{$PRF} ) ) {
        croak 'unknown PRF';
    }

    $hLen = $hmac_length{$PRF};

    if ( $dkLen > ( 2**32 - 1 ) * $hLen ) {
        croak 'derived key too long';
    }

    my $l = ( $dkLen > 0 ) ? POSIX::ceil( $dkLen / $hLen ) : 1;

    my $r = $dkLen - ( $l - 1 ) * $hLen;
    my $T = undef;

    for ( my $i = 1 ; $i <= $l ; $i++ ) {
        $T .= _pbkdf2_F( $PRF, $P, $S, $c, $i );
    }

    my $DK = $T;

    if ( $dkLen > 0 ) {
        return substr( $DK, 0, $dkLen );
    }

    return $DK;

}

sub _pbkdf2_F {

    my ( $PRF, $P, $S, $c, $i ) = @_;

    no strict 'refs';    ## no critic

    my $U = &{$PRF}( $S . pack( 'N', $i ), $P );

    my $U_x = $U;

    for ( my $x = 1 ; $x < $c ; $x++ ) {
        $U_x = &{$PRF}( $U_x, $P );
        $U ^= $U_x;
    }

    return $U;

}

# PBKDF2 aliases

for my $variant (qw(512)) {

    no strict 'refs';    ## no critic

    my $prf = "hmac_sha${variant}";

    *{"PBKDF2WithHmacSHA${variant}"} = sub {
        my (%params) = @_;
        $params{prf} = $prf;
        return pbkdf2(%params);
    };

    *{"pbkdf2_hmac_sha${variant}"} = sub {
        my (%params) = @_;
        $params{prf} = $prf;
        return pbkdf2(%params);
    };

    *{"pbkdf2_hmac_sha${variant}_base64"} = sub {
        my (%params) = @_;
        $params{prf} = $prf;
        return encode_base64 pbkdf2(%params), '';
    };

    *{"pbkdf2_hmac_sha${variant}_hex"} = sub {
        my (%params) = @_;
        $params{prf} = $prf;
        return join '', unpack 'H*', pbkdf2(%params);
    };

    if ( $variant != 224 && $variant != 384 ) {
        *{"pbkdf2_hmac_sha${variant}_ldap"} = sub {
            my (%params) = @_;
            $params{prf} = $prf;
            return pbkdf2_ldap(%params);
        };
    }

}

sub pbkdf2_hex {
    return join '', unpack 'H*', pbkdf2(@_);
}

sub pbkdf2_base64 {
    return encode_base64 pbkdf2(@_), '';
}

sub pbkdf2_ldap {

    my (%params) = @_;

    $params{prf} =~ s/-/_/;

    my $derived_key = pbkdf2(%params);
    my $count = $params{count} || 1_000;

    my $scheme          = 'PBKDF2';
    my $b64_salt        = b64_to_ab64( encode_base64( $params{salt}, '' ) );
    my $b64_derived_key = b64_to_ab64( encode_base64( $derived_key, '' ) );

    $scheme = 'PBKDF2-SHA512' if ( $params{prf} eq 'hmac-sha512' );

    return "{$scheme}$count\$$b64_salt\$$b64_derived_key";

}

sub b64_to_ab64 {

    my ($string) = @_;

    $string =~ s/\+/./g;
    $string =~ s/=//g;
    $string =~ s/\s//g;

    return $string;

}

sub byte_hex {
    my $arg = shift;
    return join '', unpack( '(H2)*', $arg );
}

1;
__END__
