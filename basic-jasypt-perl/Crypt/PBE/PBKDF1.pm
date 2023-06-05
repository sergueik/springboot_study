package Crypt::PBE::PBKDF1;

use strict;
use warnings;
use utf8;

use Carp;
use MIME::Base64;
use Digest::MD5 qw(md5);
use Exporter qw(import);
use Data::Dumper;
our $VERSION = '0.102';

our @EXPORT = qw(
  pbkdf1
  pbkdf1_base64
  pbkdf1_hex
  byte_hex
);

our @EXPORT_OK = qw(

  pbkdf1_md5
  pbkdf1_md5_base64
  pbkdf1_md5_hex

);

sub new {

    my ( $class, %params ) = @_;

    my $password = delete $params{password} || croak 'Specify password';
    my $salt     = delete $params{salt}     || croak 'Specify salt';
    my $count    = delete $params{count}    || 1_000;
    my $hash     = delete $params{hash}     || 'md5';
    my $dk_len   = 16;
    my $self     = {
        password => $password,
        salt     => $salt,
        count    => $count,
        hash     => $hash,
        dk_len   => $dk_len
    };

    bless $self, $class;

    return $self;

}

sub hash_algorithm     { shift->{hash} }
sub count              { shift->{count} }
sub derived_key_length { shift->{dk_len} }

sub derived_key {
    my ($self) = @_;
    return pbkdf1(
        password => $self->{password},
        salt     => $self->{salt},
        count    => $self->{count},
        hash     => $self->{hash},
        dk_len   => $self->{dk_len}
    );
}

sub derived_key_base64 {
    my ($self) = @_;
    return pbkdf1_base64(
        password => $self->{password},
        salt     => $self->{salt},
        count    => $self->{count},
        hash     => $self->{hash},
        dk_len   => $self->{dk_len}
    );
}

sub derived_key_hex {
    my ($self) = @_;
    return pbkdf1_hex(
        password => $self->{password},
        salt     => $self->{salt},
        count    => $self->{count},
        hash     => $self->{hash},
        dk_len   => $self->{dk_len}
    );
}

# PBKDF1 (P, S, c, dkLen)
#
#   Options:        Hash       underlying hash function
#
#   Input:          P          password, an octet string
#                   S          salt, an octet string
#                   c          iteration count, a positive integer
#                   dkLen      intended length in octets of derived key,
#                              a positive integer, at most 16 for MD2 or
#                              MD5 and 20 for SHA-1
#   Output:         DK         derived key, a dkLen-octet string

sub pbkdf1 {

    my (%params) = @_;

    my $hash = delete( $params{hash} )     || 'md5';
    my $P    = delete( $params{password} ) || croak('Specify password');
    my $S    = delete( $params{salt} )     || croak('Specify salt');
    my $c    = delete( $params{count} )    || 1_000;
    my $dkLen = delete( $params{dk_len} );

    if ( $hash ne 'md5' ) {
        croak 'unknown hash function';
    }

    if ( !$dkLen ) {
        $dkLen = 16;
    }

    if ( $hash eq 'md5' && $dkLen > 16 ) {
        croak 'derived key too long';
    }

    my $T = $P . $S;

    no strict 'refs';    ## no critic

    for ( 1 .. $c ) {
        $T = &{$hash}($T);
    }

    my $DK = substr( $T, 0, $dkLen );

    return $DK;

}

sub pbkdf1_hex {
    return byte_hex( pbkdf1(@_) );
}

sub pbkdf1_base64 {
    return encode_base64 pbkdf1(@_), '';
}

sub byte_hex {
    my $arg = shift;
    return join '', unpack( '(H2)*', $arg );
}

for my $digest (qw/ md5/) {

    my $dk_len = 16;

    my $sub_name = 'pbkdf1_' . $digest;

    no strict 'refs';    ## no critic

    *{$sub_name} = sub {
        my (%params) = @_;
        $params{hash}   = $digest;
        $params{dk_len} = $dk_len;
        return pbkdf1(%params);
    };

    *{ $sub_name . '_base64' } = sub {
        my (%params) = @_;
        $params{hash}   = $digest;
        $params{dk_len} = $dk_len;
        return encode_base64 pbkdf1(%params), '';
    };

    *{ $sub_name . '_hex' } = sub {
        my (%params) = @_;
        $params{hash}   = $digest;
        $params{dk_len} = $dk_len;
        return join '', unpack '(H2)*', pbkdf1(%params);
    };

}

1;
