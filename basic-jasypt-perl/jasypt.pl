#!/usr/bin/perl

use strict;

use Getopt::Long;

BEGIN {
    use constant RELEASE => 0;
    use constant HOME    => (
        do { $_ = $ENV{HOME}; /\/([^\/]+)$/ }
    );
    use constant SCRIPT_DIR => (
        do { my $s = `dirname $0`; chomp $s; $s }
    );
    if (RELEASE) {

        # TODO: set extra lib path in RELEASE
    }
    else {
        unshift( @INC, SCRIPT_DIR );
        unshift( @INC, `pwd` );
    }
}
use Crypt::PBE qw(:jce);
use MIME::Base64;
use Getopt::Long;

my $value     = 'test';
my $password  = 'password';
my $debug     = undef;
my $operation = 'encrypt';
my $encrypted = undef;
GetOptions(
    'value=s'     => \$value,
    'password=s'  => \$password,
    'debug'       => \$debug,
    'operation=s' => \$operation,

);

# NOTE: in Crypt::PBE version 0.103+ there is CLI.pm which supports additional options
# NOTE: jasypt_csharp used MD5 and DES
# my $pbe = PBEWithMD5AndDES($password, undef, $debug);

# NOTE: pbkdf2-csharp uses System.Security.Cryptography.Rfc2898DeriveBytes.Rfc2898DeriveBytes for PBKDF which under .Net Framework 4.6 or earlir by default uses HMACSHA1
# https://learn.microsoft.com/en-us/dotnet/api/system.security.cryptography.rfc2898derivebytes?view=netframework-4.5
# my $pbe =  PBEWithHmacSHA1AndAES_256($password, undef, $debug);

# NOTE: under .Net Framework 4.8 one can configure the System.Security.Cryptography.Rfc2898DeriveBytes.Rfc2898DeriveBytes constructor to use SHA512
# NOTE: HMACSHA512AES256 is also Jasypt default
my $pbe = PBEWithHmacSHA512AndAES_256( $password, undef, $debug );

if ($debug) {
    print "password: $password$/";
    print "value: $value$/";
}
if ( $operation =~ /encrypt/ ) {
    print 'Encrypting', $/ if $debug;
    $encrypted = $pbe->encrypt( $value, 1000 );

    print encode_base64($encrypted), $/;
    print 'Decrypting', $/ if $debug;
    print $pbe->decrypt($encrypted), $/ if $debug;

}
else {
    print 'Decrypting', $/ if $debug;
    $encrypted = decode_base64($value);
    print $value, $/ if $debug;
    print $pbe->decrypt($encrypted), $/;
}
