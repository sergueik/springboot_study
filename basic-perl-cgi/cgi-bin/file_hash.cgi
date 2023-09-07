#!/usr/bin/perl
use warnings;
use strict;

use Getopt::Long;
use Cwd;
use File::stat;

BEGIN {
    use constant RELEASE => 0;
    use constant HOME    => (
        do { $_ = $ENV{HOME}; /\/([^\/]+)$/ }
    );
    if (RELEASE) {

        # TODO: set extra lib path in RELEASE
    }
    else {
        chomp( my $script_dir = `dirname $0` );
        unshift( @INC, $script_dir );
        unshift( @INC, `pwd` );
    }
}

use YAML::Tiny;
use JSON::PP;
use Digest::MD5 qw(md5_hex);

use Data::Dumper;

sub checksum_file {
    my $filename = shift;
    my $hash;
    {
        local $/ = undef;
        open FILE, "$filename";
        binmode FILE;
        my $data = <FILE>;
        close FILE;
        $hash = md5_hex($data);
    }
    $hash;
}

sub check_newer {
    my ( $filepath, $check_epoch ) = @_;
    my $stat = stat($filepath);
    return ( $stat->mtime > $check_epoch ) ? 1 : 0;
}
our $json_pp = JSON::PP->new->ascii->pretty->allow_nonref;

my $status   = undef;
my $response = '{}';
my $content  = undef;
my $DEBUG    = 1;
my $error    = undef;

# hardcode the query parameter defauts: $inputfile $newer
my $dir       = getcwd;
my $inputfile = 'example_config.json';
my $newer     = undef;

#  $newer     = 1692089343;
my $hash = undef;

#  $hash      = '9f8377db38593544a5e994006fe4e9e4';
my $file_hash = undef;

# process QUERY_STRING directly without relying on CGI::Tiny
my $query        = {};
my $query_string = $ENV{'QUERY_STRING'};
my @pairs        = split( '&', $query_string );
foreach my $pair (@pairs) {
    my ( $name, $value ) = split( '=', $pair );
    $query->{$name} = $value;
}
if ( exists $query->{newer} ) {
    $newer = $query->{newer};
}
if ( exists $query->{inputfile} ) {
    $inputfile = $query->{inputfile};
}

if ( exists $query->{hash} ) {
    $hash = $query->{hash};
}

$inputfile = $dir . '/' . $inputfile;
print STDERR "\$inputfile=${inputfile}\n" if $DEBUG;
if ( -e $inputfile ) {

    $status = 1;

    # evaluate: status based on file mtime
    if ($newer) {
        my $check_timestamp = localtime($newer);
        $status = &check_newer( $inputfile, $newer );
    }

    # override the status with file content md5 hash check
    if ($hash) {
        $file_hash = &checksum_file($inputfile);
        if ( $file_hash cmp $hash ) {
            $status = 1;
        }
        else {

            $status = 0;
        }
    }
    print STDERR (
        "\$newer = ${newer}",
        "\$hash = ${hash}",
        "\$file_hash = ${file_hash}",
        "\$status = ${status}\n"
    ) if $DEBUG;
    if ($status) {
        if ($inputfile) {
            $content = '';
            open( FH, '<', $inputfile ) or die $!;
            while (<FH>) {
                $content .= $_;
            }
            close(FH);
        }

        if ($DEBUG) {
            print STDERR Dumper($content);
        }

    }
    else {
        # return the failure
        # alternative scenario is return failure through HTTP status code
        # 304  Not Modified
        # 204  No Content
        # 208  Already Reported
        #
        $response = {
            status => 'error',
            result => "Config ${inputfile} is unchanged"
        };
        $content = $json_pp->encode($response);
    }
}
else {
    # return the failure
    # alternative scenario is return failure through HTTP status code
    # 404 Not Found
    $response = {
        status => 'error',
        result => "Config ${inputfile} not found"
    };
    $content = $json_pp->encode($response);
}

print "Content-Type: application/json\n\n", $content;
