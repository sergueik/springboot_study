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
use CGI::Tiny;
use JSON::PP;
use Digest::MD5 qw(md5_hex);

use Data::Dumper;

sub checksum_file {
    my $filepath = shift;
    my $hash;
    if ( ! -f $filepath ) {
        return 0;
    }
    else {
        {
            local $/ = undef;
            open FILE, "$filepath";
            binmode FILE;
            my $data = <FILE>;
            close FILE;
            $hash = md5_hex($data);
        }
        return $hash;
    }
}

sub check_newer {
    my ( $filepath, $check_epoch ) = @_;
    if ( ! -f $filepath ) {
        return 0;
    }
    else {
        my $stat = stat($filepath);
        return ( $stat->mtime > $check_epoch ) ? 1 : 0;
    }
}

our $json_pp = JSON::PP->new->ascii->pretty->allow_nonref;

use vars
  qw($cgi $method $data $status $content $DEBUG $dir $inputfile $newer $hash $file_hash );
$data    = {};
$status  = undef;
$content = undef;
$DEBUG   = 1;

$dir       = getcwd;
$inputfile = 'example_config.json';
$newer     = 1692049343;
$hash      = undef;
$file_hash = undef;

cgi {
    $cgi    = $_;
    $method = $cgi->method;
    print STDERR "method=$method", $/ if $DEBUG;
    $cgi->set_error_handler(
        sub {
            my ( $cgi, $error, $rendered ) = @_;
            print STDERR "in error handler: $error", $/;
            $data->{status} = 'error';
            $data->{code}   = 500;
            $data->{result} = $error;

            # NOTE: CGI:Lite uses JSON::PP internally
            $cgi->set_response_status(406)
              ->render( html => $json_pp->encode($data) );
        }
    );

    if ( $method eq 'GET' ) {
        $inputfile = $cgi->query_param('inputfile');
        $newer     = $cgi->query_param('newer');
        $hash      = $cgi->query_param('hash');
        $inputfile = $dir . '/' . $inputfile;
        print STDERR "\$inputfile=${inputfile}\n" if $DEBUG;
        if ( -e $inputfile ) {
            $status = 1;

            # evaluate: status based on file age
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
            #
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
                $cgi->render( html => $content );
            }
            else {
              # return failure through HTTP status code
              # 304  Not Modified
              # 204  No Content
              # 208  Already Reported
              #
              # NOTE: the payload will be ignored for 304, 204, will be received for 208
              # print "Content-Type: application/json\n\n", $content;
                $data->{status} = 'error';
                $data->{result}   = "Config ${inputfile} is unchanged";
                print STDERR 'Returning payload: ' . $json_pp->encode($data)
                  if $DEBUG;
                $cgi->set_response_status(304)
                  ->render( html => $json_pp->encode($data) );

              # alternative scenario is with server always returning a 200 + JSON             # but
              # when there is no change in the requested resource
              # the returned JSON to contain a different schema,
              # e.g. { "status" : "failure", "result": "unchanged" }
            }
        }
        else {
            # return the failure
            # alternative scenario is return failure in the response JSON
            $data->{status} = 'error';
            $data->{result}   = "Config ${inputfile} not found";
            $cgi->set_response_status(410)
              ->render( html => $json_pp->encode($data) );
        }

    }
    else {
        print STDERR 'Unsupported method', $/ if $DEBUG;
        $data->{status} = 'error';
        $data->{result}   = 'unsupported method';
        $cgi->set_response_status(405)
          ->render( html => $json_pp->encode($data) );
    }

}

