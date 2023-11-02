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

# use CGI::Tiny;
# TODO:
# cgi exited without rendering a response
# Status: 500 Internal Server Error
use Data::Dumper;
my $data = {
    'sergueik53' => {
        'PORTS' => [ 22, 443, 3306 ]
    },
    'sergueik71' => {
        'PORTS' => [5432]
    },
    'sergueik119' => {}
};

sub check_newer {
    my ( $filepath, $check_epoch ) = @_;
    my $stat = stat($filepath);
    return ( $stat->mtime > $check_epoch ) ? 1 : 0;
}
our $json_pp = JSON::PP->new->ascii->pretty->allow_nonref;

my $status   = undef;
my $response = '{}';
my $content  = undef;
my $debug    = 0;
my $error    = undef;

# hardcode the query parameter defauts: $inputfile $newer
my $dir       = getcwd;
my $inputfile = 'example_config.json';
my $newer     = 1692049343;

#   $newer     = 1692089343;

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

$inputfile = $dir . '/' . $inputfile;
my $check_timestamp = localtime($newer);
$status = &check_newer( $inputfile, $newer );
if ($status) {
    if ($inputfile) {
        $content = '';
        open( FH, '<', $inputfile ) or die $!;
        while (<FH>) {
            $content .= $_;
        }
        close(FH);
    }

    if ($debug) {
        print STDERR Dumper($content);
    }

    local $@;
    my $data = eval {
        if ($debug) {
            print STDERR Dumper($content);
        }
        my $result = $json_pp->decode($content);
        return $result;
    };
    $error = $@;

    if ($data) {
        $error = $data->{error};
    }

    if ( !$error ) {

        print STDERR Dumper($data) if $debug;
        $response = {
            status => 'OK',
            result => $data
        };
    }
    else {

        $response = {
            status => 'error',
            result => "Error reading Config ${inputfile}: ${error}"
        };
    }
}
else {
    $response = {
        status => 'error',
        result => "Config ${inputfile} is older than ${check_timestamp}"
    };
}
print "Content-Type: application/json\n\n", $json_pp->encode($response);
