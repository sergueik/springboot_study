package InfluxDB::Client::Simple;

use 5.006;
use strict;
use warnings;

use Carp;
use IO::Socket::INET;

use JSON::PP;

use LWP::UserAgent;
use URI;
use Data::Dumper;

our $VERSION = '1.00';

sub new {
    my $class = shift;
    my %args  = (
        database => 'grafana',
        host     => 'localhost',
        port     => 8086,
        protocol => 'tcp',
        debug    => 0,
        timeout  => 180,
        @_,
    );
    my ( $host, $port, $protocol, $strict_udp, $timeout ) =
      map { defined($_) ? lc($_) : '' }
      @args{ 'host', 'port', 'protocol', 'strict_udp', 'timeout' };

    my $self = {
        host     => $host,
        port     => $port,
        protocol => $protocol,
        debug    => 0,
        options  => { database => $args{database} }
    };

    if ( $protocol eq 'tcp' ) {
        my $ua = LWP::UserAgent->new();
        $ua->agent("InfluxDB-Client-Simple/$VERSION");
        $ua->timeout($timeout);
        $self->{lwp_user_agent} = $ua;
    }
    else {
        die "Unknown protocol: $protocol" unless $protocol eq "udp";

        my $socket = IO::Socket::INET->new(
            PeerAddr => "$host:$port",
            Proto    => $protocol,
            Blocking => 0
        );

        if ($strict_udp) {
            die("Can't open socket: $@") unless $socket;
        }

        $self->{udp} = $socket;
    }

    bless $self, $class;
    return $self;
}

sub debug {
    my ( $self, $debug ) = @_;
    $self->{debug} = $debug;
}

sub ping {
    my ($self) = @_;

    # my $uri      = $self->_get_influxdb_http_api_uri('ping');
    # 'http://localhost:8086/ping'

    # my $response = $self->{lwp_user_agent}->head( $uri->canonical() );
    my $response = $self->{lwp_user_agent}
      ->head( "http://" . $self->{host} . ":" . $self->{port} . "/ping" );
    print STDERR Dumper( \$response ) if $self->{debug};
    if ( !$response->is_success() ) {
        my $error = $response->message();
        return {
            raw     => $response,
            error   => $error,
            version => undef,
        };
    }

    my $version = $response->header('X-Influxdb-Version');
    return {
        raw     => $response,
        error   => undef,
        version => $version,
    };
}

sub query {
    my $self  = shift;
    my $query = shift;

    # Odd number of elements in hash assignment
    my %args = ( epoch => 'ns', @_ );
    my ( $database, $chunk_size, $epoch ) =
      @args{ 'database', 'chunk_size', 'epoch' };

    die "Missing argument 'query'" if !$query;
    die "Argument epoch '$epoch' is not one of (h,m,s,ms,u,ns)"
      if $epoch !~ /^(h|m|s|ms|u|ns)$/;

    if ( ref($query) eq 'ARRAY' ) {
        $query = join( ';', @$query );
    }

    print STDERR Dumper( \$query ) if $self->{debug};

    my $uri = $self->_get_influxdb_http_api_uri('query');

    $uri->query_form(
        q => $query,
        ( $database   ? ( db         => $database )   : () ),
        ( $chunk_size ? ( chunk_size => $chunk_size ) : () ),
        ( $epoch      ? ( epoch      => $epoch )      : () )
    );
    print STDERR Dumper( $uri->canonical() ) if $self->{debug};

# "http://". $self->{host} . ":" . $self->{port} .  "/query?" +
# 'q=SELECT+*+FROM+testing' + '&' + 'db=' + $args{'database'} + '&'+ 'epoch=' . $args{epoch}
    my $response = $self->{lwp_user_agent}->post( $uri->canonical() );

    chomp( my $content = $response->content() );

    my $error;
    if ( $response->is_success() ) {
        our $json_pp = JSON::PP->new->ascii->pretty->allow_nonref;
        local $@;
        my $data = eval {
            print STDERR 'Raw content' . $/ if $self->{debug};
            print STDERR Dumper($content) if $self->{debug};
            my $result = $json_pp->decode($content);
            print STDERR 'Decoded content' . $/ if $self->{debug};
            print STDERR Dumper($result) if $self->{debug};
            return $result;
        };
        $error = $@;

# NOTE: seen error when calling ->decode_json() instead of ->decode()
# "malformed JSON string, neither array, object, number, string or atom, at character offset 0 (before "JSON::PP=HASH(0x1fb2...")

 # print $error;
 # arising from trouble with passing $self into the subroutine
 # It indicates that ->decode_json() is not, and ->decode () is the class method
        if ($data) {
            $error = $data->{error};
        }

        if ( !$error ) {
            $data->{request_id} = $response->header('Request-Id');

            print STDERR Dumper($data) if $self->{debug};
            return {
                raw   => $response,
                data  => $data,
                error => undef,
            };
        }
    }
    else {
        $error = $content;
    }

    return {
        raw   => $response,
        data  => undef,
        error => $error,
    };
}

sub write {
    my $self        = shift;
    my $measurement = shift;
    my %args        = ( %{ $self->{options} }, @_ );
    my ( $database, $precision, $retention_policy ) =
      @args{ 'database', 'precision', 'retention_policy' };

    die "Missing argument 'measurement'" if !$measurement;
    die "Missing argument 'database'"    if !$database;
    die "Argument precision '$precision' is set and not one of (h,m,s,ms,u,ns)"
      if $precision && $precision !~ /^(h|m|s|ms|u|ns)$/;

    if ( ref($measurement) eq 'ARRAY' ) {
        $measurement = join( "\n", @$measurement );
    }

    if ( $self->{protocol} eq 'tcp' ) {

        # my $uri = $self->_get_influxdb_http_api_uri('write');

# $uri->query_form( db => $database,
#                  ( $precision        ? ( precision => $precision )        : () ),
#                  ( $retention_policy ? ( rp        => $retention_policy ) : () )
# );

        # print STDERR Dumper( $uri->canonical() );
        # "http://${host}:${port}/write?db=${database}"
        print STDERR Dumper( { Content => $measurement } ) if $self->{debug};

# NOTE: $measurement is composed by the caller e.g.
# "${metric},host=${reporting_host},env=${environment} value=${value}"
# my $response = $self->{lwp_user_agent}->post( $uri->canonical(), Content => $measurement );
        my $response = $self->{lwp_user_agent}->post(
            "http://"
              . $self->{host} . ":"
              . $self->{port}
              . "/write?" . "db="
              . $database,
            Content => $measurement
        );

        chomp( my $content = $response->content() );

        if ( $response->code() != 204 ) {
            local $@;
            print STDERR 'Raw content:' . $/ if $self->{debug};
            print STDERR Dumper($content) if $self->{debug};
            our $json_pp = JSON::PP->new->ascii->pretty->allow_nonref;
            my $data = eval { $json_pp->decode($content) };
            print STDERR 'Decoded content:' . $/ if $self->{debug};
            print STDERR Dumper($data) if $self->{debug};
            my $error = $@;
            $error = $data->{error} if ( !$error && $data );

            return {
                raw   => $response,
                error => $error,
            };
        }

        return {
            raw   => $response,
            error => undef,
        };

    }
    else {

        # Udp send
        my $bytes = $self->{udp} ? $self->{udp}->send($measurement) : 0;

      # should be more picky here : compare $bytes with length of $measurement ?
        return {
            raw   => undef,
            error => $bytes
            ? undef
            : "Undefinded error while sending data (udp)",
        };
    }
}

sub send_data {
    my $self        = shift;
    my $measurement = shift;
    my $tags        = shift;
    my $fields      = shift;
    my $timestamp   = shift;
    my %options     = @_;

    print STDERR Dumper($fields) if $self->{debug};
    print STDERR Dumper("_line_protocol: " . _line_protocol( $measurement, $tags, $fields, $timestamp )) if $self->{debug};  

    return $self->write(
        _line_protocol( $measurement, $tags, $fields, $timestamp ), %options );

}

# 'http://localhost:8086/write'
sub _get_influxdb_http_api_uri {
    my ( $self, $endpoint ) = @_;

    die "Missing argument 'endpoint'" if !$endpoint;

    my $uri = {
        scheme => 'http',
        host   => $self->{host},
        port   => $self->{port},
        path   => $endpoint
    };
    print STDERR Dumper( \$uri ) if $self->{debug};

    #    my
    $uri = URI->new();

    $uri->scheme('http');
    $uri->host( $self->{host} );
    $uri->port( $self->{port} );
    $uri->path($endpoint);
    print STDERR Dumper( \$uri ) if $self->{debug};
    return $uri;
}

# Blatantly stolen from InfluxDB::LineProtocol
sub _format_value {
    my $k = shift;
    my $v = shift;

    if ( $v =~ /^(-?\d+)(?:i?)$/ ) {
        $v = $1 . 'i';
    }
    elsif ( $v =~ /^[Ff](?:ALSE|alse)?$/ ) {
        $v = 'FALSE';
    }
    elsif ( $v =~ /^[Tt](?:RUE|rue)?$/ ) {
        $v = 'TRUE';
    }
    elsif ( $v =~ /^-?\d+(?:\.\d+)?(?:e(?:-|\+)?\d+)?$/ ) {

        # pass it on, no mod
    }
    else {
        # string actually, but this should be quoted differently?
        $v =~ s/(["\\])/\\$1/g;
        $v = '"' . $v . '"';
    }
    return $v;
}

sub _line_protocol {
    my $measurement = shift;
    my $tags        = shift;
    my $fields      = shift;
    my $timestamp   = shift;

    # sort and encode (LineProtocol) tags
    my @tags;
    foreach my $k ( sort keys %$tags ) {
        my $v = $tags->{$k};
        next unless defined($v);
        $k =~ s/([,\s])/\\$1/g;
        $v =~ s/([,\s])/\\$1/g;

        push( @tags, $k . '=' . $v );
    }
    my $tag_string = join( ',', @tags );

    # sort and encode (LineProtocol) fields
    my @fields;
    foreach my $k ( sort keys %$fields ) {
        my $v = $fields->{$k} || '';
        my $esc_k = $k;
        $esc_k =~ s/([,\s])/\\$1/g;
        my $esc_v = _format_value( $k, $v );

        push( @fields, $esc_k . '=' . $esc_v );
    }
    my $field_string = join( ',', @fields );

    if ($timestamp) {
        return sprintf( "%s,%s %s %s",
            $measurement, $tag_string, $field_string, $timestamp );
    }
    else {
        return sprintf( "%s,%s %s", $measurement, $tag_string, $field_string );
    }
}

1;

