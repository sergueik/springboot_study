package InfluxDB::Client::SimpleAlpine;

use 5.006;
use strict;
use warnings;

use Carp;
use IO::Socket::INET;

use JSON::PP;
use HTTP::Tiny;

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

        # TODO: add timeout headers
        my $agent = HTTP::Tiny->new("InfluxDB-Client-Simple/$VERSION");
        $self->{agent} = $agent;
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
    my $response =
      $self->{agent}->request( 'HEAD',
        'http://' . $self->{host} . ':' . $self->{port} . '/ping' );

    #    my $response = $self->{lwp_user_agent}
    #      ->head( 'http://' . $self->{host} . ':' . $self->{port} . '/ping' );
    if ( !$response->{success} ) {
        my $error = $response->{reason};
        return {
            raw     => $response,
            error   => $error,
            version => undef,
        };
    }
    print STDERR Dumper $response->{headers} if $self->{debug};
    my $version = $response->{headers}->{'x-influxdb-version'};
    print STDERR "VERSION: ${version}". $/ if $self->{debug};
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
    print STDERR Dumper( \$query );

    my $uri = $self->_get_influxdb_http_api_uri('query');

    $uri->query_form(
        q => $query,
        ( $database   ? ( db         => $database )   : () ),
        ( $chunk_size ? ( chunk_size => $chunk_size ) : () ),
        ( $epoch      ? ( epoch      => $epoch )      : () )
    );
    print STDERR Dumper( $uri->canonical() ) if $self->{debug};

# 'http://'. $self->{host} . ':' . $self->{port} .  '/query?' + 'q=SELECT+*+FROM+testing' + '&' + 'db=' + $args{'database'} + '&'+ 'epoch=' . $args{epoch}
# my $response = $self->{lwp_user_agent}->post( $uri->canonical() );

    my $response = $self->{agent}->request( 'POST', $uri->canonical(), {} );

    chomp( my $content = $response->{content} );

    my $error;
    if ( $response->{success} ) {
        our $json_pp = JSON::PP->new->ascii->pretty->allow_nonref;
        local $@;
        my $data = eval {
            if ( $self->{debug} ) {
                print STDERR 'processing response content' . $/;
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
            $data->{request_id} = $response->{headers}->{'request-id'};

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

        # TODO: use original code
        #
        # $uri->query_form(
        #     db => $database,
        #     ( $precision        ? ( precision => $precision )        : () ),
        #     ( $retention_policy ? ( rp        => $retention_policy ) : () )
        # );

        # print STDERR Dumper( $uri->canonical() );
        # "http://${host}:${port}/write?db=${database}"
        print STDERR Dumper( { Content => $measurement } ) if $self->{debug};

# NOTE: $measurement is composed by the caller e.g.
# "${metric},host=${reporting_host},env=${environment} value=${value}"
# my $response = $self->{lwp_user_agent}->post( $uri->canonical(), Content => $measurement );
        my $response = $self->{agent}->request(
            'POST',
            'http://'
              . $self->{host} . ':'
              . $self->{port}
              . '/write?' . 'db='
              . $database,

            , { 'content' => $measurement }
        );

        chomp( my $content = $response->{content} );

        if ( $response->{status} != 204 ) {
            local $@;
            print STDERR Dumper($content) if $self->{debug};
            our $json_pp = JSON::PP->new->ascii->pretty->allow_nonref;
            my $data = eval { $json_pp->decode($content) };
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
            : 'Undefinded error while sending data (udp)',
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

    return $self->write(
        _line_protocol( $measurement, $tags, $fields, $timestamp ), %options );

}

# 'http://localhost:8086/write'
sub _get_influxdb_http_api_uri {
    my ( $self, $endpoint ) = @_;

    die 'Missing argument "endpoint"' if !$endpoint;

    my $uri = URI->new();

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
        return sprintf( '%s,%s %s %s',
            $measurement, $tag_string, $field_string, $timestamp );
    }
    else {
        return sprintf( '%s,%s %s', $measurement, $tag_string, $field_string );
    }
}

1;

