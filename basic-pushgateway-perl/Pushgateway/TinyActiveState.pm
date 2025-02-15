package Pushgateway::TinyActiveState;

use 5.14.2;
use strict;
use warnings;
use utf8;
use Carp qw/croak carp/;

# use LWP::UserAgent;
use JSON;
use Net::HTTP;

our $VERSION = '0.02';

my %METRIC_VALID_TYPES = (
    'untyped'   => 1,
    'counter'   => 1,
    'gauge'     => 1,
    'histogram' => 1,
    'summary'   => 1,
);

sub new {
    my ( $class, %opt ) = @_;
    my $self = {};
    $self->{host} = $opt{'-host'} // croak "You must specify '-host' param";
    $self->{port} = $opt{'-port'} // croak "You must specify '-port' param";
    my $path = $opt{'-path'};
    $self->{instance_name} = $opt{instance_name};
    my $timeout = $opt{'-timeout'} // 5;
    $self->{raw_str} = undef;
    $self->{raw_data} = [];
    $self->{url}     = 'http://' . $self->{host} . ':' . $self->{port} . $path;

    return bless $self, $class;
}

sub add {
    my $self = shift;
    $self->{raw_str} = $self->_add(@_);
    return $self->_send_to_prometheus( $self->{raw_str} );
}

sub increment {
    my $self = shift;

    # NOTE: fix to pass the value - the array and hash do not merge as intended
    my %args = @_;
    my $value = $args{'-value'} || 1;
    $self->{raw_str} = $self->_add(
        @_,
        '-value' => $value,
        '-type'  => 'counter',
    );
    return $self->_send_to_prometheus( $self->{raw_str} );
}

sub summary {
    my $self = shift;
    $self->{raw_str} = $self->_add( @_, '-type' => 'summary', );
    return $self->_send_to_prometheus( $self->{raw_str} );
}

sub gauge {
    my $self = shift;
    $self->{raw_str} = $self->_add( @_, '-type' => 'gauge', );
    return $self->_send_to_prometheus( $self->{raw_str} );
}

sub histogram {
    my ( $self, %opt ) = @_;
    my $metric_name = $opt{'-metric_name'}
      // croak "You must specify '-metric_name' param";
    my $label   = $opt{'-label'}   // {};
    my $value   = $opt{'-value'}   // croak "You must specify '-value' param";
    my $buckets = $opt{'-buckets'} // croak "You must specify '-buckets' param";
    croak "Param '-buckets' must be arrayref" if ref($buckets) ne 'ARRAY';
    croak "Label must be hashref"             if ref($label) ne 'HASH';

    my @metrics;
    push @metrics, "# TYPE $metric_name histogram\n";
    push @metrics,
      $self->_prepare_raw_metric( $metric_name . '_count', $label, 1 );
    push @metrics,
      $self->_prepare_raw_metric( $metric_name . '_sum', $label, $value );

    for my $bucket (@$buckets) {
        push @metrics,
          $self->_prepare_raw_metric(
            $metric_name . '_bucket',
            { %$label, 'le' => $bucket },
            $value <= $bucket ? 1 : 0
          );
    }
    push @metrics,
      $self->_prepare_raw_metric( $metric_name . '_bucket',
        { %$label, 'le' => '+Inf' }, 1 );

    return $self->_send_to_prometheus( join( '', @metrics ) );
}

sub _add {
    my ( $self, %opt ) = @_;
    my $metric_name = $opt{'-metric_name'}
      // croak "You must specify '-metric_name' param";
    my $label = $opt{'-label'} // {};
    my $value = $opt{'-value'} // croak "You must specify '-value' param";
    my $type  = $opt{'-type'}  // 'untyped';
    $type = lc($type);

    croak "Label must be hashref" if ref($label) ne 'HASH';
    croak "Unvalid metric type: '$type'. Valid types: "
      . join( ', ', keys %METRIC_VALID_TYPES )
      if not $METRIC_VALID_TYPES{$type};

    my $type_str = "# TYPE $metric_name $type\n";

    my $raw_metric = $self->_prepare_raw_metric( $metric_name, $label, $value );

    return $type_str . $raw_metric;
}

sub _prepare_raw_metric {
    my ( $self, $metric_name, $label, $value ) = @_;
    $self->{raw_str} = $metric_name;
    if ($label) {
        $self->{raw_str} .= '{'
          . join( ', ', map { $_ . '="' . $label->{$_} . '"' } keys %$label )
          . '}';
    }
    $self->{raw_str} .= " $value\n";
    return $self->{raw_str};
}

sub _send_to_prometheus {
    my ( $self, $str ) = @_;
    push @{$self->{raw_data}}, $str;
    return if $self->{defer};
    # print STDERR "sending";

    my $s = Net::HTTP->new( Host => $self->{'host'} . ':' . $self->{'port'} )
      || die $@;

    my %headers = ();
    $s->write_request( 'POST', $self->{'url'}, %headers, $str );
    my ( $code, $mess, %h ) = $s->read_response_headers;

    return 0 if $code =~ /200/;
    croak "Can't send POST request to " . $self->{url} . ' Code: ' . $code;

}

1;

