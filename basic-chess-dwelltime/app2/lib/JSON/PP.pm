package JSON::PP;
# origin: https://metacpan.org/release/JSON-PP/source/lib/JSON/PP.pm 
# JSON-2.0
 
use 5.005;
use strict;
 
use Exporter ();
BEGIN { @JSON::PP::ISA = ('Exporter') }
 
use overload ();
use JSON::PP::Boolean;
 
use Carp ();
#use Devel::Peek;
 
$JSON::PP::VERSION = '4.06';
 
@JSON::PP::EXPORT = qw(encode_json decode_json from_json to_json);
 
# instead of hash-access, i tried index-access for speed.
# but this method is not faster than what i expected. so it will be changed.
 
use constant P_ASCII                => 0;
use constant P_LATIN1               => 1;
use constant P_UTF8                 => 2;
use constant P_INDENT               => 3;
use constant P_CANONICAL            => 4;
use constant P_SPACE_BEFORE         => 5;
use constant P_SPACE_AFTER          => 6;
use constant P_ALLOW_NONREF         => 7;
use constant P_SHRINK               => 8;
use constant P_ALLOW_BLESSED        => 9;
use constant P_CONVERT_BLESSED      => 10;
use constant P_RELAXED              => 11;
 
use constant P_LOOSE                => 12;
use constant P_ALLOW_BIGNUM         => 13;
use constant P_ALLOW_BAREKEY        => 14;
use constant P_ALLOW_SINGLEQUOTE    => 15;
use constant P_ESCAPE_SLASH         => 16;
use constant P_AS_NONBLESSED        => 17;
 
use constant P_ALLOW_UNKNOWN        => 18;
use constant P_ALLOW_TAGS           => 19;
 
use constant OLD_PERL => $] < 5.008 ? 1 : 0;
use constant USE_B => $ENV{PERL_JSON_PP_USE_B} || 0;
 
BEGIN {
    if (USE_B) {
        require B;
    }
}
 
BEGIN {
    my @xs_compati_bit_properties = qw(
            latin1 ascii utf8 indent canonical space_before space_after allow_nonref shrink
            allow_blessed convert_blessed relaxed allow_unknown
            allow_tags
    );
    my @pp_bit_properties = qw(
            allow_singlequote allow_bignum loose
            allow_barekey escape_slash as_nonblessed
    );
 
    # Perl version check, Unicode handling is enabled?
    # Helper module sets @JSON::PP::_properties.
    if ( OLD_PERL ) {
        my $helper = $] >= 5.006 ? 'JSON::PP::Compat5006' : 'JSON::PP::Compat5005';
        eval qq| require $helper |;
        if ($@) { Carp::croak $@; }
    }
 
    for my $name (@xs_compati_bit_properties, @pp_bit_properties) {
        my $property_id = 'P_' . uc($name);
 
        eval qq/
            sub $name {
                my \$enable = defined \$_[1] ? \$_[1] : 1;
 
                if (\$enable) {
                    \$_[0]->{PROPS}->[$property_id] = 1;
                }
                else {
                    \$_[0]->{PROPS}->[$property_id] = 0;
                }
 
                \$_[0];
            }
 
            sub get_$name {
                \$_[0]->{PROPS}->[$property_id] ? 1 : '';
            }
        /;
    }
 
}
 
 
 
# Functions
 
my $JSON; # cache
 
sub encode_json ($) { # encode
    ($JSON ||= __PACKAGE__->new->utf8)->encode(@_);
}
 
 
sub decode_json { # decode
    ($JSON ||= __PACKAGE__->new->utf8)->decode(@_);
}
 
# Obsoleted
 
sub to_json($) {
   Carp::croak ("JSON::PP::to_json has been renamed to encode_json.");
}
 
 
sub from_json($) {
   Carp::croak ("JSON::PP::from_json has been renamed to decode_json.");
}
 
 
# Methods
 
sub new {
    my $class = shift;
    my $self  = {
        max_depth   => 512,
        max_size    => 0,
        indent_length => 3,
    };
 
    $self->{PROPS}[P_ALLOW_NONREF] = 1;
 
    bless $self, $class;
}
 
 
sub encode {
    return $_[0]->PP_encode_json($_[1]);
}
 
 
sub decode {
    return $_[0]->PP_decode_json($_[1], 0x00000000);
}
 
 
sub decode_prefix {
    return $_[0]->PP_decode_json($_[1], 0x00000001);
}
 
 
# accessor
 
 
# pretty printing
 
sub pretty {
    my ($self, $v) = @_;
    my $enable = defined $v ? $v : 1;
 
    if ($enable) { # indent_length(3) for JSON::XS compatibility
        $self->indent(1)->space_before(1)->space_after(1);
    }
    else {
        $self->indent(0)->space_before(0)->space_after(0);
    }
 
    $self;
}
 
# etc
 
sub max_depth {
    my $max  = defined $_[1] ? $_[1] : 0x80000000;
    $_[0]->{max_depth} = $max;
    $_[0];
}
 
 
sub get_max_depth { $_[0]->{max_depth}; }
 
 
sub max_size {
    my $max  = defined $_[1] ? $_[1] : 0;
    $_[0]->{max_size} = $max;
    $_[0];
}
 
 
sub get_max_size { $_[0]->{max_size}; }
 
sub boolean_values {
    my $self = shift;
    if (@_) {
        my ($false, $true) = @_;
        $self->{false} = $false;
        $self->{true} = $true;
    } else {
        delete $self->{false};
        delete $self->{true};
    }
    return $self;
}
 
sub get_boolean_values {
    my $self = shift;
    if (exists $self->{true} and exists $self->{false}) {
        return @$self{qw/false true/};
    }
    return;
}
 
sub filter_json_object {
    if (defined $_[1] and ref $_[1] eq 'CODE') {
        $_[0]->{cb_object} = $_[1];
    } else {
        delete $_[0]->{cb_object};
    }
    $_[0]->{F_HOOK} = ($_[0]->{cb_object} or $_[0]->{cb_sk_object}) ? 1 : 0;
    $_[0];
}
 
sub filter_json_single_key_object {
    if (@_ == 1 or @_ > 3) {
        Carp::croak("Usage: JSON::PP::filter_json_single_key_object(self, key, callback = undef)");
    }
    if (defined $_[2] and ref $_[2] eq 'CODE') {
        $_[0]->{cb_sk_object}->{$_[1]} = $_[2];
    } else {
        delete $_[0]->{cb_sk_object}->{$_[1]};
        delete $_[0]->{cb_sk_object} unless %{$_[0]->{cb_sk_object} || {}};
    }
    $_[0]->{F_HOOK} = ($_[0]->{cb_object} or $_[0]->{cb_sk_object}) ? 1 : 0;
    $_[0];
}
 
sub indent_length {
    if (!defined $_[1] or $_[1] > 15 or $_[1] < 0) {
        Carp::carp "The acceptable range of indent_length() is 0 to 15.";
    }
    else {
        $_[0]->{indent_length} = $_[1];
    }
    $_[0];
}
 
sub get_indent_length {
    $_[0]->{indent_length};
}
 
sub sort_by {
    $_[0]->{sort_by} = defined $_[1] ? $_[1] : 1;
    $_[0];
}
 
sub allow_bigint {
    Carp::carp("allow_bigint() is obsoleted. use allow_bignum() instead.");
    $_[0]->allow_bignum;
}
 
###############################
 
###
### Perl => JSON
###
 
 
{ # Convert
 
    my $max_depth;
    my $indent;
    my $ascii;
    my $latin1;
    my $utf8;
    my $space_before;
    my $space_after;
    my $canonical;
    my $allow_blessed;
    my $convert_blessed;
 
    my $indent_length;
    my $escape_slash;
    my $bignum;
    my $as_nonblessed;
    my $allow_tags;
 
    my $depth;
    my $indent_count;
    my $keysort;
 
 
    sub PP_encode_json {
        my $self = shift;
        my $obj  = shift;
 
        $indent_count = 0;
        $depth        = 0;
 
        my $props = $self->{PROPS};
 
        ($ascii, $latin1, $utf8, $indent, $canonical, $space_before, $space_after, $allow_blessed,
            $convert_blessed, $escape_slash, $bignum, $as_nonblessed, $allow_tags)
         = @{$props}[P_ASCII .. P_SPACE_AFTER, P_ALLOW_BLESSED, P_CONVERT_BLESSED,
                    P_ESCAPE_SLASH, P_ALLOW_BIGNUM, P_AS_NONBLESSED, P_ALLOW_TAGS];
 
        ($max_depth, $indent_length) = @{$self}{qw/max_depth indent_length/};
 
        $keysort = $canonical ? sub { $a cmp $b } : undef;
 
        if ($self->{sort_by}) {
            $keysort = ref($self->{sort_by}) eq 'CODE' ? $self->{sort_by}
                     : $self->{sort_by} =~ /\D+/       ? $self->{sort_by}
                     : sub { $a cmp $b };
        }
 
        encode_error("hash- or arrayref expected (not a simple scalar, use allow_nonref to allow this)")
             if(!ref $obj and !$props->[ P_ALLOW_NONREF ]);
 
        my $str  = $self->object_to_json($obj);
 
        $str .= "\n" if ( $indent ); # JSON::XS 2.26 compatible
 
        unless ($ascii or $latin1 or $utf8) {
            utf8::upgrade($str);
        }
 
        if ($props->[ P_SHRINK ]) {
            utf8::downgrade($str, 1);
        }
 
        return $str;
    }
 
 
    sub object_to_json {
        my ($self, $obj) = @_;
        my $type = ref($obj);
 
        if($type eq 'HASH'){
            return $self->hash_to_json($obj);
        }
        elsif($type eq 'ARRAY'){
            return $self->array_to_json($obj);
        }
        elsif ($type) { # blessed object?
            if (blessed($obj)) {
 
                return $self->value_to_json($obj) if ( $obj->isa('JSON::PP::Boolean') );
 
                if ( $allow_tags and $obj->can('FREEZE') ) {
                    my $obj_class = ref $obj || $obj;
                    $obj = bless $obj, $obj_class;
                    my @results = $obj->FREEZE('JSON');
                    if ( @results and ref $results[0] ) {
                        if ( refaddr( $obj ) eq refaddr( $results[0] ) ) {
                            encode_error( sprintf(
                                "%s::FREEZE method returned same object as was passed instead of a new one",
                                ref $obj
                            ) );
                        }
                    }
                    return '("'.$obj_class.'")['.join(',', @results).']';
                }
 
                if ( $convert_blessed and $obj->can('TO_JSON') ) {
                    my $result = $obj->TO_JSON();
                    if ( defined $result and ref( $result ) ) {
                        if ( refaddr( $obj ) eq refaddr( $result ) ) {
                            encode_error( sprintf(
                                "%s::TO_JSON method returned same object as was passed instead of a new one",
                                ref $obj
                            ) );
                        }
                    }
 
                    return $self->object_to_json( $result );
                }
 
                return "$obj" if ( $bignum and _is_bignum($obj) );
 
                if ($allow_blessed) {
                    return $self->blessed_to_json($obj) if ($as_nonblessed); # will be removed.
                    return 'null';
                }
                encode_error( sprintf("encountered object '%s', but neither allow_blessed, convert_blessed nor allow_tags settings are enabled (or TO_JSON/FREEZE method missing)", $obj)
                );
            }
            else {
                return $self->value_to_json($obj);
            }
        }
        else{
            return $self->value_to_json($obj);
        }
    }
 
 
    sub hash_to_json {
        my ($self, $obj) = @_;
        my @res;
 
        encode_error("json text or perl structure exceeds maximum nesting level (max_depth set too low?)")
                                         if (++$depth > $max_depth);
 
        my ($pre, $post) = $indent ? $self->_up_indent() : ('', '');
        my $del = ($space_before ? ' ' : '') . ':' . ($space_after ? ' ' : '');
 
        for my $k ( _sort( $obj ) ) {
            if ( OLD_PERL ) { utf8::decode($k) } # key for Perl 5.6 / be optimized
            push @res, $self->string_to_json( $k )
                          .  $del
                          . ( ref $obj->{$k} ? $self->object_to_json( $obj->{$k} ) : $self->value_to_json( $obj->{$k} ) );
        }
 
        --$depth;
        $self->_down_indent() if ($indent);
 
        return '{}' unless @res;
        return '{' . $pre . join( ",$pre", @res ) . $post . '}';
    }
 
 
    sub array_to_json {
        my ($self, $obj) = @_;
        my @res;
 
        encode_error("json text or perl structure exceeds maximum nesting level (max_depth set too low?)")
                                         if (++$depth > $max_depth);
 
        my ($pre, $post) = $indent ? $self->_up_indent() : ('', '');
 
        for my $v (@$obj){
            push @res, ref($v) ? $self->object_to_json($v) : $self->value_to_json($v);
        }
 
        --$depth;
        $self->_down_indent() if ($indent);
 
        return '[]' unless @res;
        return '[' . $pre . join( ",$pre", @res ) . $post . ']';
    }
 
    sub _looks_like_number {
        my $value = shift;
        if (USE_B) {
            my $b_obj = B::svref_2object(\$value);
            my $flags = $b_obj->FLAGS;
            return 1 if $flags & ( B::SVp_IOK() | B::SVp_NOK() ) and !( $flags & B::SVp_POK() );
            return;
        } else {
            no warnings 'numeric';
            # if the utf8 flag is on, it almost certainly started as a string
            return if utf8::is_utf8($value);
            # detect numbers
            # string & "" -> ""
            # number & "" -> 0 (with warning)
            # nan and inf can detect as numbers, so check with * 0
            return unless length((my $dummy = "") & $value);
            return unless 0 + $value eq $value;
            return 1 if $value * 0 == 0;
            return -1; # inf/nan
        }
    }
 
    sub value_to_json {
        my ($self, $value) = @_;
 
        return 'null' if(!defined $value);
 
        my $type = ref($value);
 
        if (!$type) {
            if (_looks_like_number($value)) {
                return $value;
            }
            return $self->string_to_json($value);
        }
        elsif( blessed($value) and  $value->isa('JSON::PP::Boolean') ){
            return $$value == 1 ? 'true' : 'false';
        }
        else {
            if ((overload::StrVal($value) =~ /=(\w+)/)[0]) {
                return $self->value_to_json("$value");
            }
 
            if ($type eq 'SCALAR' and defined $$value) {
                return   $$value eq '1' ? 'true'
                       : $$value eq '0' ? 'false'
                       : $self->{PROPS}->[ P_ALLOW_UNKNOWN ] ? 'null'
                       : encode_error("cannot encode reference to scalar");
            }
 
            if ( $self->{PROPS}->[ P_ALLOW_UNKNOWN ] ) {
                return 'null';
            }
            else {
                if ( $type eq 'SCALAR' or $type eq 'REF' ) {
                    encode_error("cannot encode reference to scalar");
                }
                else {
                    encode_error("encountered $value, but JSON can only represent references to arrays or hashes");
                }
            }
 
        }
    }
 
 
    my %esc = (
        "\n" => '\n',
        "\r" => '\r',
        "\t" => '\t',
        "\f" => '\f',
        "\b" => '\b',
        "\"" => '\"',
        "\\" => '\\\\',
        "\'" => '\\\'',
    );
 
 
    sub string_to_json {
        my ($self, $arg) = @_;
 
        $arg =~ s/([\x22\x5c\n\r\t\f\b])/$esc{$1}/g;
        $arg =~ s/\//\\\//g if ($escape_slash);
        $arg =~ s/([\x00-\x08\x0b\x0e-\x1f])/'\\u00' . unpack('H2', $1)/eg;
 
        if ($ascii) {
            $arg = JSON_PP_encode_ascii($arg);
        }
 
        if ($latin1) {
            $arg = JSON_PP_encode_latin1($arg);
        }
 
        if ($utf8) {
            utf8::encode($arg);
        }
 
        return '"' . $arg . '"';
    }
 
 
    sub blessed_to_json {
        my $reftype = reftype($_[1]) || '';
        if ($reftype eq 'HASH') {
            return $_[0]->hash_to_json($_[1]);
        }
        elsif ($reftype eq 'ARRAY') {
            return $_[0]->array_to_json($_[1]);
        }
        else {
            return 'null';
        }
    }
 
 
    sub encode_error {
        my $error  = shift;
        Carp::croak "$error";
    }
 
 
    sub _sort {
        defined $keysort ? (sort $keysort (keys %{$_[0]})) : keys %{$_[0]};
    }
 
 
    sub _up_indent {
        my $self  = shift;
        my $space = ' ' x $indent_length;
 
        my ($pre,$post) = ('','');
 
        $post = "\n" . $space x $indent_count;
 
        $indent_count++;
 
        $pre = "\n" . $space x $indent_count;
 
        return ($pre,$post);
    }
 
 
    sub _down_indent { $indent_count--; }
 
 
    sub PP_encode_box {
        {
            depth        => $depth,
            indent_count => $indent_count,
        };
    }
 
} # Convert
 
 
sub _encode_ascii {
    join('',
        map {
            $_ <= 127 ?
                chr($_) :
            $_ <= 65535 ?
                sprintf('\u%04x', $_) : sprintf('\u%x\u%x', _encode_surrogates($_));
        } unpack('U*', $_[0])
    );
}
 
 
sub _encode_latin1 {
    join('',
        map {
            $_ <= 255 ?
                chr($_) :
            $_ <= 65535 ?
                sprintf('\u%04x', $_) : sprintf('\u%x\u%x', _encode_surrogates($_));
        } unpack('U*', $_[0])
    );
}
 
 
sub _encode_surrogates { # from perlunicode
    my $uni = $_[0] - 0x10000;
    return ($uni / 0x400 + 0xD800, $uni % 0x400 + 0xDC00);
}
 
 
sub _is_bignum {
    $_[0]->isa('Math::BigInt') or $_[0]->isa('Math::BigFloat');
}
 
 
 
#
# JSON => Perl
#
 
my $max_intsize;
 
BEGIN {
    my $checkint = 1111;
    for my $d (5..64) {
        $checkint .= 1;
        my $int   = eval qq| $checkint |;
        if ($int =~ /[eE]/) {
            $max_intsize = $d - 1;
            last;
        }
    }
}
 
{ # PARSE 
 
    my %escapes = ( #  by Jeremy Muhlich <jmuhlich [at] bitflood.org>
        b    => "\x8",
        t    => "\x9",
        n    => "\xA",
        f    => "\xC",
        r    => "\xD",
        '\\' => '\\',
        '"'  => '"',
        '/'  => '/',
    );
 
    my $text; # json data
    my $at;   # offset
    my $ch;   # first character
    my $len;  # text length (changed according to UTF8 or NON UTF8)
    # INTERNAL
    my $depth;          # nest counter
    my $encoding;       # json text encoding
    my $is_valid_utf8;  # temp variable
    my $utf8_len;       # utf8 byte length
    # FLAGS
    my $utf8;           # must be utf8
    my $max_depth;      # max nest number of objects and arrays
    my $max_size;
    my $relaxed;
    my $cb_object;
    my $cb_sk_object;
 
    my $F_HOOK;
 
    my $allow_bignum;   # using Math::BigInt/BigFloat
    my $singlequote;    # loosely quoting
    my $loose;          # 
    my $allow_barekey;  # bareKey
    my $allow_tags;
 
    my $alt_true;
    my $alt_false;
 
    sub _detect_utf_encoding {
        my $text = shift;
        my @octets = unpack('C4', $text);
        return 'unknown' unless defined $octets[3];
        return ( $octets[0] and  $octets[1]) ? 'UTF-8'
             : (!$octets[0] and  $octets[1]) ? 'UTF-16BE'
             : (!$octets[0] and !$octets[1]) ? 'UTF-32BE'
             : ( $octets[2]                ) ? 'UTF-16LE'
             : (!$octets[2]                ) ? 'UTF-32LE'
             : 'unknown';
    }
 
    sub PP_decode_json {
        my ($self, $want_offset);
 
        ($self, $text, $want_offset) = @_;
 
        ($at, $ch, $depth) = (0, '', 0);
 
        if ( !defined $text or ref $text ) {
            decode_error("malformed JSON string, neither array, object, number, string or atom");
        }
 
        my $props = $self->{PROPS};
 
        ($utf8, $relaxed, $loose, $allow_bignum, $allow_barekey, $singlequote, $allow_tags)
            = @{$props}[P_UTF8, P_RELAXED, P_LOOSE .. P_ALLOW_SINGLEQUOTE, P_ALLOW_TAGS];
 
        ($alt_true, $alt_false) = @$self{qw/true false/};
 
        if ( $utf8 ) {
            $encoding = _detect_utf_encoding($text);
            if ($encoding ne 'UTF-8' and $encoding ne 'unknown') {
                require Encode;
                Encode::from_to($text, $encoding, 'utf-8');
            } else {
                utf8::downgrade( $text, 1 ) or Carp::croak("Wide character in subroutine entry");
            }
        }
        else {
            utf8::upgrade( $text );
            utf8::encode( $text );
        }
 
        $len = length $text;
 
        ($max_depth, $max_size, $cb_object, $cb_sk_object, $F_HOOK)
             = @{$self}{qw/max_depth  max_size cb_object cb_sk_object F_HOOK/};
 
        if ($max_size > 1) {
            use bytes;
            my $bytes = length $text;
            decode_error(
                sprintf("attempted decode of JSON text of %s bytes size, but max_size is set to %s"
                    , $bytes, $max_size), 1
            ) if ($bytes > $max_size);
        }
 
        white(); # remove head white space
 
        decode_error("malformed JSON string, neither array, object, number, string or atom") unless defined $ch; # Is there a first character for JSON structure?
 
        my $result = value();
 
        if ( !$props->[ P_ALLOW_NONREF ] and !ref $result ) {
                decode_error(
                'JSON text must be an object or array (but found number, string, true, false or null,'
                       . ' use allow_nonref to allow this)', 1);
        }
 
        Carp::croak('something wrong.') if $len < $at; # we won't arrive here.
 
        my $consumed = defined $ch ? $at - 1 : $at; # consumed JSON text length
 
        white(); # remove tail white space
 
        return ( $result, $consumed ) if $want_offset; # all right if decode_prefix
 
        decode_error("garbage after JSON object") if defined $ch;
 
        $result;
    }
 
 
    sub next_chr {
        return $ch = undef if($at >= $len);
        $ch = substr($text, $at++, 1);
    }
 
 
    sub value {
        white();
        return          if(!defined $ch);
        return object() if($ch eq '{');
        return array()  if($ch eq '[');
        return tag()    if($ch eq '(');
        return string() if($ch eq '"' or ($singlequote and $ch eq "'"));
        return number() if($ch =~ /[0-9]/ or $ch eq '-');
        return word();
    }
 
    sub string {
        my $utf16;
        my $is_utf8;
 
        ($is_valid_utf8, $utf8_len) = ('', 0);
 
        my $s = ''; # basically UTF8 flag on
 
        if($ch eq '"' or ($singlequote and $ch eq "'")){
            my $boundChar = $ch;
 
            OUTER: while( defined(next_chr()) ){
 
                if($ch eq $boundChar){
                    next_chr();
 
                    if ($utf16) {
                        decode_error("missing low surrogate character in surrogate pair");
                    }
 
                    utf8::decode($s) if($is_utf8);
 
                    return $s;
                }
                elsif($ch eq '\\'){
                    next_chr();
                    if(exists $escapes{$ch}){
                        $s .= $escapes{$ch};
                    }
                    elsif($ch eq 'u'){ # UNICODE handling
                        my $u = '';
 
                        for(1..4){
                            $ch = next_chr();
                            last OUTER if($ch !~ /[0-9a-fA-F]/);
                            $u .= $ch;
                        }
 
                        # U+D800 - U+DBFF
                        if ($u =~ /^[dD][89abAB][0-9a-fA-F]{2}/) { # UTF-16 high surrogate?
                            $utf16 = $u;
                        }
                        # U+DC00 - U+DFFF
                        elsif ($u =~ /^[dD][c-fC-F][0-9a-fA-F]{2}/) { # UTF-16 low surrogate?
                            unless (defined $utf16) {
                                decode_error("missing high surrogate character in surrogate pair");
                            }
                            $is_utf8 = 1;
                            $s .= JSON_PP_decode_surrogates($utf16, $u) || next;
                            $utf16 = undef;
                        }
                        else {
                            if (defined $utf16) {
                                decode_error("surrogate pair expected");
                            }
 
                            if ( ( my $hex = hex( $u ) ) > 127 ) {
                                $is_utf8 = 1;
                                $s .= JSON_PP_decode_unicode($u) || next;
                            }
                            else {
                                $s .= chr $hex;
                            }
                        }
 
                    }
                    else{
                        unless ($loose) {
                            $at -= 2;
                            decode_error('illegal backslash escape sequence in string');
                        }
                        $s .= $ch;
                    }
                }
                else{
 
                    if ( ord $ch  > 127 ) {
                        unless( $ch = is_valid_utf8($ch) ) {
                            $at -= 1;
                            decode_error("malformed UTF-8 character in JSON string");
                        }
                        else {
                            $at += $utf8_len - 1;
                        }
 
                        $is_utf8 = 1;
                    }
 
                    if (!$loose) {
                        if ($ch =~ /[\x00-\x1f\x22\x5c]/)  { # '/' ok
                            if (!$relaxed or $ch ne "\t") {
                                $at--;
                                decode_error('invalid character encountered while parsing JSON string');
                            }
                        }
                    }
 
                    $s .= $ch;
                }
            }
        }
 
        decode_error("unexpected end of string while parsing JSON string");
    }
 
 
    sub white {
        while( defined $ch  ){
            if($ch eq '' or $ch =~ /\A[ \t\r\n]\z/){
                next_chr();
            }
            elsif($relaxed and $ch eq '/'){
                next_chr();
                if(defined $ch and $ch eq '/'){
                    1 while(defined(next_chr()) and $ch ne "\n" and $ch ne "\r");
                }
                elsif(defined $ch and $ch eq '*'){
                    next_chr();
                    while(1){
                        if(defined $ch){
                            if($ch eq '*'){
                                if(defined(next_chr()) and $ch eq '/'){
                                    next_chr();
                                    last;
                                }
                            }
                            else{
                                next_chr();
                            }
                        }
                        else{
                            decode_error("Unterminated comment");
                        }
                    }
                    next;
                }
                else{
                    $at--;
                    decode_error("malformed JSON string, neither array, object, number, string or atom");
                }
            }
            else{
                if ($relaxed and $ch eq '#') { # correctly?
                    pos($text) = $at;
                    $text =~ /\G([^\n]*(?:\r\n|\r|\n|$))/g;
                    $at = pos($text);
                    next_chr;
                    next;
                }
 
                last;
            }
        }
    }
 
 
    sub array {
        my $a  = $_[0] || []; # you can use this code to use another array ref object.
 
        decode_error('json text or perl structure exceeds maximum nesting level (max_depth set too low?)')
                                                    if (++$depth > $max_depth);
 
        next_chr();
        white();
 
        if(defined $ch and $ch eq ']'){
            --$depth;
            next_chr();
            return $a;
        }
        else {
            while(defined($ch)){
                push @$a, value();
 
                white();
 
                if (!defined $ch) {
                    last;
                }
 
                if($ch eq ']'){
                    --$depth;
                    next_chr();
                    return $a;
                }
 
                if($ch ne ','){
                    last;
                }
 
                next_chr();
                white();
 
                if ($relaxed and $ch eq ']') {
                    --$depth;
                    next_chr();
                    return $a;
                }
 
            }
        }
 
        $at-- if defined $ch and $ch ne '';
        decode_error(", or ] expected while parsing array");
    }
 
    sub tag {
        decode_error('malformed JSON string, neither array, object, number, string or atom') unless $allow_tags;
 
        next_chr();
        white();
 
        my $tag = value();
        return unless defined $tag;
        decode_error('malformed JSON string, (tag) must be a string') if ref $tag;
 
        white();
 
        if (!defined $ch or $ch ne ')') {
            decode_error(') expected after tag');
        }
 
        next_chr();
        white();
 
        my $val = value();
        return unless defined $val;
        decode_error('malformed JSON string, tag value must be an array') unless ref $val eq 'ARRAY';
 
        if (!eval { $tag->can('THAW') }) {
             decode_error('cannot decode perl-object (package does not exist)') if $@;
             decode_error('cannot decode perl-object (package does not have a THAW method)');
        }
        $tag->THAW('JSON', @$val);
    }
 
    sub object {
        my $o = $_[0] || {}; # you can use this code to use another hash ref object.
        my $k;
 
        decode_error('json text or perl structure exceeds maximum nesting level (max_depth set too low?)')
                                                if (++$depth > $max_depth);
        next_chr();
        white();
 
        if(defined $ch and $ch eq '}'){
            --$depth;
            next_chr();
            if ($F_HOOK) {
                return _json_object_hook($o);
            }
            return $o;
        }
        else {
            while (defined $ch) {
                $k = ($allow_barekey and $ch ne '"' and $ch ne "'") ? bareKey() : string();
                white();
 
                if(!defined $ch or $ch ne ':'){
                    $at--;
                    decode_error("':' expected");
                }
 
                next_chr();
                $o->{$k} = value();
                white();
 
                last if (!defined $ch);
 
                if($ch eq '}'){
                    --$depth;
                    next_chr();
                    if ($F_HOOK) {
                        return _json_object_hook($o);
                    }
                    return $o;
                }
 
                if($ch ne ','){
                    last;
                }
 
                next_chr();
                white();
 
                if ($relaxed and $ch eq '}') {
                    --$depth;
                    next_chr();
                    if ($F_HOOK) {
                        return _json_object_hook($o);
                    }
                    return $o;
                }
 
            }
 
        }
 
        $at-- if defined $ch and $ch ne '';
        decode_error(", or } expected while parsing object/hash");
    }
 
 
    sub bareKey { # doesn't strictly follow Standard ECMA-262 3rd Edition
        my $key;
        while($ch =~ /[^\x00-\x23\x25-\x2F\x3A-\x40\x5B-\x5E\x60\x7B-\x7F]/){
            $key .= $ch;
            next_chr();
        }
        return $key;
    }
 
 
    sub word {
        my $word =  substr($text,$at-1,4);
 
        if($word eq 'true'){
            $at += 3;
            next_chr;
            return defined $alt_true ? $alt_true : $JSON::PP::true;
        }
        elsif($word eq 'null'){
            $at += 3;
            next_chr;
            return undef;
        }
        elsif($word eq 'fals'){
            $at += 3;
            if(substr($text,$at,1) eq 'e'){
                $at++;
                next_chr;
                return defined $alt_false ? $alt_false : $JSON::PP::false;
            }
        }
 
        $at--; # for decode_error report
 
        decode_error("'null' expected")  if ($word =~ /^n/);
        decode_error("'true' expected")  if ($word =~ /^t/);
        decode_error("'false' expected") if ($word =~ /^f/);
        decode_error("malformed JSON string, neither array, object, number, string or atom");
    }
 
 
    sub number {
        my $n    = '';
        my $v;
        my $is_dec;
        my $is_exp;
 
        if($ch eq '-'){
            $n = '-';
            next_chr;
            if (!defined $ch or $ch !~ /\d/) {
                decode_error("malformed number (no digits after initial minus)");
            }
        }
 
        # According to RFC4627, hex or oct digits are invalid.
        if($ch eq '0'){
            my $peek = substr($text,$at,1);
            if($peek =~ /^[0-9a-dfA-DF]/){ # e may be valid (exponential)
                decode_error("malformed number (leading zero must not be followed by another digit)");
            }
            $n .= $ch;
            next_chr;
        }
 
        while(defined $ch and $ch =~ /\d/){
            $n .= $ch;
            next_chr;
        }
 
        if(defined $ch and $ch eq '.'){
            $n .= '.';
            $is_dec = 1;
 
            next_chr;
            if (!defined $ch or $ch !~ /\d/) {
                decode_error("malformed number (no digits after decimal point)");
            }
            else {
                $n .= $ch;
            }
 
            while(defined(next_chr) and $ch =~ /\d/){
                $n .= $ch;
            }
        }
 
        if(defined $ch and ($ch eq 'e' or $ch eq 'E')){
            $n .= $ch;
            $is_exp = 1;
            next_chr;
 
            if(defined($ch) and ($ch eq '+' or $ch eq '-')){
                $n .= $ch;
                next_chr;
                if (!defined $ch or $ch =~ /\D/) {
                    decode_error("malformed number (no digits after exp sign)");
                }
                $n .= $ch;
            }
            elsif(defined($ch) and $ch =~ /\d/){
                $n .= $ch;
            }
            else {
                decode_error("malformed number (no digits after exp sign)");
            }
 
            while(defined(next_chr) and $ch =~ /\d/){
                $n .= $ch;
            }
 
        }
 
        $v .= $n;
 
        if ($is_dec or $is_exp) {
            if ($allow_bignum) {
                require Math::BigFloat;
                return Math::BigFloat->new($v);
            }
        } else {
            if (length $v > $max_intsize) {
                if ($allow_bignum) { # from Adam Sussman
                    require Math::BigInt;
                    return Math::BigInt->new($v);
                }
                else {
                    return "$v";
                }
            }
        }
 
        return $is_dec ? $v/1.0 : 0+$v;
    }
 
 
    sub is_valid_utf8 {
 
        $utf8_len = $_[0] =~ /[\x00-\x7F]/  ? 1
                  : $_[0] =~ /[\xC2-\xDF]/  ? 2
                  : $_[0] =~ /[\xE0-\xEF]/  ? 3
                  : $_[0] =~ /[\xF0-\xF4]/  ? 4
                  : 0
                  ;
 
        return unless $utf8_len;
 
        my $is_valid_utf8 = substr($text, $at - 1, $utf8_len);
 
        return ( $is_valid_utf8 =~ /^(?:
             [\x00-\x7F]
            |[\xC2-\xDF][\x80-\xBF]
            |[\xE0][\xA0-\xBF][\x80-\xBF]
            |[\xE1-\xEC][\x80-\xBF][\x80-\xBF]
            |[\xED][\x80-\x9F][\x80-\xBF]
            |[\xEE-\xEF][\x80-\xBF][\x80-\xBF]
            |[\xF0][\x90-\xBF][\x80-\xBF][\x80-\xBF]
            |[\xF1-\xF3][\x80-\xBF][\x80-\xBF][\x80-\xBF]
            |[\xF4][\x80-\x8F][\x80-\xBF][\x80-\xBF]
        )$/x )  ? $is_valid_utf8 : '';
    }
 
 
    sub decode_error {
        my $error  = shift;
        my $no_rep = shift;
        my $str    = defined $text ? substr($text, $at) : '';
        my $mess   = '';
        my $type   = 'U*';
 
        if ( OLD_PERL ) {
            my $type   =  $] <  5.006           ? 'C*'
                        : utf8::is_utf8( $str ) ? 'U*' # 5.6
                        : 'C*'
                        ;
        }
 
        for my $c ( unpack( $type, $str ) ) { # emulate pv_uni_display() ?
            $mess .=  $c == 0x07 ? '\a'
                    : $c == 0x09 ? '\t'
                    : $c == 0x0a ? '\n'
                    : $c == 0x0d ? '\r'
                    : $c == 0x0c ? '\f'
                    : $c <  0x20 ? sprintf('\x{%x}', $c)
                    : $c == 0x5c ? '\\\\'
                    : $c <  0x80 ? chr($c)
                    : sprintf('\x{%x}', $c)
                    ;
            if ( length $mess >= 20 ) {
                $mess .= '...';
                last;
            }
        }
 
        unless ( length $mess ) {
            $mess = '(end of string)';
        }
 
        Carp::croak (
            $no_rep ? "$error" : "$error, at character offset $at (before \"$mess\")"
        );
 
    }
 
 
    sub _json_object_hook {
        my $o    = $_[0];
        my @ks = keys %{$o};
 
        if ( $cb_sk_object and @ks == 1 and exists $cb_sk_object->{ $ks[0] } and ref $cb_sk_object->{ $ks[0] } ) {
            my @val = $cb_sk_object->{ $ks[0] }->( $o->{$ks[0]} );
            if (@val == 0) {
                return $o;
            }
            elsif (@val == 1) {
                return $val[0];
            }
            else {
                Carp::croak("filter_json_single_key_object callbacks must not return more than one scalar");
            }
        }
 
        my @val = $cb_object->($o) if ($cb_object);
        if (@val == 0) {
            return $o;
        }
        elsif (@val == 1) {
            return $val[0];
        }
        else {
            Carp::croak("filter_json_object callbacks must not return more than one scalar");
        }
    }
 
 
    sub PP_decode_box {
        {
            text    => $text,
            at      => $at,
            ch      => $ch,
            len     => $len,
            depth   => $depth,
            encoding      => $encoding,
            is_valid_utf8 => $is_valid_utf8,
        };
    }
 
} # PARSE
 
 
sub _decode_surrogates { # from perlunicode
    my $uni = 0x10000 + (hex($_[0]) - 0xD800) * 0x400 + (hex($_[1]) - 0xDC00);
    my $un  = pack('U*', $uni);
    utf8::encode( $un );
    return $un;
}
 
 
sub _decode_unicode {
    my $un = pack('U', hex shift);
    utf8::encode( $un );
    return $un;
}
 
#
# Setup for various Perl versions (the code from JSON::PP58)
#
 
BEGIN {
 
    unless ( defined &utf8::is_utf8 ) {
       require Encode;
       *utf8::is_utf8 = *Encode::is_utf8;
    }
 
    if ( !OLD_PERL ) {
        *JSON::PP::JSON_PP_encode_ascii      = \&_encode_ascii;
        *JSON::PP::JSON_PP_encode_latin1     = \&_encode_latin1;
        *JSON::PP::JSON_PP_decode_surrogates = \&_decode_surrogates;
        *JSON::PP::JSON_PP_decode_unicode    = \&_decode_unicode;
 
        if ($] < 5.008003) { # join() in 5.8.0 - 5.8.2 is broken.
            package JSON::PP;
            require subs;
            subs->import('join');
            eval q|
                sub join {
                    return '' if (@_ < 2);
                    my $j   = shift;
                    my $str = shift;
                    for (@_) { $str .= $j . $_; }
                    return $str;
                }
            |;
        }
    }
 
 
    sub JSON::PP::incr_parse {
        local $Carp::CarpLevel = 1;
        ( $_[0]->{_incr_parser} ||= JSON::PP::IncrParser->new )->incr_parse( @_ );
    }
 
 
    sub JSON::PP::incr_skip {
        ( $_[0]->{_incr_parser} ||= JSON::PP::IncrParser->new )->incr_skip;
    }
 
 
    sub JSON::PP::incr_reset {
        ( $_[0]->{_incr_parser} ||= JSON::PP::IncrParser->new )->incr_reset;
    }
 
    eval q{
        sub JSON::PP::incr_text : lvalue {
            $_[0]->{_incr_parser} ||= JSON::PP::IncrParser->new;
 
            if ( $_[0]->{_incr_parser}->{incr_pos} ) {
                Carp::croak("incr_text cannot be called when the incremental parser already started parsing");
            }
            $_[0]->{_incr_parser}->{incr_text};
        }
    } if ( $] >= 5.006 );
 
} # Setup for various Perl versions (the code from JSON::PP58)
 
 
###############################
# Utilities
#
 
BEGIN {
    eval 'require Scalar::Util';
    unless($@){
        *JSON::PP::blessed = \&Scalar::Util::blessed;
        *JSON::PP::reftype = \&Scalar::Util::reftype;
        *JSON::PP::refaddr = \&Scalar::Util::refaddr;
    }
    else{ # This code is from Scalar::Util.
        # warn $@;
        eval 'sub UNIVERSAL::a_sub_not_likely_to_be_here { ref($_[0]) }';
        *JSON::PP::blessed = sub {
            local($@, $SIG{__DIE__}, $SIG{__WARN__});
            ref($_[0]) ? eval { $_[0]->a_sub_not_likely_to_be_here } : undef;
        };
        require B;
        my %tmap = qw(
            B::NULL   SCALAR
            B::HV     HASH
            B::AV     ARRAY
            B::CV     CODE
            B::IO     IO
            B::GV     GLOB
            B::REGEXP REGEXP
        );
        *JSON::PP::reftype = sub {
            my $r = shift;
 
            return undef unless length(ref($r));
 
            my $t = ref(B::svref_2object($r));
 
            return
                exists $tmap{$t} ? $tmap{$t}
              : length(ref($$r)) ? 'REF'
              :                    'SCALAR';
        };
        *JSON::PP::refaddr = sub {
          return undef unless length(ref($_[0]));
 
          my $addr;
          if(defined(my $pkg = blessed($_[0]))) {
            $addr .= bless $_[0], 'Scalar::Util::Fake';
            bless $_[0], $pkg;
          }
          else {
            $addr .= $_[0]
          }
 
          $addr =~ /0x(\w+)/;
          local $^W;
          #no warnings 'portable';
          hex($1);
        }
    }
}
 
 
# shamelessly copied and modified from JSON::XS code.
 
$JSON::PP::true  = do { bless \(my $dummy = 1), "JSON::PP::Boolean" };
$JSON::PP::false = do { bless \(my $dummy = 0), "JSON::PP::Boolean" };
 
sub is_bool { blessed $_[0] and ( $_[0]->isa("JSON::PP::Boolean") or $_[0]->isa("Types::Serialiser::BooleanBase") or $_[0]->isa("JSON::XS::Boolean") ); }
 
sub true  { $JSON::PP::true  }
sub false { $JSON::PP::false }
sub null  { undef; }
 
###############################
 
package JSON::PP::IncrParser;
 
use strict;
 
use constant INCR_M_WS   => 0; # initial whitespace skipping
use constant INCR_M_STR  => 1; # inside string
use constant INCR_M_BS   => 2; # inside backslash
use constant INCR_M_JSON => 3; # outside anything, count nesting
use constant INCR_M_C0   => 4;
use constant INCR_M_C1   => 5;
use constant INCR_M_TFN  => 6;
use constant INCR_M_NUM  => 7;
 
$JSON::PP::IncrParser::VERSION = '1.01';
 
sub new {
    my ( $class ) = @_;
 
    bless {
        incr_nest    => 0,
        incr_text    => undef,
        incr_pos     => 0,
        incr_mode    => 0,
    }, $class;
}
 
 
sub incr_parse {
    my ( $self, $coder, $text ) = @_;
 
    $self->{incr_text} = '' unless ( defined $self->{incr_text} );
 
    if ( defined $text ) {
        if ( utf8::is_utf8( $text ) and !utf8::is_utf8( $self->{incr_text} ) ) {
            utf8::upgrade( $self->{incr_text} ) ;
            utf8::decode( $self->{incr_text} ) ;
        }
        $self->{incr_text} .= $text;
    }
 
    if ( defined wantarray ) {
        my $max_size = $coder->get_max_size;
        my $p = $self->{incr_pos};
        my @ret;
        {
            do {
                unless ( $self->{incr_nest} <= 0 and $self->{incr_mode} == INCR_M_JSON ) {
                    $self->_incr_parse( $coder );
 
                    if ( $max_size and $self->{incr_pos} > $max_size ) {
                        Carp::croak("attempted decode of JSON text of $self->{incr_pos} bytes size, but max_size is set to $max_size");
                    }
                    unless ( $self->{incr_nest} <= 0 and $self->{incr_mode} == INCR_M_JSON ) {
                        # as an optimisation, do not accumulate white space in the incr buffer
                        if ( $self->{incr_mode} == INCR_M_WS and $self->{incr_pos} ) {
                            $self->{incr_pos} = 0;
                            $self->{incr_text} = '';
                        }
                        last;
                    }
                }
 
                my ($obj, $offset) = $coder->PP_decode_json( $self->{incr_text}, 0x00000001 );
                push @ret, $obj;
                use bytes;
                $self->{incr_text} = substr( $self->{incr_text}, $offset || 0 );
                $self->{incr_pos} = 0;
                $self->{incr_nest} = 0;
                $self->{incr_mode} = 0;
                last unless wantarray;
            } while ( wantarray );
        }
 
        if ( wantarray ) {
            return @ret;
        }
        else { # in scalar context
            return defined $ret[0] ? $ret[0] : undef;
        }
    }
}
 
 
sub _incr_parse {
    my ($self, $coder) = @_;
    my $text = $self->{incr_text};
    my $len = length $text;
    my $p = $self->{incr_pos};
 
INCR_PARSE:
    while ( $len > $p ) {
        my $s = substr( $text, $p, 1 );
        last INCR_PARSE unless defined $s;
        my $mode = $self->{incr_mode};
 
        if ( $mode == INCR_M_WS ) {
            while ( $len > $p ) {
                $s = substr( $text, $p, 1 );
                last INCR_PARSE unless defined $s;
                if ( ord($s) > 0x20 ) {
                    if ( $s eq '#' ) {
                        $self->{incr_mode} = INCR_M_C0;
                        redo INCR_PARSE;
                    } else {
                        $self->{incr_mode} = INCR_M_JSON;
                        redo INCR_PARSE;
                    }
                }
                $p++;
            }
        } elsif ( $mode == INCR_M_BS ) {
            $p++;
            $self->{incr_mode} = INCR_M_STR;
            redo INCR_PARSE;
        } elsif ( $mode == INCR_M_C0 or $mode == INCR_M_C1 ) {
            while ( $len > $p ) {
                $s = substr( $text, $p, 1 );
                last INCR_PARSE unless defined $s;
                if ( $s eq "\n" ) {
                    $self->{incr_mode} = $self->{incr_mode} == INCR_M_C0 ? INCR_M_WS : INCR_M_JSON;
                    last;
                }
                $p++;
            }
            next;
        } elsif ( $mode == INCR_M_TFN ) {
            while ( $len > $p ) {
                $s = substr( $text, $p++, 1 );
                next if defined $s and $s =~ /[rueals]/;
                last;
            }
            $p--;
            $self->{incr_mode} = INCR_M_JSON;
 
            last INCR_PARSE unless $self->{incr_nest};
            redo INCR_PARSE;
        } elsif ( $mode == INCR_M_NUM ) {
            while ( $len > $p ) {
                $s = substr( $text, $p++, 1 );
                next if defined $s and $s =~ /[0-9eE.+\-]/;
                last;
            }
            $p--;
            $self->{incr_mode} = INCR_M_JSON;
 
            last INCR_PARSE unless $self->{incr_nest};
            redo INCR_PARSE;
        } elsif ( $mode == INCR_M_STR ) {
            while ( $len > $p ) {
                $s = substr( $text, $p, 1 );
                last INCR_PARSE unless defined $s;
                if ( $s eq '"' ) {
                    $p++;
                    $self->{incr_mode} = INCR_M_JSON;
 
                    last INCR_PARSE unless $self->{incr_nest};
                    redo INCR_PARSE;
                }
                elsif ( $s eq '\\' ) {
                    $p++;
                    if ( !defined substr($text, $p, 1) ) {
                        $self->{incr_mode} = INCR_M_BS;
                        last INCR_PARSE;
                    }
                }
                $p++;
            }
        } elsif ( $mode == INCR_M_JSON ) {
            while ( $len > $p ) {
                $s = substr( $text, $p++, 1 );
                if ( $s eq "\x00" ) {
                    $p--;
                    last INCR_PARSE;
                } elsif ( $s eq "\x09" or $s eq "\x0a" or $s eq "\x0d" or $s eq "\x20" ) {
                    if ( !$self->{incr_nest} ) {
                        $p--; # do not eat the whitespace, let the next round do it
                        last INCR_PARSE;
                    }
                    next;
                } elsif ( $s eq 't' or $s eq 'f' or $s eq 'n' ) {
                    $self->{incr_mode} = INCR_M_TFN;
                    redo INCR_PARSE;
                } elsif ( $s =~ /^[0-9\-]$/ ) {
                    $self->{incr_mode} = INCR_M_NUM;
                    redo INCR_PARSE;
                } elsif ( $s eq '"' ) {
                    $self->{incr_mode} = INCR_M_STR;
                    redo INCR_PARSE;
                } elsif ( $s eq '[' or $s eq '{' ) {
                    if ( ++$self->{incr_nest} > $coder->get_max_depth ) {
                        Carp::croak('json text or perl structure exceeds maximum nesting level (max_depth set too low?)');
                    }
                    next;
                } elsif ( $s eq ']' or $s eq '}' ) {
                    if ( --$self->{incr_nest} <= 0 ) {
                        last INCR_PARSE;
                    }
                } elsif ( $s eq '#' ) {
                    $self->{incr_mode} = INCR_M_C1;
                    redo INCR_PARSE;
                }
            }
        }
    }
 
    $self->{incr_pos} = $p;
    $self->{incr_parsing} = $p ? 1 : 0; # for backward compatibility
}
 
 
sub incr_text {
    if ( $_[0]->{incr_pos} ) {
        Carp::croak("incr_text cannot be called when the incremental parser already started parsing");
    }
    $_[0]->{incr_text};
}
 
 
sub incr_skip {
    my $self  = shift;
    $self->{incr_text} = substr( $self->{incr_text}, $self->{incr_pos} );
    $self->{incr_pos}     = 0;
    $self->{incr_mode}    = 0;
    $self->{incr_nest}    = 0;
}
 
 
sub incr_reset {
    my $self = shift;
    $self->{incr_text}    = undef;
    $self->{incr_pos}     = 0;
    $self->{incr_mode}    = 0;
    $self->{incr_nest}    = 0;
}
 
###############################
 
 
1;
__END__
