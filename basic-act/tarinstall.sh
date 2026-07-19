#!/usr/bin/env sh

# Destination directory for the installation of the final binary.
TARINSTALL_DESTDIR=${TARINSTALL_DESTDIR:-"/usr/local/bin"}

# Path relative the extracted tar where to find the binary to install. When
# empty, this will be the same as the name of the tar file, without any
# extension (so .tar.gz, or .tgz removed).
TARINSTALL_EXTRACT=${TARINSTALL_EXTRACT:-""}

# Name of the binary to place under $TARINSTALL_DESTDIR. When empty, the
# default, this will be basename of the extraction path specified at
# $TARINSTALL_EXTRACT.
TARINSTALL_BIN=${TARINSTALL_BIN:-""}

# Set this to 1 for increased verbosity
TARINSTALL_VERBOSE=${TARINSTALL_VERBOSE:-0}

while [ $# -gt 0 ]; do
  case "$1" in
    -d | --dest | --destination)
      TARINSTALL_DESTDIR=$2; shift 2;;
    --dest=* | --destination=*)
      TARINSTALL_DESTDIR="${1#*=}"; shift 1;;

    -b | --bin | --binary)
      TARINSTALL_BIN=$2; shift 2;;
    --bin=* | --binary=*)
      TARINSTALL_BIN="${1#*=}"; shift 1;;

    -x | --extract)
      TARINSTALL_EXTRACT=$2; shift 2;;
    --extract=*)
      TARINSTALL_EXTRACT="${1#*=}"; shift 1;;

    -v | --verbose)
      TARINSTALL_VERBOSE=1; shift;;

    --)
      shift; break;;
    -*)
      usage "Unknown option: $1 !";;
    *)
      break;;
  esac
done

verbose() {
  [ "$TARINSTALL_VERBOSE" = "1" ] && printf %s\\n "$1" >&2
}
errlog() {
  printf %s\\n "$1" >&2
}
download() {
  verbose "Downloading $1"
  if command -v curl >&2 >/dev/null; then
    curl -sSL "$1" > "$2"
  elif command -v wget >&2 >/dev/null; then
    wget -q -O - "$1" > "$2"
  else
    errlog "Can neither find curl, nor wget for downloading"
    return 1
  fi
}

if [ "$#" != "1" ]; then
  errlog "You must specify a URL to install from!"
  exit 1
fi

TARNAME=$(basename "$1")
[ -z "$TARINSTALL_EXTRACT" ] && TARINSTALL_EXTRACT=${TARNAME%%.*}
[ -z "$TARINSTALL_BIN" ] && TARINSTALL_BIN=$(basename "$TARINSTALL_EXTRACT")

TMPDIR=$(mktemp -d)
download "$1" "${TMPDIR}/${TARNAME}"
if ! [ -f "${TMPDIR}/${TARNAME}" ]; then
  errlog "Could not download from $1 to ${TMPDIR}/${TARNAME}"
  exit 1
fi

taropts="xf"
[ "$TARINSTALL_VERBOSE" = "1" ] && taropts="xvf"
tar -C "${TMPDIR}" -${taropts} "${TMPDIR}/${TARNAME}"
if [ -f "${TMPDIR}/${TARINSTALL_EXTRACT}" ]; then
  chmod a+x "${TMPDIR}/${TARINSTALL_EXTRACT}"
  verbose "Installing as ${TARINSTALL_DESTDIR%/}/${TARINSTALL_BIN}"
  mv -f "${TMPDIR}/${TARINSTALL_EXTRACT}" "${TARINSTALL_DESTDIR%/}/${TARINSTALL_BIN}"
  rm -rf "$TMPDIR"
else
  errlog "Could not find ${TMPDIR}/${TARINSTALL_EXTRACT}"
  exit 1
fi
