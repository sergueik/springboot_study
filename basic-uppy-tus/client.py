#!/usr/bin/env python3

import argparse
import base64
import os
import sys
import requests
import re
import json
from urllib.parse import urlparse

TUS_VERSION = '1.0.0'


def parse_size(value):
  value = value.strip().upper().replace(' ', '')

  m = re.fullmatch(r"(\d+)([KMGT]?B?)?", value)
  if not m:
    raise argparse.ArgumentTypeError(
      f"Invalid size: {value}"
    )

  number = int(m.group(1))
  suffix = m.group(2) or ''

  multipliers = {
    '': 1,
    'B': 1,
    'K': 1024,
    'KB': 1024,
    'M': 1024 ** 2,
    'MB': 1024 ** 2,
    'G': 1024 ** 3,
    'GB': 1024 ** 3,
    'T': 1024 ** 4,
    'TB': 1024 ** 4,
  }

  return number * multipliers[suffix]

def create_upload(url, file_path):
  size = os.path.getsize(file_path)
  filename = os.path.basename(file_path)

  headers = {
    'Tus-Resumable': TUS_VERSION,
    'Upload-Length': str(size),
    'Upload-Metadata': f"filename {base64.b64encode(filename.encode('utf-8')).decode('utf-8')}"
  }

  # print(f'Send POST to {url}')
  r = requests.post(url, headers=headers)
  r.raise_for_status()
  location = r.headers['Location']
  # print('response header: {}'.format(location))
  # NOTE python way of naming api and ordering arguments
  upload_id = re.sub('^.+/', '', location)
  location = url + '/' + upload_id
  return location, upload_id

def base_origin(url: str) -> str:
  # keep only scheme + host, drop path completely
  p = urlparse(url)
  return f"{p.scheme}://{p.netloc}"

def finalize(upload_url: str, upload_id: str) -> None:
  finalize_url = base_origin(upload_url) + '/api/uploads/finalize'
  payload = { 'uploadId': upload_id}
  body = json.dumps(payload).encode('ascii')
  response = requests.post(finalize_url,data=body, headers={ 'Content-Type': 'application/json' })
  response.raise_for_status()

def validate(upload_url: str, upload_id: str, hash: str, file_path: str) -> None:
  pass

def get_offset(upload_url):
  r = requests.head(upload_url, headers={'Tus-Resumable': TUS_VERSION})
  r.raise_for_status()

  return int(r.headers.get('Upload-Offset', 0))


def upload(upload_url, file_path, chunk_size, start_offset=None):
  file_size = os.path.getsize(file_path)

  offset = start_offset if start_offset is not None else get_offset(upload_url)

  with open(file_path, 'rb') as f:
    f.seek(offset)

    while offset < file_size:
      chunk = f.read(chunk_size)
      if not chunk:
        break

      headers = {
        'Tus-Resumable': TUS_VERSION,
        'Upload-Offset': str(offset),
        'Content-Type': 'application/offset+octet-stream'
      }

      r = requests.patch(upload_url, headers=headers, data=chunk)
      r.raise_for_status()

      # ALWAYS trust server response
      offset = int(r.headers.get('Upload-Offset', offset))

      percent = (offset / file_size) * 100
      print(f"\rUploaded {offset}/{file_size} bytes ({percent:.2f}%)", end="", file=sys.stderr)

  print("\nDone.", file=sys.stderr)


def main():
  parser = argparse.ArgumentParser(description='Simple TUS upload CLI client')
  # the file and url are considered the *primary operands*
  # NOTE: positional arguments are *always* required unless coded so
  parser.add_argument('file', help='File to upload', nargs='?')
  parser.add_argument('url', help='TUS server base URL (e.g. http://localhost:1080/files)',nargs='?')
  # support both

  parser.add_argument('--file', dest='file_opt')
  parser.add_argument('--url', dest='url_opt')

  parser.add_argument('--profile')

  # quality-of-life
  # NOTE: In Python int automatically grows as needed
  parser.add_argument( '--chunk-size', type=parse_size, default=1024 * 1024, help='Chunk size (e.g. 256K, 10M, 1GB)')

  parser.add_argument('--create', action='store_true', help='Create new upload before uploading')

  parser.add_argument('--resume', action='store_true', help='Resume upload from server offset')

  parser.add_argument('--from-offset', type=int, default=None, help='Force start offset (advanced override)')

  args = parser.parse_args()

  file_path = args.file or args.file_opt
  if not file_path:
    parser.error('file is required')

  # TODO: fix the error: the following arguments are required: url
  base_url = args.url or args.url_opt
  if not base_url:
    parser.error('url is required')  

  if args.create:
    upload_url,upload_id = create_upload(base_url, file_path)
    print(f"Created upload: {upload_url}", file=sys.stderr)
  else:
    upload_url = base_url
    if upload_id is None:
      m = re.search(r'/([^/]+)$', upload_url)
      if m:
        upload_id = m.group(1)
  if upload_id is None:
    parser.error( 'upload_id is undefined; use --create or specify an upload resource URL')
  upload( upload_url=upload_url, file_path=file_path, chunk_size=args.chunk_size, start_offset=args.from_offset)
  if args.profile is None or args.profile == 'java':
    finalize(upload_url,upload_id)
    # TODO
    digest = None
    validate(upload_url, upload_id, digest, file_path)
  elif args.profile == 'tusd':
    pass
  else:
    print('unknown profile: {}'.format( args.profile ))

if __name__ == '__main__':
  main()
