#!/usr/bin/env python3

import argparse
import base64
import os
import sys
import requests


TUS_VERSION = '1.0.0'


def create_upload(url, file_path):
    size = os.path.getsize(file_path)
    filename = os.path.basename(file_path)

    headers = {
        'Tus-Resumable': TUS_VERSION,
        'Upload-Length': str(size),
        'Upload-Metadata': f"filename {base64.b64encode(filename.encode('utf-8')).decode('utf-8')}"
    }

    r = requests.post(url, headers=headers)
    r.raise_for_status()

    return r.headers['Location']


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
            offset = int(r.headers.get("Upload-Offset", offset))

            percent = (offset / file_size) * 100
            print(f"\rUploaded {offset}/{file_size} bytes ({percent:.2f}%)", end="", file=sys.stderr)

    print("\nDone.", file=sys.stderr)


def main():
    parser = argparse.ArgumentParser(description="Simple TUS upload CLI client")

    parser.add_argument("file", help="File to upload")
    parser.add_argument("url", help="TUS server base URL (e.g. http://localhost:1080/files)")

    parser.add_argument("--chunk-size", type=int, default=1024 * 1024,
                        help="Chunk size in bytes (default: 1MB)")

    parser.add_argument("--create", action="store_true",
                        help="Create new upload before uploading")

    parser.add_argument("--resume", action="store_true",
                        help="Resume upload from server offset")

    parser.add_argument("--from-offset", type=int, default=None,
                        help="Force start offset (advanced override)")

    args = parser.parse_args()

    file_path = args.file
    base_url = args.url

    if args.create:
        upload_url = create_upload(base_url, file_path)
        print(f"Created upload: {upload_url}", file=sys.stderr)
    else:
        upload_url = base_url

    upload(
        upload_url=upload_url,
        file_path=file_path,
        chunk_size=args.chunk_size,
        start_offset=args.from_offset
    )


if __name__ == "__main__":
    main()
