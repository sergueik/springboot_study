#!/usr/bin/env python3

import argparse
import base64
import json
import mimetypes
import os
import sys

import requests


def build_upload_request(
        filename,
        foo=None,
        bar=None,
        content_type=None,
        code_page=None):

    #
    # binary-safe read
    #
    with open(filename, "rb") as stream:
        data = stream.read()

    #
    # RFC4648 base64
    #
    content_base64 = (
        base64.b64encode(data)
        .decode("ascii")
    )

    #
    # optional MIME detection
    #
    if content_type is None:

        detected, _ = (
            mimetypes.guess_type(filename)
        )

        content_type = (
            detected
            or "application/octet-stream"
        )

    payload = {
        "foo": foo,
        "bar": bar,
        "filename": os.path.basename(filename),
        "contentType": content_type,
        "contentBase64": content_base64
    }

    #
    # optional metadata
    #
    if code_page is not None:
        payload["codePage"] = code_page

    return payload


def post_upload_request(
        url,
        payload,
        verbose=False):

    headers = {
        "Content-Type":
            "application/json"
    }

    response = requests.post(
        url,
        headers=headers,
        json=payload)

    if verbose:

        print(
            "HTTP status:",
            response.status_code)

        print(
            "Response body:")

        print(response.text)

    return response


def main():

    parser = argparse.ArgumentParser()

    parser.add_argument(
        "--foo")

    parser.add_argument(
        "--bar")

    parser.add_argument(
        "--file",
        required=True)

    parser.add_argument(
        "--content-type")

    parser.add_argument(
        "--code-page")

    parser.add_argument(
        "--url",
        required=True)

    parser.add_argument(
        "--verbose",
        action="store_true")

    parser.add_argument(
        "--dry-run",
        action="store_true")

    args = parser.parse_args()

    payload = build_upload_request(
        filename=args.file,
        foo=args.foo,
        bar=args.bar,
        content_type=args.content_type,
        code_page=args.code_page)

    if args.dry_run:

        print(
            json.dumps(
                payload,
                indent=2))

        return

    response = post_upload_request(
        url=args.url,
        payload=payload,
        verbose=args.verbose)

    #
    # CI/CD friendly exit code
    #
    if response.status_code >= 400:
        sys.exit(1)


if __name__ == "__main__":
    main()
