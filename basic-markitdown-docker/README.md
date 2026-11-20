### Info
  replica of the Deban based [markitdown]()
  [dockerfile](https://github.com/tupolev/markitdown/blob/main/Dockerfile) with some tweaks

### Usage

There is no official DockerHub repository although MarkItDown supports Docker.

```sh
docker pull python:3.13-slim-bullseye
```
* remove unavaiilable security repositories and downgrade Perl package - the bullseye EOL.
```sh
docker build -t builder -f Dockerfile.builder .
```
```text
Step 1/7 : FROM python:3.13-slim-bullseye
 ---> 5e751abf51db
Step 2/7 : ENV DEBIAN_FRONTEND=noninteractive     PYTHONDONTWRITEBYTECODE=1     PYTHONUNBUFFERED=1     EXIFTOOL_PATH=/usr/bin/exiftool     FFMPEG_PATH=/usr/bin/ffmpeg
 ---> Using cache
 ---> 4e3122274a8d
Step 3/7 : RUN grep -rl -E 'debian-security|bullseye-security'     /etc/apt/sources.list /etc/apt/sources.list.d/ /dev/null     | xargs -IX sed -i '/\(debian-security\|bullseye-security\)/d' X
 ---> Using cache
 ---> 154bdd53e907
Step 4/7 : RUN (grep -qrl -E 'debian-security|bullseye-security'     /etc/apt/sources.list /etc/apt/sources.list.d/ && echo 'still have';  exit 1) || exit 0
 ---> Using cache
 ---> e23df0951be5
Step 5/7 : RUN apt-get -qq update
 ---> Using cache
 ---> c50fe01753e1
Step 6/7 : RUN apt-get install -qqy perl=5.32.1-4+deb11u3 perl-base=5.32.1-4+deb11u3 --allow-downgrades
 ---> Using cache
 ---> 2ad9d551420c
Step 7/7 : RUN apt-get install -qy --no-install-recommends         ca-certificates ffmpeg exiftool     && rm -rf /var/lib/apt/lists/*
 ---> Running in 59eaa2fe68f6
Reading package lists...
Building dependency tree...
Reading state information...
ca-certificates is already the newest version (20210119).
The following additional packages will be installed:
...
Suggested packages:
...
0 upgraded, 189 newly installed, 0 to remove and 0 not upgraded.
Need to get 119 MB of archives.
...
Processing triggers for libc-bin (2.31-13+deb11u13) ...
Removing intermediate container 59eaa2fe68f6
 ---> d5e5d23bb8ab
Successfully built d5e5d23bb8ab
Successfully tagged builder:latest
```
* clone specified tag of MarkItDown repository source and expose `markitdown` CLI as the container entrypoint.

```sh
docker build -t markitdown --build-arg VERSION=0.1.8 -f Dockerfile.build .
```

```text
Step 1/15 : FROM builder
 ---> d5e5d23bb8ab
Step 2/15 : WORKDIR /opt
 ---> Using cache
 ---> 5f92fceec8b3
Step 3/15 : RUN apt-get -qq update
 ---> Running in 65bb29fec7ba
Removing intermediate container 65bb29fec7ba
 ---> 7fc892c667f8
Step 4/15 : RUN apt-get install -qy --no-install-recommends git     && rm -rf /var/lib/apt/lists/*
 ---> Running in 82ed3cb3ab65
Reading package lists...
Building dependency tree...
Reading state information...
The following additional packages will be installed:
...
Removing intermediate container 82ed3cb3ab65
 ---> 827f3869e644
Step 5/15 : ARG REPO=https://github.com/microsoft/markitdown.git
 ---> Running in 507a8d311605
Removing intermediate container 507a8d311605
 ---> 39fd57053247
Step 6/15 : ARG VERSION=0.1.8
 ---> Running in f4533e55a2d1
Removing intermediate container f4533e55a2d1
 ---> 2664d9dc23ca
Step 7/15 : ENV TAG=v$VERSION
 ---> Running in cc4bade39730
Removing intermediate container cc4bade39730
 ---> 6cdd05766906
Step 8/15 : RUN git clone --no-progress --depth 1 --branch "${TAG}" "${REPO}" markitdown
 ---> Running in a3e89ba2ad5f
[91mCloning into 'markitdown'...
[0m[91mNote: switching to 'b8f79c57ebc0044be41323d89b2a45d3fda8460e'.

You are in 'detached HEAD' state. You can look around, make experimental
changes and commit them, and you can discard any commits you make in this
state without impacting any branches by switching back to a branch.
...
Building wheels for collected packages: markitdown, markitdown-sample-plugin
...
Successfully built markitdown markitdown-sample-plugin
Installing collected packages: standard-chunk, pydub, flatbuffers, XlsxWriter, xlrd, urllib3, tzdata, typing-extensions, truststore, tabulate, striprtf, soupsieve, sniffio, pyyaml, python-dotenv, pypdfium2, PyJWT, pycparser, psutil, protobuf, pillow, packaging, olefile, numpy, networkx, lxml, jiter, isodate, idna, h11, et-xmlfile, defusedxml, cobble, click, charset-normalizer, certifi, audioop-lts, annotated-types, typing-inspection, standard-aifc, requests, python-pptx, python-docx, python-dateutil, pydantic-core, openpyxl, onnxruntime, mammoth, httpcore2, cffi, beautifulsoup4, anyio, youtube-transcript-api, speechrecognition, pydantic, pandas, markdownify, magika, httpx2, cryptography, azure-core, pdfminer-six, openai, markitdown, azure-ai-documentintelligence, azure-ai-contentunderstanding, pdfplumber, msal, markitdown-sample-plugin, msal-extensions, azure-identity, pymupdf4llm, pymupdf-layout, pymupdf, markitdown-ocr

Successfully installed PyJWT-2.15.1 XlsxWriter-3.3.3 annotated-types-0.8.0 anyio-4.16.0 audioop-lts-0.2.2 azure-ai-contentunderstanding-1.2.0b3 azure-ai-documentintelligence-1.0.2 azure-core-1.41.0 azure-identity-1.27.0 beautifulsoup4-4.15.0 certifi-2026.10.23 cffi-2.1.1 charset-normalizer-3.5.2 click-8.5.1 cobble-0.1.4 cryptography-50.0.2 defusedxml-0.7.1 et-xmlfile-2.0.0 flatbuffers-25.12.19 h11-0.16.0 httpcore2-2.16.2 httpx2-2.16.2 idna-3.20 isodate-0.7.2 jiter-0.19.0 lxml-6.1.3 magika-0.6.3 mammoth-1.11.0 markdownify-1.2.4 markitdown-0.1.8 markitdown-ocr-0.1.1 markitdown-sample-plugin-0.1.0a1 msal-1.39.0 msal-extensions-1.3.1 networkx-3.7 numpy-2.5.5 olefile-0.47 onnxruntime-1.32.0 openai-3.57.0 openpyxl-3.1.5 packaging-26.3 pandas-3.1.1 pdfminer-six-20260107 pdfplumber-0.11.10 pillow-13.0.0 protobuf-7.36.2 psutil-7.2.2 pycparser-3.12 pydantic-2.14.4 pydantic-core-2.50.5 pydub-0.25.1 pymupdf-2.1 pymupdf-layout-2.1 pymupdf4llm-2.1 pypdfium2-5.15.2 python-dateutil-4.0.0 python-docx-1.2.0 python-dotenv-1.2.4 python-pptx-1.0.2 pyyaml-6.0.3 requests-2.34.2 sniffio-1.3.1 soupsieve-4.1.3 speechrecognition-3.17.0 standard-aifc-3.13.0 standard-chunk-3.13.0 striprtf-0.0.35 tabulate-0.10.0 truststore-0.10.5 typing-extensions-4.16.0 typing-inspection-0.4.5 tzdata-2026.6 urllib3-2.8.0 xlrd-2.0.2 youtube-transcript-api-1.2.5
[91mWARNING: Running pip as the 'root' user can result in broken permissions and conflicting behaviour with the system package manager, possibly rendering your system unusable. It is recommended to use a virtual environment instead: https://pip.pypa.io/warnings/venv. Use the --root-user-action option if you know what you are doing and want to suppress this warning.
[0mRemoving intermediate container 4fa76185acee
 ---> 058320aebef1
Step 11/15 : COPY entrypoint.sh /usr/local/bin/entrypoint.sh
 ---> 0a1d8a3f16f3
Step 12/15 : RUN chmod +x /usr/local/bin/entrypoint.sh
 ---> Running in 721383efad5b
Removing intermediate container 721383efad5b
 ---> 5fd774f83735
Step 13/15 : WORKDIR /data
 ---> Running in e9ceffc9f8a6
Removing intermediate container e9ceffc9f8a6
 ---> 8217ee4c20a3
Step 14/15 : ENTRYPOINT ["/usr/local/bin/entrypoint.sh"]
 ---> Running in e6af4a9725be
Removing intermediate container e6af4a9725be
 ---> 0ad2f7f2b620
Step 15/15 : CMD ["--help"]
 ---> Running in 51afc91ccd33
Removing intermediate container 51afc91ccd33
 ---> 5873afaee7d1
Successfully built 5873afaee7d1
Successfully tagged markitdown:latest

```      
    
* alternatviely install the published application sans markitdown-ocr and openai

```sh
docker build -t markitdown2 --build-arg VERSION=0.1.8 -f Dockerfile .
```
```text
Step 1/8 : FROM builder
 ---> d5e5d23bb8ab
Step 2/8 : ARG VERSION=0.1.8
 ---> Running in a367d5a7b89b
Removing intermediate container a367d5a7b89b
 ---> 57f20158e6d9
Step 3/8 : RUN python -m pip install --no-cache-dir --upgrade pip     && python -m pip install --no-cache-dir         "markitdown[all]==${VERSION}"         markitdown-sample-plugin
 ---> Running in 12cfdae61b07
Requirement already satisfied: pip in /usr/local/lib/python3.13/site-packages (25.2)
Collecting pip
  Downloading pip-26.3-py3-none-any.whl.metadata (4.6 kB)
Downloading pip-26.3-py3-none-any.whl (2.0 MB)
   ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━ 2.0/2.0 MB 12.1 MB/s  0:00:00
Installing collected packages: pip
  Attempting uninstall: pip
...
Successfully installed Pillow-13.0.0 PyJWT-2.15.1 XlsxWriter-3.3.3 audioop-lts-0.2.2 azure-ai-contentunderstanding-1.2.0b3 azure-ai-documentintelligence-1.0.2 azure-core-1.41.0 azure-identity-1.27.0 beautifulsoup4-4.15.0 certifi-2026.10.23 cffi-2.1.1 charset-normalizer-3.5.2 click-8.5.1 cobble-0.1.4 cryptography-50.0.2 defusedxml-0.7.1 et-xmlfile-2.0.0 flatbuffers-25.12.19 idna-3.20 isodate-0.7.2 lxml-6.1.3 magika-0.6.3 mammoth-1.11.0 markdownify-1.2.4 markitdown-0.1.8 markitdown-sample-plugin-0.1.0a1 msal-1.39.0 msal-extensions-1.3.1 numpy-2.5.5 olefile-0.47 onnxruntime-1.32.0 openpyxl-3.1.5 packaging-26.3 pandas-3.1.1 pdfminer-six-20260107 pdfplumber-0.11.10 protobuf-7.36.2 pycparser-3.12 pydub-0.25.1 pypdfium2-5.15.2 python-dateutil-4.0.0 python-dotenv-1.2.4 python-pptx-1.0.2 requests-2.34.2 soupsieve-4.1.3 speechrecognition-3.17.0 standard-aifc-3.13.0 standard-chunk-3.13.0 striprtf-0.0.35 typing-extensions-4.16.0 tzdata-2026.6 urllib3-2.8.0 xlrd-2.0.2 youtube-transcript-api-1.2.5
WARNING: Running pip as the 'root' user can result in broken permissions and conflicting behaviour with the system package manager, possibly rendering your system unusable. It is recommended to use a virtual environment instead: https://pip.pypa.io/warnings/venv. Use the --root-user-action option if you know what you are doing and want to suppress this warning.
Removing intermediate container 12cfdae61b07
 ---> 105b9da4fae0
Step 4/8 : COPY entrypoint.sh /usr/local/bin/entrypoint.sh
 ---> c948dc3449ce
Step 5/8 : RUN chmod +x /usr/local/bin/entrypoint.sh
 ---> Running in 3f225aedcba6
Removing intermediate container 3f225aedcba6
 ---> 3b0fa2ae4860
Step 6/8 : WORKDIR /data
 ---> Running in 3287373adc6a
Removing intermediate container 3287373adc6a
 ---> af7f89d6b1a0
Step 7/8 : ENTRYPOINT ["/usr/local/bin/entrypoint.sh"]
 ---> Running in 68387d38c36c
Removing intermediate container 68387d38c36c
 ---> 1e90d9a60035
Step 8/8 : CMD ["--help"]
 ---> Running in 93be30fdadaa
Removing intermediate container 93be30fdadaa
 ---> f6843f1cf232
Successfully built f6843f1cf232
Successfully tagged markitdown2:latest
```
```sh
docker image ls
```
```text
REPOSITORY          TAG                  IMAGE ID            CREATED             SIZE
markitdown2         latest               f6843f1cf232        3 minutes ago       1GB
markitdown          latest               5873afaee7d1        8 minutes ago       1.26GB
builder             latest               d5e5d23bb8ab        16 minutes ago      591MB
<none>              <none>               47e9035e62a3        45 minutes ago      617MB
python              3.13-slim-bullseye   5e751abf51db        15 months ago       120MB
```
```sh
ls -hl ~/.docker/machine/machines/default/disk.vmdk
```
```text
-rw-r--r-- 1 kouzm 197609 2.4G Nov 19 12:20 /c/Users/kouzm/.docker/machine/machines/default/disk.vmdk
```
```sh
docker run -it --rm markitdown
```

```text
usage: SYNTAX:

    markitdown <OPTIONAL: FILENAME>
    If FILENAME is empty, markitdown reads from stdin.

EXAMPLE:

    markitdown example.pdf

    OR

    cat example.pdf | markitdown

    OR

    markitdown < example.pdf

    OR to save to a file use

    markitdown example.pdf -o example.md

    OR

    markitdown example.pdf > example.md

Convert various file formats to markdown.

positional arguments:
  filename

options:
  -h, --help            show this help message and exit
  -v, --version         show the version number and exit
  -o, --output OUTPUT   Output file name. If not provided, output is written
                        to stdout.
  -x, --extension EXTENSION
                        Provide a hint about the file extension (e.g., when
                        reading from stdin).
  -m, --mime-type MIME_TYPE
                        Provide a hint about the file's MIME type.
  -c, --charset CHARSET
                        Provide a hint about the file's charset (e.g., UTF-8).
  -d, --use-docintel    Use Document Intelligence to extract text instead of
                        offline conversion. Requires a valid Document
                        Intelligence Endpoint.
  --use-cu, --use-content-understanding
                        Use Azure Content Understanding to extract text.
                        Requires --cu-endpoint.
  -e, --endpoint ENDPOINT
                        Document Intelligence Endpoint. Required if using
                        Document Intelligence. Defaults to the
                        MARKITDOWN_DOCINTEL_ENDPOINT environment variable.
  --cu-endpoint CU_ENDPOINT
                        Content Understanding Endpoint. Required if using
                        --use-cu. Defaults to the MARKITDOWN_CU_ENDPOINT
                        environment variable.
  --cu-analyzer CU_ANALYZER
                        Content Understanding analyzer ID. If not specified,
                        auto-selects by file type.
  --cu-file-types CU_FILE_TYPES
                        Comma-separated list of file types to route to Content
                        Understanding (e.g., pdf,jpeg,mp4). If omitted, all
                        supported types are routed.
  --docintel-model-id DOCINTEL_MODEL_ID
                        Document Intelligence model ID (e.g., 'prebuilt-
                        layout', 'prebuilt-invoice', or a custom model ID).
                        Defaults to 'prebuilt-layout'.
  --docintel-query-fields DOCINTEL_QUERY_FIELDS
                        Comma-separated list of field names to extract via the
                        billable Document Intelligence queryFields add-on (OCR
                        file types only).
  --docintel-features DOCINTEL_FEATURES
                        Comma-separated Document Intelligence analysis add-ons
                        for OCR file types. Available values: barcodes,
                        formulas, keyValuePairs, languages, ocrHighResolution,
                        and styleFont. The formulas, ocrHighResolution, and
                        styleFont incur additional charges. No add-ons are
                        enabled by default; use 'none' to explicitly request
                        none.
  -p, --use-plugins     Use 3rd-party plugins to convert files. Use --list-
                        plugins to see installed plugins.
  --list-plugins        List installed 3rd-party plugins. Plugins are loaded
                        when using the -p or --use-plugins option.
  --keep-data-uris      Keep data URIs (like base64-encoded images) in the
                        output. By default, data URIs are truncated.
```
```sh
cp file-sample_150kB.pdf  ~/test
```
```sh
docker run -v /c/Users/$USERNAME/test:/tmp -w /tmp -it --rm markitdown file-sample_150kB.pdf
```
```text
## Page 1

Lorem ipsum
Lorem ipsum dolor sit amet, consectetur adipiscing
elit. Nunc ac faucibus odio.
Vestibulum neque massa, scelerisque sit amet ligula eu, congue molestie mi. Praesent ut
varius sem. Nullam at porttitor arcu, nec lacinia nisi. Ut ac dolor vitae odio interdum
condimentum. Vivamus dapibus sodales ex, vitae malesuada ipsum cursus
convallis. Maecenas sed egestas nulla, ac condimentum orci. Mauris diam felis,
vulputate ac suscipit et, iaculis non est. Curabitur semper arcu ac ligula semper, nec luctus
nisl blandit. Integer lacinia ante ac libero lobortis imperdiet. Nullam mollis convallis ipsum,
ac accumsan nunc vehicula vitae. Nulla eget justo in felis tristique fringilla. Morbi sit amet
tortor quis risus auctor condimentum. Morbi in ullamcorper elit. Nulla iaculis tellus sit amet
mauris tempus fringilla.
Maecenas mauris lectus, lobortis et purus mattis, blandit dictum tellus.
 Maecenas non lorem quis tellus placerat varius.
 Nulla facilisi.
 Aenean congue fringilla justo ut aliquam.

Mauris id ex erat. Nunc vulputate neque vitae justo facilisis, non condimentum ante
sagittis.
 Morbi viverra semper lorem nec molestie.
 Maecenas tincidunt est efficitur ligula euismod, sit amet ornare est vulputate.
12
10
8
Column 1
6
Column 2
4 Column 3
2
0
Row 1 Row 2 Row 3 Row 4

## Page 2
...

```
```sh
cp sample-presentation.pptx ~/test
```
```sh
docker run -v /c/Users/$USERNAME/test:/tmp -w /tmp -it --rm markitdown sample-presentation.pptx
```
```text
<!-- Slide number: 1 -->
# Sample Presentation
A demo PPTX file from samplelib.com

<!-- Slide number: 2 -->
# Agenda
Introduction
Project goals
Quarterly results
Roadmap
Questions & answers
Lorem ipsum

<!-- Slide number: 3 -->
# Project Goals
Ship the new dashboard
Faster page loads
Mobile-first layout
Grow active users
Lorem ipsum churn
Improve vitae libero

<!-- Slide number: 4 -->
# vitae libero
| Region | Q1 | Q2 | Q3 |
| --- | --- | --- | --- |
| North | 120 | 135 | 150 |
| South | 98 | 102 | 117 |
| East | 143 | 139 | 161 |
| West | 76 | 88 | 94 |

### Notes:
Pellentesque fermentum nisl vitae fringilla venenatis. Etiam id mauris vitae orci maximus ultricies

<!-- Slide number: 5 -->
# Lorem ipsum by Quarter

### Chart

| Category | Product A | Product B |
|---|---|---|
| Q1 | 19.2 | 10.1 |
| Q2 | 21.4 | 14.2 |
| Q3 | 16.7 | 12.0 |
| Q4 | 26.1 | 18.4 |

<!-- Slide number: 6 -->
# Embedded Image

![image.png](Picture2.jpg)

<!-- Slide number: 7 -->
# Pros and Cons
Advantages
• Open standard
• Wide tool support
• Easy to script
Trade-offs
• Verbose XML
• Large file sizes
• Layout drift

<!-- Slide number: 8 -->
Thank you!

```

```sh
cp sample-simple.docx ~/test
```
```sh
docker run -v /c/Users/$USERNAME/test:/tmp -w /tmp -it --rm markitdown sample-simple.docx
```
```text
Sample Document

A demo DOCX file from samplelib.com.

# Introduction

This document shows the common building blocks of a Word file: headings, formatted text, lists, tables and images. Lorem ipsum dolor sit amet, consectetur adipiscing elit. Sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat. Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla pariatur. Excepteur sint occaecat cupidatat non proident, sunt in culpa qui officia deserunt mollit anim id est laborum.

# Text Formatting

Runs can be **bold**, *italic*, <u>underlined</u> or coloured. Font sizes vary too — like this.

# Lists

Bulleted list:

* First bullet point
* Second bullet point
* Third bullet point

Numbered list:

1. First numbered step
2. Second numbered step
3. Third numbered step

# Table

|  |  |  |
| --- | --- | --- |
| **Product** | **Quantity** | **Price** |
| **Notebook** | 3 | $24.00 |
| **Pen** | 12 | $9.60 |
| **Marker** | 5 | $11.25 |

# Image

An embedded raster image:

![](data:image/png;base64...)

In eleifend velit vitae libero sollicitudin euismod. Fusce vitae vestibulum velit. Pellentesque vulputate lectus quis pellentesque commodo. Aliquam erat volutpat. Vestibulum in egestas velit. Pellentesque fermentum nisl vitae fringilla venenatis. Etiam id mauris vitae orci maximus ultricies
```
```sh
cp sample-simple.xlsx ~/test
```
```sh
docker run -v /c/Users/$USERNAME/test:/tmp -w /tmp -it --rm markitdown sample-simple.xlsx
```
```text
## test1
| Lorem ipsum |
| --- |

## test2
| dolor in reprehenderit |
| --- |
```
```sh
cp file_example_PPT_500kB.ppt ~/test
```
```sh
docker run -v /c/Users/$USERNAME/test:/tmp -w /tmp -it --rm markitdown file_example_PPT_500kB.ppt
```
```text
Traceback (most recent call last):
  File "/usr/local/bin/markitdown", line 6, in <module>
    sys.exit(main())
             ~~~~^^
  File "/usr/local/lib/python3.13/site-packages/markitdown/__main__.py", line 259, in main
    result = markitdown.convert(
        args.filename, stream_info=stream_info, keep_data_uris=args.keep_data_uris
    )
  File "/usr/local/lib/python3.13/site-packages/markitdown/_markitdown.py", line 349, in convert
    return self.convert_local(source, stream_info=stream_info, **kwargs)
           ~~~~~~~~~~~~~~~~~~^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  File "/usr/local/lib/python3.13/site-packages/markitdown/_markitdown.py", line 403, in convert_local
    return self._convert(file_stream=fh, stream_info_guesses=guesses, **kwargs)
           ~~~~~~~~~~~~~^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  File "/usr/local/lib/python3.13/site-packages/markitdown/_markitdown.py", line 697, in _convert
    raise UnsupportedFormatException(
        "Could not convert stream to Markdown. No converter attempted a conversion, suggesting that the filetype is simply not supported."
    )
markitdown._exceptions.UnsupportedFormatException: Could not convert stream to Markdown. No converter attempted a conversion, suggesting that the filetype is simply not supported.

```
```sh
cp tf16403429.vsdx ~/test
```
```sh
docker run -v /c/Users/$USERNAME/test:/tmp -w /tmp -it --rm markitdown tf16403429.vsdx
```
```text
Traceback (most recent call last):
  File "/usr/local/bin/markitdown", line 6, in <module>
    sys.exit(main())
             ~~~~^^
  File "/usr/local/lib/python3.13/site-packages/markitdown/__main__.py", line 259, in main
    result = markitdown.convert(
        args.filename, stream_info=stream_info, keep_data_uris=args.keep_data_uris
    )
  File "/usr/local/lib/python3.13/site-packages/markitdown/_markitdown.py", line 349, in convert
    return self.convert_local(source, stream_info=stream_info, **kwargs)
           ~~~~~~~~~~~~~~~~~~^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  File "/usr/local/lib/python3.13/site-packages/markitdown/_markitdown.py", line 403, in convert_local
    return self._convert(file_stream=fh, stream_info_guesses=guesses, **kwargs)
           ~~~~~~~~~~~~~^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  File "/usr/local/lib/python3.13/site-packages/markitdown/_markitdown.py", line 694, in _convert
    raise FileConversionException(attempts=failed_attempts)
markitdown._exceptions.FileConversionException: File conversion failed after 2 attempts:
 - DocxConverterWithOCR threw OSError with message: Could not find main document part. Are you sure this is a valid .docx file?
 - DocxConverter threw OSError with message: Could not find main document part. Are you sure this is a valid .docx file?

```
### Background
```sh
git ls-remote https://github.com/microsoft/markitdown.git \
    'refs/tags/v0.1.8' 'refs/tags/v0.1.8^{}'
```
```sh
git fetch --tags

git show --no-patch --format=fuller v0.1.8

git rev-parse v0.1.8^{commit}
```


### Rationale

Why introduce and maintain another document-extraction technology when Microsoft already owns the formats, maintains the applications that produce them, and provides an extraction solution that can be extended through an established enterprise relationship?

I would sharpen your argument into six points.
Executive rationale for retaining Microsoft MarkItDown
1. Microsoft has the first-party advantage.

Microsoft is the original creator and principal maintainer of the Office document formats and their underlying application-specific implementations.

Other extraction solutions generally fall into three categories:
- Implementations following published specifications, with varying degrees of completeness.
- Implementations reproducing Microsoft behavior through compatibility engineering.
- Reverse-engineered implementations, such as Apache POI, which reconstruct document semantics from the underlying OOXML structures.
These approaches can be technically impressive, but they do not enjoy the same first-party position as Microsoft.

2. Reverse engineering has limited strategic value in this particular enterprise context.

Apache POI is a mature and useful engineering solution. Its value is undeniable when building independent integrations, processing documents without Microsoft dependencies, or conducting forensic inspection.

However, if the objective is ordinary enterprise document ingestion rather than document archaeology, the advantage of reconstructing Microsoft's implementation is considerably less compelling.

Why invest in reproducing behavior when the original vendor is available?

3. SharePoint integration changes the equation.

The in-house MarkItDown port has already been extended substantially to accommodate SharePoint.

This is particularly relevant because enterprise document access is not simply a matter of downloading a file. Authentication, authorization, SSO, and the surrounding access-control model are part of the problem.

The existing implementation has already crossed much of that integration boundary.

Whether it satisfies every client-specific access requirement remains an open question, but it provides a substantial head start over introducing an unrelated extraction framework.

4. PDF extraction is a separate evaluation criterion.

Microsoft has substantial capabilities in the PDF ecosystem as well.

The relative merits of MarkItDown and alternative PDF extraction engines should be assessed independently, rather than assuming that a different Office extraction library automatically offers a better overall solution.

This is an area where comparative testing is justified.

5. Missing format support is a potential vendor roadmap item.

Suppose the current implementation does not adequately support Visio.

That is a legitimate gap, but it is also a candidate for a formal feature request.

The bank may already have a substantial commercial and technical relationship with Microsoft, including established escalation and product-feedback channels.

A feature request to an existing strategic vendor may be more economical than adopting, integrating, testing, and maintaining another technology.

6. Avoid creating an unnecessary internal fork.

This is perhaps the strongest operational argument.

The functionality is already available in an existing Microsoft project. A coding assistant might be prompted to recreate similar functionality, perhaps even producing a reasonably convincing implementation.

But that creates a new responsibility:
- Identify and validate the generated code.
- Establish functional equivalence.
- Submit and maintain the implementation locally.
- Track upstream changes.
- Handle security patches and compatibility regressions.
- Own the resulting technical debt.
What business benefit justifies taking on those responsibilities?

If no material advantage can be demonstrated, the additional implementation is an unnecessary maintenance liability.

The existing in-house MarkItDown port also benefits from substantial SharePoint integration work, including the authentication and authorization boundary. Although client-specific access requirements still require validation, this represents an established integration investment.

Remaining functional gaps, such as Visio support, should first be evaluated as potential extensions or feature requests within the existing Microsoft relationship.

Introducing another extraction framework, or generating an internal alternative through AI-assisted development, would create additional validation, security, compatibility, and maintenance obligations.

The burden of proof should therefore be on the proposed alternative: what measurable capability does it provide that the existing Microsoft-based implementation cannot reasonably deliver?

Absent a convincing answer, extending and maintaining the existing implementation is the more defensible enterprise decision.

**7. Preserve the institutional value of engineering contributions.**

There is also an important distinction between developing a useful capability independently and developing it as an institutional contribution.

If the bank identifies and implements a genuinely valuable enhancement, such as Visio extraction, submitting that work through the bank's established Microsoft relationship is potentially much more valuable than publishing or maintaining an isolated personal contribution.

The bank can bring institutional credibility, an existing commercial relationship, a broader use case, and an opportunity for formal product engagement. Microsoft, in turn, has an incentive to consider improvements that benefit an established enterprise customer and potentially its wider user base.

This creates a preferable outcome: useful engineering work becomes a candidate for upstream adoption rather than another privately maintained fork.

The objective should be to invest engineering effort where its benefits can be shared and sustained, not merely where code can be produced.



### Practical validation: Extracting an executive Kanban diagram from PowerPoint

A recent conversion request provided a useful real-world validation opportunity for the locally deployed Microsoft MarkItDown implementation.

The source PowerPoint contained an executive-level Kanban/state-transition diagram. Although visually resembling a decorated spreadsheet, its actual structure was considerably more sophisticated: colored header rows, breadcrumb-like visual elements, a chessboard-style panel arrangement, and a left-to-right timeline representing causal relationships and state progression.

Rather than constructing a synthetic benchmark, this actual business document was used as test material for the extraction pipeline.

The exercise demonstrated the practical value of having MarkItDown installed locally using vetted enterprise-repository components. Initial execution was performed through Docker to provide a more controlled and auditable deployment boundary, with desktop and virtual-environment execution available as subsequent options.

The important outcome was not merely converting another PowerPoint file. It was demonstrating that an existing, readily deployable extraction technology could be applied immediately to an authentic business-document conversion requirement, without first commissioning a custom parser or introducing another independently maintained framework.

### See Also

  * MarkItDown [package](https://pypi.org/project/markitdown/) - is a Python package and command-line utility for converting various files to Markdown (e.g., for indexing, text analysis, etc)
  * [ottijp/markitdown](https://hub.docker.com/r/ottijp/markitdown/tags)

---
### Author
[Serguei Kouzmine](mailto:kouzmine_serguei@yahoo.com)

