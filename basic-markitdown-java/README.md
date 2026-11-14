### Info 

replica of [DuanYan007/markitdown](https://github.com/DuanYan007/markitdown)  - an Java reimplementation of Microsoft MarkItDown for document-to-Markdown conversion, built as an open-source rewrite project and delivered as a CLI and Java library

Relies on [poi](https://poi.apache.org/apidocs/index.html), 
[pdfbox](https://pdfbox.apache.org/), [tika](https://tika.apache.org)
[jsoup](https://jsoup.org/apidocs/)
 - does not currently use [jsonpath](https://github.com/json-path/JsonPath)


### Usage

```sh
mvn package -DskipTests
```
```sh
java -jar target\example.markitdown4j.jar --help 
```
```text
Usage:
markitdown [-hpqrvV] [--batch] [--examples] [--generate-config]
           [--include-images] [--include-metadata] [--include-tables]
           [--large-file] [--list-formats] [--no-images] [--no-metadata]
           [--no-tables] [--ocr] [--optimize-memory] [--progress]
           [--show-config] [--stats] [--validate-config]
           [--config-path=<configPath>] [-f=<outputFormat>]
           [--image-format=<imageFormat>] [--image-output-dir=<imageOutputDir>]
           [-l=<language>] [-m=<mimeType>] [--max-file-size=<maxFileSize>]
           [--memory-limit=<memoryLimit>] [-o=<output>]
           [--ocr-api-key=<ocrApiKey>] [--ocr-endpoint=<ocrEndpoint>]
           [--ocr-engine=<ocrEngine>] [--ocr-model=<ocrModel>]
           [--ocr-poll-interval=<ocrPollInterval>] [--ocr-timeout=<ocrTimeout>]
           [--pdf-password=<pdfPassword>] [--table-format=<tableFormat>]
           [--temp-dir=<tempDir>] [--threads=<threads>] [<inputFiles>...]

What It Does:
Convert documents to Markdown.

Typical flow:
  1. Choose your input files or pipe content with --mime-type
  2. Set output, content, and OCR options as needed
  3. Use config and diagnostic commands to inspect behavior

Inputs:
      [<inputFiles>...]      Input files or HTTP(S) URLs to convert (optional
                               if using pipe input)

Options (ordered by common tasks):
  -h, --help                 Show this help message and exit.
  -V, --version              Print version information and exit.
  -o, --output=<output>      Output file or directory (default: stdout for
                               pipe, .md file for file input)
  -f, --format=<outputFormat>
                             Output format: markdown, plain, json (default:
                               markdown)
  -m, --mime-type=<mimeType> MIME type for pipe input (e.g., application/pdf)
      --include-images       Include images in the output (default: true)
      --no-images            Exclude images from the output
      --include-tables       Include tables in the output (default: true)
      --no-tables            Exclude tables from the output
      --include-metadata     Include metadata in the output (default: true)
      --no-metadata          Exclude metadata from the output
      --ocr                  Use OCR for text extraction from images
  -l, --language=<language>  Language for OCR (default: auto)
      --ocr-engine=<ocrEngine>
                             OCR engine: tesseract-cli, http, paddleocr
                               (default: tesseract-cli)
      --ocr-endpoint=<ocrEndpoint>
                             Remote OCR endpoint
      --ocr-api-key=<ocrApiKey>
                             Remote OCR API key or token
      --ocr-model=<ocrModel> OCR model name for remote providers
      --ocr-timeout=<ocrTimeout>
                             OCR timeout in milliseconds
      --ocr-poll-interval=<ocrPollInterval>
                             OCR polling interval in milliseconds for async
                               providers
      --table-format=<tableFormat>
                             Table format: github, markdown, pipe (default:
                               github)
      --image-format=<imageFormat>
                             Image format: markdown, html, base64 (default:
                               markdown)
      --image-output-dir=<imageOutputDir>
                             Directory for extracted images relative to output
                               file (default: assets/)
      --max-file-size=<maxFileSize>
                             Maximum file size in bytes (default: 50MB, use 0
                               for unlimited)
      --pdf-password=<pdfPassword>
                             Password for encrypted PDF files
      --large-file           Allow processing of large files (>50MB)
      --temp-dir=<tempDir>   Temporary directory for file operations
  -v, --verbose              Enable verbose output
  -q, --quiet                Suppress all output except errors
  -p, --parallel             Enable parallel processing for multiple files
      --threads=<threads>    Number of threads for parallel processing
                               (default: CPU cores)
      --progress             Show progress bar during conversion
      --stats                Show performance statistics after conversion
      --memory-limit=<memoryLimit>
                             Memory limit in MB for batch processing (default:
                               auto-detect)
      --optimize-memory      Enable memory optimization for large file
                               processing
      --examples             Show usage examples and exit
      --generate-config      Generate default configuration file
      --config-path=<configPath>
                             Path to configuration file
      --validate-config      Validate configuration file
      --show-config          Show current configuration
      --list-formats         List supported MIME types and converters
  -r, --recursive            Recursively process files in directories
      --batch                Batch process all supported files in directory
Examples:
  markitdown document.pdf                       # Convert a PDF to Markdown
  markitdown document.docx -o output.md         # Convert a Word document
  markitdown presentation.pptx --no-tables      # Convert a PowerPoint without
tables
  markitdown spreadsheet.xlsx --ocr             # Convert an Excel file with OCR
  markitdown *.pdf                              # Convert all PDFs in the
directory
  markitdown *.pdf --parallel                   # Convert multiple PDFs in
parallel
  markitdown https://example.com/report.pdf -o out/ # Download a URL and
convert it
  cat document.pdf | markitdown                 # Convert from stdin
  curl -s http://example.com/doc.pdf | markitdown  # Convert a remote document
stream

```
### See Also:


  * [elbruno/MarkItDownServer](https://github.com/elbruno/MarkItDownServer) - an FastAPI web server exposing the markitdown functionality to convert files to markitdown through REST API
  * [dezoito/markitdown-api](https://github.com/dezoito/markitdown-api) - ltra lightweight API server to convert files (`.pdf`, `.docx`, `.xlsx`) into formatted markdown
  * [basic-poislideshow](https://github.com/sergueik/springboot_study/tree/master/basic-poislideshow)
  * [scanner/searcher](https://github.com/sergueik/powershell_samples/tree/master/csharp/basic-budget-searcher)

### Author
[Serguei Kouzmine](kouzmine_serguei@yahoo.com)


