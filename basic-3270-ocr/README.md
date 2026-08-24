### Usage
```cmd
pushd java
mvn package
java -jar target\example.teller-screen.jar -screenfile ..\input\example.txt -outputfile ..\images\console1.png
popd
```
```text
Wrote ..\images\console1.png

```

![3270 Console](images\console1.png)

```cmd
pushd csharp/windows-forms
cd Program/bin/Debug
.\teller_screen.exe -screenfile=..\..\..\..\..\input\example.txt -outputfile=..\..\..\..\..\images\console2.png
popd
```
```text
Wrote "..\..\..\..\..\images\console2.png"
```

![3270 Console](images\console2.png)

```
python teller_screen.py --screenfile ..\input\example.txt --outputfile ..\images\console3.png
```
```text
Wrote ..\images\console3.png
```

![3270 Console](images\console3.png)

```sh
docker pull minidocks/imagemagick
docker pull jitesoft/tesseract-ocr
```

> NOTE not using "$HOME" - it may easily be pointing to SMB drive
```
export WORKDIR=/c/Users/$USERNAME/Documents/images
mkdir -p $WORKDIR
```
```
cp images/console1.png  $WORKDIR/input.png

docker run --rm -v "$WORKDIR:/work:Z" minidocks/imagemagick magick /work/input.png $OPTIONS_STRING /work/prepared.png
```
```
ls $WORKDIR/prepared.png
/c/Users/kouzm/Documents/images/prepared.png
```
```
docker run --rm -v "$WORKDIR:/work:Z" jitesoft/tesseract-ocr /work/prepared.png /work/result
```
```text
libgomp: Thread creation failed: Operation not permitted
ObjectCache(0x7fc48fe10300)::~ObjectCache(): WARNING! LEAK! object 0x561019774230 still has count 1 (id /usr/local/share/tessdata/eng.traineddatalstm-punc-dawg)
ObjectCache(0x7fc48fe10300)::~ObjectCache(): WARNING! LEAK! object 0x5610198b2040 still has count 1 (id /usr/local/share/tessdata/eng.traineddatalstm-word-dawg)
ObjectCache(0x7fc48fe10300)::~ObjectCache(): WARNING! LEAK! object 0x5610198b1fd0 still has count 1 (id /usr/local/share/tessdata/eng.traineddatalstm-number-dawg)
```

pick the older less aggressive release
```sh
docker pull jitesoft/tesseract-ocr:5.4.1-alpine
```

```sh
docker run --rm -v "$WORKDIR:/work:Z" jitesoft/tesseract-ocr:5.4.1-alpine /work/prepared.png /work/result
```

```text
Error loading shared library libtiff.so.5: No such file or directory (needed by /usr/local/lib/libleptonica.so.6)
Error loading shared library libwebpmux.so.3: No such file or directory (needed by /usr/local/lib/libleptonica.so.6)
Error relocating /usr/local/lib/libleptonica.so.6: TIFFPrintDirectory: symbol not found
...
```

pick the yet older release
```sh
docker pull jitesoft/tesseract-ocr:5.3.3-alpine
```

```sh
docker run --rm -v "$WORKDIR:/work:Z" jitesoft/tesseract-ocr:5.4.1-alpine /work/prepared.png /work/result
cat "$WORKDIR/result.txt"
```
```text
Estimating resolution as 173
```

```
ADIPISCING:
SED:

LABORE:

DOLORE:

PF1=HELP PF2=SPLIT PF3=
PF=!

PF7=UP PF8=DOWN

MOCK L&F

IPSUM: ______
AMET:
CONSECTETUR: ________
ELIT:
TEMPOR: ~______
UT: Li
ET: ~
MAGNA

END PF4=RETURN PFS=RFIND PFO=RCHANGE
SWAP PF1IQ=LEFT PF11=RIGHT PF12=RETRIEVE
```

### Troubleshooting

The `jitesoft/tesseract-ocr:latest` image failed in __Docker Toolbox__ with:

```text
libgomp: Thread creation failed: Operation not permitted
```

An older `:jessie` image was tried next, based on the assumption that an older Debian-based image might not yet exhibit 
the aggressive threading/runtime behavior. But it was - that did not resolve the problem.

Next, `:5.4.1-alpine` was tried, but it failed because of incompatible or missing `TIFF/WebP` runtime libraries required by `Leptonica`.

Finally, the `:5.3.3-alpine` worked and successfully produced the __OCR__ output.

### See Also

### Author
