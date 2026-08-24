### Usage

This experiment evaluates whether a simple OCR pipeline (ImageMagick + Tesseract) can extract reusable business knowledge from workflow diagrams


```
docker pull minidocks/imagemagick
docker pull jitesoft/tesseract-ocr
```

* run code to generate 3270 screen mock for a given sceen text input
* run code to improve the bitmap quality with Image Magick and OCR with tecerct
* evaluate, adjust model paramers

=>  able to construct intelligent `ALT` text


#### Components of the Pipeline


* Labeled 3270 Screen Generator: custom

| Implementation | Platform / OS | Status |
|---|---|---|
| Java | Ubuntu | ✅ |
| C# / Windows Forms | Windows | ✅ |
| C# / Xwt | Ubuntu / Mono / GTK | ✅ |
| Python | Ubuntu | ✅ |

* Image Quality Management: [ImageMagick](https://en.wikipedia.org/wiki/ImageMagick)
* OCR: [Tesseract](https://en.wikipedia.org/wiki/Tesseract_(software))


### 3270 Terminal Text Extaction

a.k.a. OCR'ing teller screens

Font shape and spacing is important. One may need to find a close match to fonts used by Blue Prism , UiPath - these are likely resular True Type fonts. For the purpose of exercise take a public 3270 fonts

```sh
apt-get install fonts-3270
```
apt source entry to add if not present:

|Distro | Package                                        | repo|
|-------|------------------------------------------------|-----|
|Debian |https://packages.debian.org/sid/fonts/fonts-3270| https://packages.debian.org/sid/fonts/fonts-3270|
|Ubuntu |http://packages.ubuntu.com/impish/fonts-3270| https://packages.ubuntu.com/impish/fonts/fonts-3270|

direct
```sh
BASE_URL='https://github.com/ryanoasis/nerd-fonts'
curl -skLo ~/Downloads/3270NerdFontMono-Regular.ttf "$BASE_URL/raw/refs/heads/master/patched-fonts/3270/3270NerdFontMono-Regular.ttf"
```
switch to Windows console (elevated)
```cmd
set FILENAME=3270NerdFontMono-Regular.ttf
copy /y "%USERPROFILE%\Downloads\%FILENAME%" "C:\Windows\Fonts\"
set FONT_NAME=3270 Nerd Font Mono
reg.exe add "HKLM\SOFTWARE\Microsoft\Windows NT\CurrentVersion\Fonts" /v "%FONT_NAME% (TrueType)" /t REG_SZ /d "%FILENAME%" /f
```
> NOTE there is quite a lot of 3270 fonts in the dir
> NOTE: the direct s3 link suggested in `https://github.com/rbanffy/3270font` no longer works:
> ```sh
> curl -skLo ~/Downloads/fonts-3270.zip https://3270font.s3.amazonaws.com/3270_fonts_d916271.zip
> ```

![CICS login screen mock](images/console1.png)

```sh
./scan_screenshot.sh images/console1.png
```
result is printed to console:
```text
Estimating resolution as 172
MOCK MAINFRAME LOGIN SCREEN
USER ID) ===> _
PASSWORD ===> _.

PF3=EXIT ENTER=CONT INUE
```


```sh
mvn package
pushd images
java -jar ../target/example.teller-screen.jar
popd
```

![CICS login screen mock](images/console2.png)

```sh
./scan_screenshot.sh images/console2.png
```
result is printed to console:
```text
MOCK MAINFRAME LOGIN SCREEN

USER ID
PASSWORD ===

PF3=EXIT ENTER=CONT INUE
```
```
sudo apt-get install ttf-mscorefonts-installer
```
```sh
export FONT_PATH=/usr/share/fonts/truetype/msttcorefonts/Courier_New.ttf
```
```sh
mvn package
pushd images
java -jar ../target/example.teller-screen.jar
popd
```
![CICS login screen mock](images/console3.png)

```sh
./scan_screenshot.sh images/console3.png
```


```text
Estimating resolution as 184
MOCK MAINFRAME LOGIN SCREEN

USER ID
PASSWORD ===>

PF3=EXIT ENTER=CONT INUE
```
__close inspection__:

|defect |explanation |
|-------|------------|
|`ID)` instead of `ID` | glyph/spacing recognition issue|
|underscore runs collapsing to single `_` | exactly the kind of character/grid issue worth investigating.
|`_ .` after PASSWORD | likely rendering/segmentation interaction|
|extra space in `CONT INUE` | character spacing / word segmentation|



### Cleanup
```
docker image rm jitesoft/tesseract-ocr:latest minidocks/imagemagick:latest
```
###  ### Background / FaaS analogy

Although this project is primarily a small cross-platform drawing/rendering
experiment, its structure is closely analogous to a common FaaS textbook
scenario: an event-driven image-processing function.

In the typical example, an image upload (or an embedded image discovered while
processing a document) triggers a function which processes the image and
produces a derived artifact. Depending on the example, that processing may
include:

* generating a thumbnail / scaling the image;
* discovering and extracting image metadata;
* tagging or classifying the image;
* applying privacy-oriented transformations, such as blurring detected
  sensitive regions.

This project follows the same basic pattern at a much smaller scale:

```text
input data
    │
    ▼
event / invocation
    │
    ▼
renderer
    │
    ├── font/layout processing
    ├── drawing
    └── image generation
    │
    ▼
PNG artifact
```
This project follows the same general idea, but uses the image-processing
pipeline to investigate a different problem: extracting reusable business
knowledge from screenshots and workflow diagrams.

The processing can be viewed conceptually as:

```text
image / document
       │
       ▼
   extraction event
       │
       ▼
 image-processing pipeline
       │
       ├── image normalization / scaling
       ├── OCR
       ├── diagram / screen interpretation
       └── other image analysis
              │
              ▼
       reusable knowledge
```

### Controlled image generation and possible ML use

The 3270 terminal renderer is also intended as a controlled image generator for
a possible later machine-learning experiment.

The C# implementation was originally copied from the Windows Forms version
deliberately. Applications such as Blue Prism, Pega and UiPath may use drawing
and text-rendering APIs that are closer to the Windows/.NET rendering stack
than the Java or Python implementations. Keeping that implementation provides
a potentially useful rendering path for generating images that resemble the
screens encountered by the business applications themselves.

The Java, C# and Python implementations can therefore serve as independent
generators of labeled synthetic training data. This has an important practical
advantage over collecting screenshots from the actual RPA products: the
generator can produce controlled variations that would be difficult or
impractical to obtain from the products themselves.

Potential image-generation parameters include:

* font selection and font characteristics;
* rendering backend;
* image quality and rasterization;
* character spacing and alignment;
* CICS screen wording;
* screen length and density;
* deliberately introduced rendering imperfections.

For example, longer and denser terminal screens are expected to provide a
progressively harder OCR problem. Rather than assuming that relationship,
synthetic data makes it possible to generate a range of densities and measure
the resulting effect on OCR and ML performance.

Conceptually:

```text
                    synthetic screen specification
                              │
             ┌────────────────┼────────────────┐
             ▼                ▼                ▼
          Java             C# / Xwt          Python
        renderer          renderer(s)        renderer
             │                │                │
             └────────────────┼────────────────┘
                              ▼
                         console.png
                              │
                              ▼
                    ImageMagick / Tesseract
                              │
                              ▼
                    labeled / extracted data
                              │
                              ▼
                       ML / Jupyter
                              │
                              ▼
                  training / evaluation
```


### Accessibility as a target domain

Accessibility is an important target domain for this work.

A screenshot of a terminal screen, workflow diagram, scanned document, or other
image may contain information that is effectively unavailable to a visually
impaired consumer unless an equivalent textual representation is provided.

An OCR and image-understanding pipeline can potentially produce substantially
richer alternative text than the traditional generic:

> "Image"

For example, instead of merely identifying an image as a screenshot, the
pipeline could describe the meaningful content of a 3270/CICS screen:

```text
CICS login screen XY01.
The screen contains input fields for USER ID and PASSWORD,
with PF3 assigned to EXIT and ENTER assigned to CONTINUE.
```

From a manager/business-owner perspective, the question may be very simple:

What inputs does this screen take?

But obtaining the answer has a nontrivial human cost:
```
Question
   │
   ▼
Identify screen owner
   │
   ▼
Obtain permission to contact them
   │
   ▼
Find time when they are available
   │
   ▼
Explain what you are asking
   │
   ▼
Owner examines / remembers the screen
   │
   ▼
Answer
```
The expensive part isn't necessarily the knowledge. It is getting access to the person who possesses the knowledge.

And the owner may be:
  * busy
  * changed team
  * someone you have never met
  * unavailable when you need the answer
  * reluctant to interrupt their current work for what appears to be a small question

That makes the image itself potentially valuable as a knowledge-recovery interface. If the system can infer:
```
USER ID
PASSWORD
PF3 = EXIT
ENTER = CONTINUE
```
then you have already answered a surprisingly useful part of the manager's question without scheduling the meeting.


The value isn't only "extract information from important documents." It is also "cheaply determine which artifacts are not worth further human attention."
### Inventory and triage

Another possible outcome is deliberately negative:

> "We do not need to worry about this document."

For business-critical operations, a human-to-human conversation with the
application owner or subject-matter expert will eventually be appropriate.
The purpose of automated extraction is not to eliminate that conversation
where authoritative knowledge is required.

The situation is different for the large population of low-value,
obsolete, redundant, or effectively disposable artifacts. Spending human
time identifying and classifying each of them may cost more than the value of
the information they contain.

An automated image/OCR pipeline can therefore be useful even when its only
conclusion is that an artifact does not warrant further investigation:

```text
artifact
   │
   ▼
automated extraction / classification
   │
   ├── potentially important ──► human review / owner
   │
   └── apparently disposable ──► no further action

```
### The cost of manual archaeology

A practical motivation for the experiment came from a real work situation:
an FTE subject-matter expert reported that they had spent the previous week
studying diagrams and screenshots to determine where a particular bot was
mentioned and where it was not.

The resulting observation may be very small:

> Bot Z is mentioned here, but not there.

But establishing that fact can require a substantial amount of manual
archaeology when the source material consists of screenshots, diagrams,
embedded images, and other documents that are not readily searchable.

This is an important distinction for the project. The expensive part is not
necessarily understanding the final answer. It is finding and examining the
artifacts from which the answer can be derived.

An automated extraction and indexing pass could make that first stage much
cheaper:

```text
documents / screenshots / diagrams
              │
              ▼
       automated extraction
              │
              ▼
       searchable inventory
              │
       ┌──────┴──────┐
       ▼             ▼
  "probably nothing"  "interesting"
       │             │
       ▼             ▼
    move on       SME / owner
```

### Accessibility and knowledge recovery

Accessibility is one potential application of the extracted information.

A screenshot may contain information that is not otherwise readily available
to a consumer or analyst. Even a relatively simple question such as "what
inputs does this screen take?" may require contacting the application owner
or subject-matter expert. The knowledge itself may be simple, while obtaining
it can involve identifying the appropriate person, obtaining permission to
ask, finding time to talk, and waiting for an answer.

An image-understanding pipeline provides a complementary way to recover such
information directly from existing screenshots and other visual artifacts.

For a 3270/CICS screen, for example, recognizing that the screen contains a
USER ID and PASSWORD input and that PF3 and ENTER have specific functions can
already provide useful business information, independently of whether the
ultimate consumer is a human analyst, an accessibility tool, or an ML
pipeline.

This does not replace the application owner as the authoritative source.
Rather, it can reduce the number of questions that need to be escalated to
the owner and provide useful context before that conversation takes place.

That last sentence is important. The system doesn't have to "know the business" perfectly to be useful.

It can turn:

"I have a screenshot of some mysterious legacy screen; who owns this thing?"

into:

"This appears to be a login screen with USER ID and PASSWORD fields and PF3/ENTER actions. I now have enough context to ask the owner a much more precise question."

And that is a very plausible benefit even before getting anywhere near sophisticated ML.






#### Control experiment: plain text

![plain text](images/text.png)

```sh
./scan_screenshot.sh images/text.png
```
```text
Lorem ipsum dolor sit amet, consectetuer
adipiscing elit. Maecenas porttitor congue
massa. Fusce posuere, magna sed pulvinar
ultricies, purus lectus malesuada libero, sit
amet commodo magna eros quis urna.
```

A workflow diagram is not primarily a text artifact. Even a simple "no fancy" workflow diagram typically contains far more structural information than textual information. The labels may represent only a small fraction of the artifact, while the majority of the representation describes shapes, connectors, positioning, grouping, and visual relationships

In addition, many workflow formats are shared with graphical IDEs or modeling tools. The file may contain a significant amount of authoring metadata: coordinates, layout information, editor state, object identifiers, style definitions, and other information required by the tool. This information is useful to the authoring environment but has little direct business meaning.

OCR extracts the visible labels, but it discards the very information that makes the diagram a process: the topology and relationships
### Challenges
#### Challenge 1: workflow diagram
`diagram1.mermaid`:
```code
flowchart LR

Start((Order Received))
Cond1{"Customer account active?"}
Cond2{"Order value exceeds $10,000?"}
Then1["Create shipment"]
Then2["Request manager approval"]
Else["Reject order and notify customer"]
End((Process Complete))
Start --> Cond1
Cond1 -->|Yes| Cond2
Cond1 -->|No| Else
Cond2 -->|Yes| Then2
Cond2 -->|No| Then1
Then1 --> End
Then2 --> End
Else --> End
```
![diagram1](images/diagram1.png)

Can OCR recover the business process from the rendered diagram?

```sh
./scan_screenshot.sh images/diagram1.png
```
```text
Order value exceeds $10,000?

mplete |

/
```
Activity labels were discovered, but decision logic and branching were gone.

#### Challenge 2: business process

`diagram2.mermaid`:
```code
flowchart LR

Start([Purchase request received])
Decision{"Has the purchasing manager approved the request?"}
Approve["Create purchase order and notify supplier"]
Reject["Reject request and notify requestor with explanation"]
End([Process completed])

Start --> Decision
Decision -->|Yes| Approve
Decision -->|No| Reject
Approve --> End
Reject --> End
```
![plain text](images/diagram2.png)

```sh
./scan_screenshot.sh images/diagram2.png
```
```text
Create purchase order and notify supplier

Reject request and notify requestor with explanation

Process completed
```

Observation:
The extracted text reads like a business process description,
but it no longer conveys the rule that determines which path is executed

### Findings

|Business information |OCR recovered |
|-----------------|-----------|
|Activity labels       | 	✅ Mostly    |
|Decision conditions |	⚠️ Partial|
|Branching logic	 |❌         |
|Sequence	         |❌         |
|Process semantics	 |❌         |

### Note

A typical workflow diagram contains far more structural information than textual information. The labels may occupy only a small fraction of the artifact, while the majority of the representation describes geometry: nodes, edges, positions, grouping, routing, and visual relationships. OCR extracts mainly the labels and discards most of the structure.

A workflow diagram is already a highly optimized visual representation. The majority of its information content is *not* text.

A large fraction of the file representation may be:
  * geometry (coordinates, bounding boxes, connectors),
  * rendering instructions,
  * editor metadata,
  * collaboration/versioning information,
  * application-specific state

### Troubleshooting

```text
Error, could not create TXT output file: Permission denied
cat: /tmp/tmp.Emwe1AX3aD/result.txt: No such file or dir
```
check the image configuration
```sh
docker inspect jitesoft/tesseract-ocr | grep -i User
```

```text
"User": "tesseract",
```

### Troublshooting

missing dependency - :
```text
java -jar target/example.teller-screen.jar

Exception in thread "main" java.lang.UnsatisfiedLinkError:
Can't load library: /usr/lib/jvm/java-11-openjdk-amd64/lib/libawt_xawt.so
at java.base/java.lang.ClassLoader.loadLibrary(ClassLoader.java:2638)
at java.base/java.lang.Runtime.load0(Runtime.java:768)
```
```sh
find /usr/lib/jvm -name 'libawt_xawt.so'
```
machine has no jdk, only heaadless jre:
```sh
java -version
```
```text
openjdk version "11.0.31" 2026-04-21
OpenJDK Runtime Environment (build 11.0.31+11-post-1ubuntu1-22.04.2-Ubuntu)
OpenJDK 64-Bit Server VM (build 11.0.31+11-post-1ubuntu1-22.04.2-Ubuntu, mixed mode, sharing)
```
```sh
readlink -f "$(which java)"
```
```text
/usr/lib/jvm/java-11-openjdk-amd64/bin/java
```
```sh
dpkg -l | grep openjdk
```
```text
ii  openjdk-11-jre-headless:amd64           11.0.31+11-1ubuntu1~22.04.2                      amd64        OpenJDK Java runtime, using Hotspot JIT (headless)
```
```sh
sudo apt install openjdk-11-jdk
```

The `libawt` error goes away
but now need to tune the code to use
```text
ii  fonts-3270     2.3.1-1      all          monospaced font based on IBM 3270
```

```sh
dpkg -L fonts-3270
```
```text
/.
/usr
/usr/share
/usr/share/doc
/usr/share/doc/fonts-3270
/usr/share/doc/fonts-3270/README.md.gz
/usr/share/doc/fonts-3270/changelog.Debian.gz
/usr/share/doc/fonts-3270/copyright
/usr/share/fonts
/usr/share/fonts/opentype
/usr/share/fonts/opentype/3270
/usr/share/fonts/opentype/3270/3270-Regular.otf
/usr/share/fonts/opentype/3270/3270Condensed-Regular.otf
/usr/share/fonts/opentype/3270/3270SemiCondensed-Regular.otf
/usr/share/metainfo
/usr/share/metainfo/fonts-3270.metainfo.xml
```
getting via curl yields exotic error:
```sh
curl -skI https://mochasoft.dk/images/tn3270_1.png
```
```text
455 is not a standard status code defined by the core HTTP specifications. Here the useful clues are:

HTTP/2 455
server: Microsoft-IIS/10.0
content-length: 54

followed by:

The custom error module does not recognize this error

That strongly suggests the IIS site/application (or something in front of it) is deliberately generating a nonstandard 455 response, rather than the PNG simply being unavailable in the ordinary 404 Not Found sense.
```
### Next Step
S0 = pristine synthetic 3270-looking screen

Then create:

S1 = slightly overexposed
S2 = noisy
S3 = reduced contrast
S4 = brown on dark gray
S5 = vintage / color-shifted
S6 = slight character-position jitter


![CICS login screen mock](images/console5.png)


```sh
./scan_screenshot.sh images/console5.png
```
```txt
Estimating resolution as 185
=>
ADIPISCING:
SED:

ENIM:
MINIM:
QUIS:
COMMODO :

PF1=HELP PF2=SPLIT
PF 7=UP

LORE-MF

IPSUM:
AMET:
CONSECTETUR:
ELIT:
DO:
AD:
VENIAM:
NOSTRUD:
CONSEQUAT:

PF3=END PF4=RETURN PFS=RFIND PF6=RCHANGE
WAP PF1O=LEFT PF11=RIGHT PF12=RETRIEVE


```

```
cat example.text
```
```
                        MOCK L&F

LOREM:  ___________                         IPSUM: _____________

DOLOR:  __________                 AMET:  _____________

AMET ====> _____________                 CONSECTETUR: _________

ADIPISCING: __________      ELIT: __________
SED: _________                              DO: _______________

EIUSMOD: _____________       TEMPOR: _____________

INCIDIDUNT: _________                     UT: _________

LABORE: _____________        ET: _____________

DOLORE: _________          MAGNA: _____________

ALIQUA: _____________       UT: __________

ENIM: _____________                  AD: __________

MINIM: _____________       VENIAM: _____________

QUIS: _____________                 NOSTRUD: ___________

EXERCITATION: _________          ULLAMCO: _____________

LABORIS: _____________       NISI: __________

UT: _____________                  ALIQUIP: ___________

EX: _____________          EA: _____________

COMMODO: _____________       CONSEQUAT: __________

                         ...

			 PF1=HELP  PF2=SPLIT  PF3=END  PF4=RETURN  PF5=RFIND  PF6=RCHANGE
			 PF7=UP    PF8=DOWN   PF9=SWAP  PF10=LEFT  PF11=RIGHT  PF12=RETRIEVE

```
```
mvn package
mkdir results
```
```sh
java -jar target/example.teller-screen.jar  -screenfile example.text  -outputfile images/console.png
```
```sh
./scan_screenshot.sh  images/console.png | tee results/console.txt /dev/stderr
```
```text
Estimating resolution as 179
MOCK L&F

LOREM: IPSUM:
DOLOR: AMET:

AMET =

ADIPISCING: __________ ELIT: ~_________

SED: ______-__ DO: _______________
EIUSMOD: _____________ TEMPOR: __________
INCIDIDUNT: _________ UT: ~W
LABORE: _____________ ET: ~----_--

DOLORE: _________ MAGNA: ___--

ALIQUA: UT:

ENIM: AD:

MOCK L&F

LOREM: IPSUM:
DOLOR: AMET:

AMET =

ADIPISCING: __________ ELIT: ~_________

SED: ______-__ DO: _______________
EIUSMOD: _____________ TEMPOR: __________
INCIDIDUNT: _________ UT: ~W
LABORE: _____________ ET: ~----_--

DOLORE: _________ MAGNA: ___--

ALIQUA: UT:

ENIM: AD:

```

### Python Corner
```sh
sudo python3 -m pip install pillow
```
```text
Requirement already satisfied: pillow in /usr/lib/python3/dist-packages (9.0.1)
```

```sh
python3 teller_screen.py --screenfile example.txt --outputfile images/console6.png --textfile console6.txt
```

![Python Generated, Default Font](screenshots/console6.png)

```sh
./scan_screenshot.sh  images/console6.png "-channel RGB -negate" | tee results/console.txt /dev/stderr
```
```
Estimating resolution as 180
Detected 11 diacritics
MOCK L&F

LOREM:

DOLOR: AMET:
AMET ====> ___-

QPIPISCING

INCIDIDUNT:
LABORE:
DOLORE:
ALIQUA:
ENIM:
MINIM:
Quis: NOSTRUD:
EXERCITATION: ULLAMCO:
LABORIS: __9- NISI: __-_--_e
UT: ALIQUIP: _-o
EX: _ EA:

BESEHE HELP BE: =SPLIT BESZENR PE4=RETURN BE? REIND PE eT RCHANGE
PFr=UP =D PF4=SWAP PFIQ=LEFT PF11=RIGHT PF12=RETRIEVE
MOCK L&F

LOREM:

DOLOR: AMET:
AMET ====> ___-

QPIPISCING

INCIDIDUNT:
LABORE:
DOLORE:
ALIQUA:
ENIM:
MINIM:
Quis: NOSTRUD:
EXERCITATION: ULLAMCO:
LABORIS: __9- NISI: __-_--_e
UT: ALIQUIP: _-o
EX: _ EA:

BESEHE HELP BE: =SPLIT BESZENR PE4=RETURN BE? REIND PE eT RCHANGE
PFr=UP =D PF4=SWAP PFIQ=LEFT PF11=RIGHT PF12=RETRIEVE

```
```sh
cat console6.txt
```
```text
                         MOCK L&F

LOREM:  ___________                         IPSUM: _____________

DOLOR:  __________                 AMET:  _____________

AMET ====> _____________                 CONSECTETUR: _________

ADIPISCING: __________      ELIT: __________
SED: _________                              DO: _______________

EIUSMOD: _____________       TEMPOR: _____________

INCIDIDUNT: _________                     UT: _________

LABORE: _____________        ET: _____________

DOLORE: _________          MAGNA: _____________

ALIQUA: _____________       UT: __________

ENIM: _____________                  AD: __________

MINIM: _____________       VENIAM: _____________

QUIS: _____________                 NOSTRUD: ___________

EXERCITATION: _________          ULLAMCO: _____________

LABORIS: _____________       NISI: __________

UT: _____________                  ALIQUIP: ___________

EX: _____________          EA: _____________

COMMODO: _____________       CONSEQUAT: __________

                         ...

PF1=HELP  PF2=SPLIT  PF3=END  PF4=RETURN  PF5=RFIND  PF6=RCHANGE
PF7=UP    PF8=DOWN   PF9=SWAP  PF10=LEFT  PF11=RIGHT  PF12=RETRIEVE

```
> NOTE: visually it is clear that the quality of screen image from Python generator is inferior compared to Java due primarily less advanced font metric and appropriate line-spacing calculation calculation

#### Next Steps

Add a Python loop around it with a more ML oriented options - labeling, exloring all ML options

```
3270/Courier-like glyph geometry
        +
hard/pixel-ish rasterization
        +
yellow / gray-ish phosphor foreground
        +
very dark blue/black background
        +
display/photo/capture artifacts
        +
color fringing at high-contrast edges
```
```sh
              logical screen
                    │
                    ▼
             font + metrics
                    │
                    ▼
              rasterization
                    │
          ┌─────────┴─────────┐
          │                   │
     rendering hint       color scheme
          │                   │
          └─────────┬─────────┘
                    ▼
              pristine PNG
                    │
                    ▼
             degradation
          ┌─────────┼─────────┐
          │         │         │
       noise    exposure   color fringe
          │         │         │
          └─────────┼─────────┘
                    ▼
               OCR input
```
### Unrelated

The next step is understanding what information exists, categorizing it, and identifying the relationships that make it valuable.

---

### See Also
  * https://github.com/mono/xwt
  * https://www.bollynook.com/en/lyrics/19270/urvasi/
### Author
[Serguei Kouzmine](kouzmine_serguei@yahoo.com)
