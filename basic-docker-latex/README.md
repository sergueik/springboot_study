### Info
Custom image based on [TeX Live](https://tug.org/texlive/) downgraded from base image Debian [Sid](https://www.debian.org/releases/sid/) a.k.a. "Unstable"  development and testing branch of the OS to resolve the challenge with aging Docker ToolBox.
### Intermediate

It turns out that the published tag `texlive/texlive:TL2015-historic-tree` is not a runnable operating-system image at all.


```sh
docker inspect texlive/texlive:TL2015-historic-tree | jq '.[0].Config.Cmd,.[0].Config.Etrypointm,.[0].Os, .[0].Architecture'
```
```text
null
null
"linux"
"amd64"
```
```sh
docker run --rm texlive/texlive:TL2015-historic-tree latex --version
```
```text
C:\Program Files\Docker Toolbox\docker.exe: 
Error response from daemon: 
OCI runtime create failed: 
container_linux.go:349: 
starting container process caused "exec: \"latex\": 
executable file not found in $PATH": unknown.
```
same with
```sh
docker run --rm -it texlive/texlive:TL2015-historic-tree sh
```
```sh
cat /etc/os-release
cat /etc/debian_version
```

The fact is some __TeX Live__ Docker artifacts were published as data/container filesystem trees rather than complete runtime environments

Therefore the `TL2015-historic-tree` is probably the wrong artifact to construct a historically frozen TeX Live environment

### Take 2

```sh
docker pull mfisherman/texlive-minimal:latest
```

```sh
docker run --rm -it mfisherman/texlive-minimal:latest sh
```
```sh
apk add inkscape
```
success:
```text
OK: 661 MiB in 280 packages
```
```sh
apk add openjdk17-jre graphviz
```
success:
```text
OK: 845 MiB in 295 packages
```
```sh
dot -v
```
```text
dot - graphviz version 12.2.1 (20241206.2353)
```
```sh
java -version
```

```text
openjdk version "17.0.19" 2026-04-21
```
```sh
cat /etc/os-release
uname -
a
df -h
df -i
```

```text
Linux 58b31ac6f2eb 4.19.130-boot2docker #1 SMP Mon Jun 29 23:52:55 UTC 2020 x86_64 Linux
```
```text
Filesystem                Size      Used Available Use% Mounted on

overlay                  17.8G      4.2G     12.7G  25% /
tmpfs                    64.0M         0     64.0M   0% /dev
tmpfs                     1.9G         0      1.9G   0% /sys/fs/cgroup
shm                      64.0M         0     64.0M   0% /dev/shm
/dev/sda1                17.8G      4.2G     12.7G  25% /etc/resolv.conf
/dev/sda1                17.8G      4.2G     12.7G  25% /etc/hostname
/dev/sda1                17.8G      4.2G     12.7G  25% /etc/hosts
tmpfs                     1.9G         0      1.9G   0% /proc/asound
tmpfs                     1.9G         0      1.9G   0% /proc/acpi
tmpfs                    64.0M         0     64.0M   0% /proc/kcore
tmpfs                    64.0M         0     64.0M   0% /proc/keys
tmpfs                    64.0M         0     64.0M   0% /proc/timer_list
tmpfs                     1.9G         0      1.9G   0% /proc/scsi
tmpfs                     1.9G         0      1.9G   0% /sys/firmware

```
```text
Filesystem              Inodes      Used Available Use% Mounted on
overlay                2434064    116262   2317802   5% /
tmpfs                   504954        17    504937   0% /dev
tmpfs                   504954        13    504941   0% /sys/fs/cgroup
shm                     504954         1    504953   0% /dev/shm
/dev/sda1              2434064    116262   2317802   5% /etc/resolv.conf
/dev/sda1              2434064    116262   2317802   5% /etc/hostname
/dev/sda1              2434064    116262   2317802   5% /etc/hosts
tmpfs                   504954         1    504953   0% /proc/asound
tmpfs                   504954         1    504953   0% /proc/acpi
tmpfs                   504954        17    504937   0% /proc/kcore
tmpfs                   504954        17    504937   0% /proc/keys
tmpfs                   504954        17    504937   0% /proc/timer_list
tmpfs                   504954         1    504953   0% /proc/scsi
tmpfs                   504954         1    504953   0% /sys/firmware
```

```sh
apk add xvfb
```
success
```text
OK: 1167 MiB in 320 packages
```
```sh
apk add plantuml
```
success
```text
OK: 1193 MiB in 322 packages
```
```sh 
plantuml -version
```
```text
PlantUML version 1.2025.2 (Tue Jan 07 17:35:36 GMT 2025)
(GPL source distribution)
Java Runtime: OpenJDK Runtime Environment
JVM: OpenJDK 64-Bit Server VM
Default Encoding: UTF-8
Language: en
Country: US

PLANTUML_LIMIT_SIZE: 4096

Dot version: dot - graphviz version 12.2.1 (20241206.2353)
Installation seems OK. File generation OK
```
```sh
 docker build -t docker-latex-alpine -f Dockerfile.alpine  .
```
`test.tex`:
```tex
\documentclass{article}
\usepackage{graphicx}

\begin{document}

\immediate\write18{plantuml -tpng testuml.tex}

\includegraphics{testuml.png}

\end{document}
```
`testuml.tex`:
```tex
@startuml
Alice -> Bob : Hello
@enduml
```
```sh
lualatex --shell-escape test.tex
```
```text
[Loading MPS to PDF converter (version 2006.09.02).]
) (/usr/share/texmf-dist/tex/latex/epstopdf-pkg/epstopdf-base.sty
(/usr/share/texmf-dist/tex/latex/latexconfig/epstopdf-sys.cfg))
plantuml -tpng testuml.tex

[1{/usr/share/texmf-var/fonts/map/pdftex/updmap/pdftex.map}<./testuml.png>]
(./test.aux))
 406 words of node memory still in use:
   3 hlist, 1 vlist, 1 rule, 2 glue, 3 kern, 1 glyph, 4 attribute, 48 glue_spec
, 4 attribute_list, 1 write nodes
   avail lists: 1:1,2:34,3:4,4:1,5:23,6:2,7:34,9:38
</usr/share/texmf-dist/fonts/opentype/public/lm/lmroman10-regular.otf>
Output written on test.pdf (1 page, 5162 bytes).
Transcript written on test.log.
/workdir # ls -l test.pdf
-rw-r--r--    1 root     root          5162 Jun  3 17:13 test.pdf

```

![test.pdf](screenshotr/capture-test-luatex.png)

```bash
xvfb-run lualatex --shell-escape main.tex
```

To cleanup temp files use:
```bash
latexmk -c
```

> Note: in order for the table of contents to be generated correctly, you must run build command twice. 

### Troubleshooting 

```text
Error: Problem executing scripts APT::Update::Post-Invoke
'rm -f /var/cache/apt/archives/*.deb /var/cache/apt/archives/partial/*.deb /var/cache/apt/*.bin || true'
```

fix `Dockerfile`.


```text
Sending build context to Docker daemon  96.77kB
Step 1/4 : FROM texlive/texlive:latest-small
 ---> a74deadaad60
Step 2/4 : RUN apt-get update && apt-get -qqy upgrade || true
 ---> Using cache
 ---> dc40b1cb06e1
Step 3/4 : RUN    apt-get install -qy plantuml xvfb inkscape wget libgbm1 &&     ARCH="$(dpkg --print-architecture)" &&     DRAWIO_URL="" &&     case "$ARCH" in       amd64) DRAWIO_URL="https://github.com/jgraph/drawio-desktop/releases/download/v22.1.2/drawio-amd64-22.1.2.deb" ;;       arm64) DRAWIO_URL="https://github.com/jgraph/drawio-desktop/releases/download/v22.1.2/drawio-arm64-22.1.2.deb" ;;       *) echo "Unsupported architecture: $ARCH" && exit 1 ;;     esac &&     mkdir -p /opt/drawio-desktop &&     wget -q -O /opt/drawio-desktop/drawio.deb "$DRAWIO_URL" &&     apt-get install -y /opt/drawio-desktop/drawio.deb &&     rm -rf /opt/drawio-desktop &&     rm -rf /var/lib/apt/lists/*
 ---> Running in dc0e3da02359
Reading package lists...
Building dependency tree...
Reading state information...
libgbm1 is already the newest version (26.0.6-1).
libgbm1 set to manually installed.
Solving dependencies...
The following additional packages will be installed:
  ant ant-optional aspell aspell-en dictionaries-common dirmngr emacsen-common
  enchant fig2dev fonts-liberation fonts-liberation-sans-narrow fonts-tuffy
  gawk gnupg gnupg-l10n gnupg-utils gpg-wks-client gpgsm gpgv graphviz
  hunspell-en-us icc-profiles-free imagemagick imagemagick-7-common
  imagemagick-7.q16 java-wrappers lib2geom1.4.0 libapache-pom-java libaspell15
  libatkmm-1.6-1v5 libavalon-framework-java libbatik-java libbcpkix-java
  libbcprov-java libbcutil-java libblas3 libboost-filesystem1.90.0
  libcairomm-1.0-1v5 libcdr-0.1-1 libcdt6 libcgraph8 libcommons-io-java
  libcommons-logging-java libcommons-parent-java libdjvulibre-text
  libdjvulibre21 libdouble-conversion3 libenchant-2-2 libfftw3-double3
  libfontbox2-java libfop-java libgc1 libgfortran5 libglibmm-2.4-1t64 libgomp1
  libgpgme45 libgpgmepp7 libgsl28 libgslcblas0 libgspell-1-3
  libgspell-1-common libgtkmm-3.0-1t64 libgtksourceview-4-0
  libgtksourceview-4-common libgvc7 libgvplugin-gd8 libgvplugin-pango8
  libgvpr2 libhunspell-1.7-0 libicu78 libimage-magick-perl
  libimage-magick-q16-perl libimath-3-1-29t64 libjakarta-servlet-api-java
  libjaxp1.3-java libjlatexmath-java libjxr-tools libjxr0t64 liblapack3
  liblqr-1-0 libltdl7 libmagickcore-7.q16-10 libmagickcore-7.q16-10-extra
  libmagickwand-7.q16-10 libmpfr6 libnetpbm11t64 libopenexr-3-4-33
  libopenjph0.27 libpangomm-1.4-1v5 libpathplan4 libpoppler-glib8t64
  libpoppler147 libpotrace0 libqdox-java libraw25 librevenge-0.0-0
  librsvg2-common libsaxon-java libsigc++-2.0-0v5 libtext-iconv-perl
  libunwind8 libvisio-0.1-1 libwebpdemux2 libwmf-0.2-7 libwmf-bin
  libwmflite-0.2-7 libwpd-0.10-10 libwpg-0.3-3 libxaw7 libxdot4 libxfont2
  libxkbfile1 libxml-commons-external-java libxmlgraphics-commons-java libxmu6
  libxmuu1 libxslt1.1 netpbm python-tinycss2-common python3-bs4
  python3-cachecontrol python3-certifi python3-chardet
  python3-charset-normalizer python3-cssselect python3-html5lib python3-idna
  python3-lxml python3-msgpack python3-numpy python3-numpy-dev
  python3-packaging python3-platformdirs python3-requests python3-scour
  python3-soupsieve python3-tinycss2 python3-typing-extensions python3-urllib3
  python3-webencodings x11-xkb-utils xauth xfonts-base xserver-common
Suggested packages:
  ant-doc default-jdk | java-compiler | java-sdk antlr javacc junit junit4
  jython libactivation-java libbcel-java libbsf-java libcommons-net-java
  libmail-java libjdepend-java libjsch-java liblog4j1.2-java liboro-java
  libregexp-java libxalan2-java libxml-commons-resolver1.1-java libxz-java
  aspell-doc spellutils wordlist pinentry-gnome3 tor xfig gawk-doc
  gpg-wks-server parcimonie xloadimage gsfonts graphviz-doc hunspell
  openoffice.org-hunspell | openoffice.org-core imagemagick-7-doc autotrace
  cups-bsd | lpr | lprng enscript ffmpeg gimp grads groff-base hp2xx html2ps
  mplayer povray radiance sane-utils texlive-base-bin libraw-bin xdg-utils dia
  inkscape-tutorials libsvg-perl pstoedit python3-uniconvertor ruby
  libavalon-framework-java-doc librhino-java libexcalibur-logkit-java
  libenchant-2-voikko libfftw3-bin libfftw3-dev gsl-ref-psdoc | gsl-doc-pdf
  | gsl-doc-info | gsl-ref-html libjdom1-java libwmf-0.2-7-gtk
  libxmlgraphics-commons-java-doc python-charset-normalizer-doc python3-genshi
  python-lxml-doc gcc gfortran python-numpy-doc python3-dev python3-pytest
  python3-cryptography python3-openssl python3-socks python-requests-doc
  python-tinycss2-doc python3-backports.zstd | python3-supported-min
  python3-brotli
The following NEW packages will be installed:
  ant ant-optional aspell aspell-en dictionaries-common dirmngr emacsen-common
  enchant fig2dev fonts-liberation fonts-liberation-sans-narrow fonts-tuffy
  gawk gnupg gnupg-l10n gnupg-utils gpg-wks-client gpgsm gpgv graphviz
  hunspell-en-us icc-profiles-free imagemagick imagemagick-7-common
  imagemagick-7.q16 inkscape java-wrappers lib2geom1.4.0 libapache-pom-java
  libaspell15 libatkmm-1.6-1v5 libavalon-framework-java libbatik-java
  libbcpkix-java libbcprov-java libbcutil-java libblas3
  libboost-filesystem1.90.0 libcairomm-1.0-1v5 libcdr-0.1-1 libcdt6 libcgraph8
  libcommons-io-java libcommons-logging-java libcommons-parent-java
  libdjvulibre-text libdjvulibre21 libdouble-conversion3 libenchant-2-2
  libfftw3-double3 libfontbox2-java libfop-java libgc1 libgfortran5
  libglibmm-2.4-1t64 libgomp1 libgpgme45 libgpgmepp7 libgsl28 libgslcblas0
  libgspell-1-3 libgspell-1-common libgtkmm-3.0-1t64 libgtksourceview-4-0
  libgtksourceview-4-common libgvc7 libgvplugin-gd8 libgvplugin-pango8
  libgvpr2 libhunspell-1.7-0 libicu78 libimage-magick-perl
  libimage-magick-q16-perl libimath-3-1-29t64 libjakarta-servlet-api-java
  libjaxp1.3-java libjlatexmath-java libjxr-tools libjxr0t64 liblapack3
  liblqr-1-0 libltdl7 libmagickcore-7.q16-10 libmagickcore-7.q16-10-extra
  libmagickwand-7.q16-10 libmpfr6 libnetpbm11t64 libopenexr-3-4-33
  libopenjph0.27 libpangomm-1.4-1v5 libpathplan4 libpoppler-glib8t64
  libpoppler147 libpotrace0 libqdox-java libraw25 librevenge-0.0-0
  librsvg2-common libsaxon-java libsigc++-2.0-0v5 libtext-iconv-perl
  libunwind8 libvisio-0.1-1 libwebpdemux2 libwmf-0.2-7 libwmf-bin
  libwmflite-0.2-7 libwpd-0.10-10 libwpg-0.3-3 libxaw7 libxdot4 libxfont2
  libxkbfile1 libxml-commons-external-java libxmlgraphics-commons-java libxmu6
  libxmuu1 libxslt1.1 netpbm plantuml python-tinycss2-common python3-bs4
  python3-cachecontrol python3-certifi python3-chardet
  python3-charset-normalizer python3-cssselect python3-html5lib python3-idna
  python3-lxml python3-msgpack python3-numpy python3-numpy-dev
  python3-packaging python3-platformdirs python3-requests python3-scour
  python3-soupsieve python3-tinycss2 python3-typing-extensions python3-urllib3
  python3-webencodings wget x11-xkb-utils xauth xfonts-base xserver-common
  xvfb
0 upgraded, 148 newly installed, 0 to remove and 5 not upgraded.
Need to get 151 MB of archives.
After this operation, 429 MB of additional disk space will be used.
Get:1 http://deb.debian.org/debian testing/main amd64 libmpfr6 amd64 4.2.2-3 [729 kB]
Get:2 http://deb.debian.org/debian testing/main amd64 gawk amd64 1:5.3.2-1 [772 kB]
Get:3 http://deb.debian.org/debian testing/main amd64 imagemagick-7-common all 8:7.1.2.23+dfsg1-1 [72.2 kB]
Get:4 http://deb.debian.org/debian testing/main amd64 libgomp1 amd64 16.1.0-1 [151 kB]
Get:5 http://deb.debian.org/debian testing/main amd64 libfftw3-double3 amd64 3.3.10-2+b2 [775 kB]
Get:6 http://deb.debian.org/debian testing/main amd64 liblqr-1-0 amd64 0.4.2-2.2+b1 [30.0 kB]
Get:7 http://deb.debian.org/debian testing/main amd64 libltdl7 amd64 2.5.4-11 [416 kB]
Get:8 http://deb.debian.org/debian testing/main amd64 libraw25 amd64 0.22.1-1 [424 kB]
Get:9 http://deb.debian.org/debian testing/main amd64 libwebpdemux2 amd64 1.5.0-0.1+b2 [113 kB]
Get:10 http://deb.debian.org/debian testing/main amd64 libmagickcore-7.q16-10 amd64 8:7.1.2.23+dfsg1-1 [1867 kB]
Get:11 http://deb.debian.org/debian testing/main amd64 libimage-magick-q16-perl amd64 8:7.1.2.23+dfsg1-1 [114 kB]
Get:12 http://deb.debian.org/debian testing/main amd64 libimage-magick-perl all 8:7.1.2.23+dfsg1-1 [38.9 kB]
Get:13 http://deb.debian.org/debian testing/main amd64 libmagickwand-7.q16-10 amd64 8:7.1.2.23+dfsg1-1 [346 kB]
Get:14 http://deb.debian.org/debian testing/main amd64 python3-numpy-dev amd64 1:2.3.5+ds-3+b1 [136 kB]
Get:15 http://deb.debian.org/debian testing/main amd64 libblas3 amd64 3.12.1-7+b2 [206 kB]
Get:16 http://deb.debian.org/debian testing/main amd64 libgfortran5 amd64 16.1.0-1 [878 kB]
Get:17 http://deb.debian.org/debian testing/main amd64 liblapack3 amd64 3.12.1-7+b2 [2546 kB]
Get:18 http://deb.debian.org/debian testing/main amd64 python3-numpy amd64 1:2.3.5+ds-3+b1 [7486 kB]
Get:19 http://deb.debian.org/debian testing/main amd64 wget amd64 1.25.0-2 [984 kB]
Get:20 http://deb.debian.org/debian testing/main amd64 ant all 1.10.17-1 [2172 kB]
Get:21 http://deb.debian.org/debian testing/main amd64 ant-optional all 1.10.17-1 [456 kB]
Get:22 http://deb.debian.org/debian testing/main amd64 libaspell15 amd64 0.60.8.2-3+b1 [339 kB]
Get:23 http://deb.debian.org/debian testing/main amd64 libtext-iconv-perl amd64 1.7-9 [14.0 kB]
Get:24 http://deb.debian.org/debian testing/main amd64 emacsen-common all 3.0.8 [13.6 kB]
Get:25 http://deb.debian.org/debian testing/main amd64 dictionaries-common all 1.31.4 [172 kB]
Get:26 http://deb.debian.org/debian testing/main amd64 aspell amd64 0.60.8.2-3+b1 [277 kB]
Get:27 http://deb.debian.org/debian testing/main amd64 aspell-en all 2020.12.07-0-1.1 [306 kB]
Get:28 http://deb.debian.org/debian testing/main amd64 dirmngr amd64 2.4.9-4 [382 kB]
Get:29 http://deb.debian.org/debian testing/main amd64 hunspell-en-us all 1:2020.12.07-4 [558 kB]
Get:30 http://deb.debian.org/debian testing/main amd64 libhunspell-1.7-0 amd64 1.7.3+really1.7.3-5 [229 kB]
Get:31 http://deb.debian.org/debian testing/main amd64 libenchant-2-2 amd64 2.8.16+dfsg-3 [56.1 kB]
Get:32 http://deb.debian.org/debian testing/main amd64 enchant amd64 2.8.16+dfsg-3 [26.1 kB]
Get:33 http://deb.debian.org/debian testing/main amd64 libnetpbm11t64 amd64 2:11.13.03+ds-2 [186 kB]
Get:34 http://deb.debian.org/debian testing/main amd64 netpbm amd64 2:11.13.03+ds-2 [2092 kB]
Get:35 http://deb.debian.org/debian testing/main amd64 fig2dev amd64 1:3.2.9a-5 [449 kB]
Get:36 http://deb.debian.org/debian testing/main amd64 fonts-liberation all 1:2.1.5-3 [1475 kB]
Get:37 http://deb.debian.org/debian testing/main amd64 fonts-liberation-sans-narrow all 1:1.07.6-4 [182 kB]
Get:38 http://deb.debian.org/debian testing/main amd64 fonts-tuffy all 20120614-3 [213 kB]
Get:39 http://deb.debian.org/debian testing/main amd64 gnupg-l10n all 2.4.9-4 [749 kB]
Get:40 http://deb.debian.org/debian testing/main amd64 gpgsm amd64 2.4.9-4 [275 kB]
Get:41 http://deb.debian.org/debian testing/main amd64 gnupg all 2.4.9-4 [415 kB]
Get:42 http://deb.debian.org/debian testing/main amd64 gpg-wks-client amd64 2.4.9-4 [106 kB]
Get:43 http://deb.debian.org/debian testing/main amd64 gpgv amd64 2.4.9-4 [240 kB]
Get:44 http://deb.debian.org/debian testing/main amd64 libcdt6 amd64 14.1.2-1+b1 [18.3 kB]
Get:45 http://deb.debian.org/debian testing/main amd64 libcgraph8 amd64 14.1.2-1+b1 [58.5 kB]
Get:46 http://deb.debian.org/debian testing/main amd64 libpathplan4 amd64 14.1.2-1+b1 [29.6 kB]
Get:47 http://deb.debian.org/debian testing/main amd64 libxdot4 amd64 14.1.2-1+b1 [21.6 kB]
Get:48 http://deb.debian.org/debian testing/main amd64 libgvc7 amd64 14.1.2-1+b1 [375 kB]
Get:49 http://deb.debian.org/debian testing/main amd64 libgvpr2 amd64 14.1.2-1+b1 [125 kB]
Get:50 http://deb.debian.org/debian testing/main amd64 libgvplugin-gd8 amd64 14.1.2-1+b1 [27.0 kB]
Get:51 http://deb.debian.org/debian testing/main amd64 libgvplugin-pango8 amd64 14.1.2-1+b1 [31.4 kB]
Get:52 http://deb.debian.org/debian testing/main amd64 graphviz amd64 14.1.2-1+b1 [60.3 kB]
Get:53 http://deb.debian.org/debian testing/main amd64 icc-profiles-free all 2.4 [33.8 MB]
Get:54 http://deb.debian.org/debian testing/main amd64 imagemagick-7.q16 amd64 8:7.1.2.23+dfsg1-1 [731 kB]
Get:55 http://deb.debian.org/debian testing/main amd64 imagemagick amd64 8:7.1.2.23+dfsg1-1 [24.7 kB]
Get:56 http://deb.debian.org/debian testing/main amd64 librsvg2-common amd64 2.62.1+dfsg-1 [146 kB]
Get:57 http://deb.debian.org/debian testing/main amd64 python3-certifi all 2026.5.20+ds-1 [9712 B]
Get:58 http://deb.debian.org/debian testing/main amd64 python3-charset-normalizer amd64 3.4.7-1 [245 kB]
Get:59 http://deb.debian.org/debian testing/main amd64 python3-idna all 3.11-1 [41.7 kB]
Get:60 http://deb.debian.org/debian testing/main amd64 python3-urllib3 all 2.6.3-2 [118 kB]
Get:61 http://deb.debian.org/debian testing/main amd64 python3-chardet all 5.2.0+dfsg-2 [108 kB]
Get:62 http://deb.debian.org/debian testing/main amd64 python3-requests all 2.32.5+dfsg-1 [72.4 kB]
Get:63 http://deb.debian.org/debian testing/main amd64 python3-msgpack amd64 1.1.2-2 [107 kB]
Get:64 http://deb.debian.org/debian testing/main amd64 python3-cachecontrol all 0.14.4-1 [17.4 kB]
Get:65 http://deb.debian.org/debian testing/main amd64 python3-packaging all 26.0-1 [64.3 kB]
Get:66 http://deb.debian.org/debian testing/main amd64 python-tinycss2-common all 1.5.1-3 [44.4 kB]
Get:67 http://deb.debian.org/debian testing/main amd64 python3-webencodings all 0.5.1-5 [11.1 kB]
Get:68 http://deb.debian.org/debian testing/main amd64 python3-tinycss2 all 1.5.1-3 [25.1 kB]
Get:69 http://deb.debian.org/debian testing/main amd64 libdouble-conversion3 amd64 3.4.0-1+b1 [40.7 kB]
Get:70 http://deb.debian.org/debian testing/main amd64 libgslcblas0 amd64 2.8+dfsg-6+b1 [131 kB]
Get:71 http://deb.debian.org/debian testing/main amd64 libgsl28 amd64 2.8+dfsg-6+b1 [1002 kB]
Get:72 http://deb.debian.org/debian testing/main amd64 lib2geom1.4.0 amd64 1.4-5 [432 kB]
Get:73 http://deb.debian.org/debian testing/main amd64 libsigc++-2.0-0v5 amd64 2.12.1-4+b1 [24.6 kB]
Get:74 http://deb.debian.org/debian testing/main amd64 libglibmm-2.4-1t64 amd64 2.66.8-3 [637 kB]
Get:75 http://deb.debian.org/debian testing/main amd64 libatkmm-1.6-1v5 amd64 2.28.4-5 [82.3 kB]
Get:76 http://deb.debian.org/debian testing/main amd64 libboost-filesystem1.90.0 amd64 1.90.0-6 [276 kB]
Get:77 http://deb.debian.org/debian testing/main amd64 libcairomm-1.0-1v5 amd64 1.14.5-3 [57.2 kB]
Get:78 http://deb.debian.org/debian testing/main amd64 libicu78 amd64 78.3-2 [10.0 MB]
Get:79 http://deb.debian.org/debian testing/main amd64 librevenge-0.0-0 amd64 0.0.5-3+b3 [293 kB]
Get:80 http://deb.debian.org/debian testing/main amd64 libcdr-0.1-1 amd64 0.1.9-1 [412 kB]
Get:81 http://deb.debian.org/debian testing/main amd64 libgc1 amd64 1:8.2.12-1 [253 kB]
Get:82 http://deb.debian.org/debian testing/main amd64 libgspell-1-common all 1.14.3-1 [37.5 kB]
Get:83 http://deb.debian.org/debian testing/main amd64 libgspell-1-3 amd64 1.14.3-1 [51.1 kB]
Get:84 http://deb.debian.org/debian testing/main amd64 libpangomm-1.4-1v5 amd64 2.46.4-2 [63.1 kB]
Get:85 http://deb.debian.org/debian testing/main amd64 libgtkmm-3.0-1t64 amd64 3.24.10-3 [1016 kB]
Get:86 http://deb.debian.org/debian testing/main amd64 libgtksourceview-4-common all 4.8.4-9 [533 kB]
Get:87 http://deb.debian.org/debian testing/main amd64 libgtksourceview-4-0 amd64 4.8.4-9+b1 [219 kB]
Get:88 http://deb.debian.org/debian testing/main amd64 libgpgme45 amd64 2.1.0-2 [352 kB]
Get:89 http://deb.debian.org/debian testing/main amd64 libgpgmepp7 amd64 2.0.0-3 [160 kB]
Get:90 http://deb.debian.org/debian testing/main amd64 libpoppler147 amd64 25.03.0-11.1+b1 [2051 kB]
Get:91 http://deb.debian.org/debian testing/main amd64 libpoppler-glib8t64 amd64 25.03.0-11.1+b1 [156 kB]
Get:92 http://deb.debian.org/debian testing/main amd64 libpotrace0 amd64 1.16-2+b3 [25.5 kB]
Get:93 http://deb.debian.org/debian testing/main amd64 libvisio-0.1-1 amd64 0.1.11-1 [325 kB]
Get:94 http://deb.debian.org/debian testing/main amd64 libwpd-0.10-10 amd64 0.10.3-4 [261 kB]
Get:95 http://deb.debian.org/debian testing/main amd64 libwpg-0.3-3 amd64 0.3.4-4 [77.4 kB]
Get:96 http://deb.debian.org/debian testing/main amd64 libxslt1.1 amd64 1.1.45-0.1 [158 kB]
Get:97 http://deb.debian.org/debian testing/main amd64 inkscape amd64 1.4.3-1 [23.9 MB]
Get:98 http://deb.debian.org/debian testing/main amd64 java-wrappers all 0.5 [8848 B]
Get:99 http://deb.debian.org/debian testing/main amd64 libapache-pom-java all 33-2 [5852 B]
Get:100 http://deb.debian.org/debian testing/main amd64 libavalon-framework-java all 4.2.0+ds-1 [70.8 kB]
Get:101 http://deb.debian.org/debian testing/main amd64 libjaxp1.3-java all 1.3.05-6 [227 kB]
Get:102 http://deb.debian.org/debian testing/main amd64 libxml-commons-external-java all 1.4.01-6 [240 kB]
Get:103 http://deb.debian.org/debian testing/main amd64 libcommons-parent-java all 56-1 [10.8 kB]
Get:104 http://deb.debian.org/debian testing/main amd64 libcommons-io-java all 2.22.0-1 [570 kB]
Get:105 http://deb.debian.org/debian testing/main amd64 libcommons-logging-java all 1.3.0-2 [68.6 kB]
Get:106 http://deb.debian.org/debian testing/main amd64 libxmlgraphics-commons-java all 2.11-1 [631 kB]
Get:107 http://deb.debian.org/debian testing/main amd64 libbatik-java all 1.19-2 [3935 kB]
Get:108 http://deb.debian.org/debian testing/main amd64 libbcprov-java all 1.80-3 [5543 kB]
Get:109 http://deb.debian.org/debian testing/main amd64 libbcutil-java all 1.80-3 [615 kB]
Get:110 http://deb.debian.org/debian testing/main amd64 libbcpkix-java all 1.80-3 [985 kB]
Get:111 http://deb.debian.org/debian testing/main amd64 libdjvulibre-text all 3.5.30-1 [51.8 kB]
Get:112 http://deb.debian.org/debian testing/main amd64 libdjvulibre21 amd64 3.5.30-1 [616 kB]
Get:113 http://deb.debian.org/debian testing/main amd64 libfontbox2-java all 2.0.29-1 [1510 kB]
Get:114 http://deb.debian.org/debian testing/main amd64 libqdox-java all 1.12.1-4 [173 kB]
Get:115 http://deb.debian.org/debian testing/main amd64 libjakarta-servlet-api-java all 6.1.0-2 [378 kB]
Get:116 http://deb.debian.org/debian testing/main amd64 libfop-java all 1:2.10+dfsg-2 [4607 kB]
Get:117 http://deb.debian.org/debian testing/main amd64 libimath-3-1-29t64 amd64 3.1.12-1+b5 [45.1 kB]
Get:118 http://deb.debian.org/debian testing/main amd64 libjlatexmath-java all 1.0.7-4 [998 kB]
Get:119 http://deb.debian.org/debian testing/main amd64 libjxr0t64 amd64 1.2~git20170615.f752187-5.3+b2 [173 kB]
Get:120 http://deb.debian.org/debian testing/main amd64 libjxr-tools amd64 1.2~git20170615.f752187-5.3+b2 [16.5 kB]
Get:121 http://deb.debian.org/debian testing/main amd64 libopenjph0.27 amd64 0.27.0-1 [153 kB]
Get:122 http://deb.debian.org/debian testing/main amd64 libopenexr-3-4-33 amd64 3.4.6+ds-4+b1 [724 kB]
Get:123 http://deb.debian.org/debian testing/main amd64 libwmflite-0.2-7 amd64 0.2.14-1 [74.1 kB]
Get:124 http://deb.debian.org/debian testing/main amd64 libmagickcore-7.q16-10-extra amd64 8:7.1.2.23+dfsg1-1 [77.3 kB]
Get:125 http://deb.debian.org/debian testing/main amd64 libsaxon-java all 1:6.5.5-13 [576 kB]
Get:126 http://deb.debian.org/debian testing/main amd64 libunwind8 amd64 1.8.1-0.4 [54.0 kB]
Get:127 http://deb.debian.org/debian testing/main amd64 libwmf-0.2-7 amd64 0.2.14-1 [117 kB]
Get:128 http://deb.debian.org/debian testing/main amd64 libwmf-bin amd64 0.2.14-1 [43.4 kB]
Get:129 http://deb.debian.org/debian testing/main amd64 libxmu6 amd64 2:1.1.3-4+b1 [59.6 kB]
Get:130 http://deb.debian.org/debian testing/main amd64 libxaw7 amd64 2:1.0.16-1+b2 [214 kB]
Get:131 http://deb.debian.org/debian testing/main amd64 libxfont2 amd64 1:2.0.6-2+b1 [133 kB]
Get:132 http://deb.debian.org/debian testing/main amd64 libxkbfile1 amd64 1:1.1.0-1+b5 [76.7 kB]
Get:133 http://deb.debian.org/debian testing/main amd64 libxmuu1 amd64 2:1.1.3-4+b1 [22.0 kB]
Get:134 http://deb.debian.org/debian testing/main amd64 plantuml all 1:1.2020.2+ds-6 [8149 kB]
Get:135 http://deb.debian.org/debian testing/main amd64 python3-soupsieve all 2.8.4-1 [38.7 kB]
Get:136 http://deb.debian.org/debian testing/main amd64 python3-typing-extensions all 4.15.0-2 [92.5 kB]
Get:137 http://deb.debian.org/debian testing/main amd64 python3-bs4 all 4.14.3-2 [118 kB]
Get:138 http://deb.debian.org/debian testing/main amd64 python3-cssselect all 1.4.0-1 [22.1 kB]
Get:139 http://deb.debian.org/debian testing/main amd64 python3-html5lib all 1.2-3 [92.1 kB]
Get:140 http://deb.debian.org/debian testing/main amd64 python3-lxml amd64 6.1.0-1 [2232 kB]
Get:141 http://deb.debian.org/debian testing/main amd64 python3-platformdirs all 4.9.6-1 [17.3 kB]
Get:142 http://deb.debian.org/debian testing/main amd64 python3-scour all 0.38.2-6 [56.0 kB]
Get:143 http://deb.debian.org/debian testing/main amd64 x11-xkb-utils amd64 7.7+9 [159 kB]
Get:144 http://deb.debian.org/debian testing/main amd64 xauth amd64 1:1.1.2-1.1 [35.9 kB]
Get:145 http://deb.debian.org/debian testing/main amd64 xfonts-base all 1:1.0.5+nmu1 [5895 kB]
Get:146 http://deb.debian.org/debian testing/main amd64 xserver-common all 2:21.1.22-1 [28.0 kB]
Get:147 http://deb.debian.org/debian testing/main amd64 xvfb amd64 2:21.1.22-1 [836 kB]
Get:148 http://deb.debian.org/debian testing/main amd64 gnupg-utils amd64 2.4.9-4 [192 kB]
Preconfiguring packages ...
Fetched 151 MB in 1min 34s (1608 kB/s)
dpkg-deb (subprocess): decompressing archive '/var/cache/apt/archives/libmpfr6_4.2.2-3_amd64.deb' (size=729036)
member 'control.tar': lzma error: Cannot allocate memory
tar: This does not look like a tar archive
tar: Exiting with failure status due to previous errors
dpkg-deb: error: tar subprocess failed with exit status 2
dpkg: error processing archive /var/cache/apt/archives/libmpfr6_4.2.2-3_amd64.deb (--unpack):
 dpkg-deb --control subprocess failed with exit status 2
Errors were encountered while processing:
 /var/cache/apt/archives/libmpfr6_4.2.2-3_amd64.deb
E: Sub-process /usr/bin/dpkg returned an error code (1)
E: Problem executing scripts DPkg::Post-Invoke 'rm -f /var/cache/apt/archives/*.deb /var/cache/apt/archives/partial/*.deb /var/cache/apt/*.bin || true'
E: Sub-process returned an error code
The command '/bin/sh -c apt-get install -qy plantuml xvfb inkscape wget libgbm1 &&     ARCH="$(dpkg --print-architecture)" &&     DRAWIO_URL="" &&     case "$ARCH" in       amd64) DRAWIO_URL="https://github.com/jgraph/drawio-desktop/releases/download/v22.1.2/drawio-amd64-22.1.2.deb" ;;       arm64) DRAWIO_URL="https://github.com/jgraph/drawio-desktop/releases/download/v22.1.2/drawio-arm64-22.1.2.deb" ;;       *) echo "Unsupported architecture: $ARCH" && exit 1 ;;     esac &&     mkdir -p /opt/drawio-desktop &&     wget -q -O /opt/drawio-desktop/drawio.deb "$DRAWIO_URL" &&     apt-get install -y /opt/drawio-desktop/drawio.deb &&     rm -rf /opt/drawio-desktop &&     rm -rf /var/lib/apt/lists/*' returned a non-zero code: 100


```
refactor Dockerfile. The new run log:
```text
Sending build context to Docker daemon  92.16kB
Step 1/24 : FROM texlive/texlive:latest-small
 ---> a74deadaad60
Step 2/24 : RUN grep -R Post-Invoke /etc/apt/apt.conf.d/ || true
 ---> Running in ee7b143fce21
/etc/apt/apt.conf.d/docker-clean:DPkg::Post-Invoke { "rm -f /var/cache/apt/archives/*.deb /var/cache/apt/archives/partial/*.deb /var/cache/apt/*.bin || true"; };
/etc/apt/apt.conf.d/docker-clean:APT::Update::Post-Invoke { "rm -f /var/cache/apt/archives/*.deb /var/cache/apt/archives/partial/*.deb /var/cache/apt/*.bin || true"; };
Removing intermediate container ee7b143fce21
 ---> 226ca388616f
Step 3/24 : RUN rm -f /etc/apt/apt.conf.d/docker-clean || true
 ---> Running in 06dae8bb3c6d
Removing intermediate container 06dae8bb3c6d
 ---> 0102b8c1f1dd
Step 4/24 : RUN apt-get update
 ---> Running in d0bcb6f57560
Get:1 http://deb.debian.org/debian testing InRelease [151 kB]
Get:2 http://deb.debian.org/debian testing-updates InRelease [45.1 kB]
Get:3 http://deb.debian.org/debian-security testing-security InRelease [48.0 kB]
Get:4 http://deb.debian.org/debian testing/main amd64 Packages [9698 kB]
Fetched 9941 kB in 10s (1042 kB/s)
Reading package lists...
Removing intermediate container d0bcb6f57560
 ---> ac55ed8620d0
Step 5/24 : RUN apt-cache policy libmpfr6 || true
 ---> Running in 6a8de896c5f7
libmpfr6:
  Installed: (none)
  Candidate: 4.2.2-3
  Version table:
     4.2.2-3 500
        500 http://deb.debian.org/debian testing/main amd64 Packages
Removing intermediate container 6a8de896c5f7
 ---> f278df1ae6a7
Step 6/24 : RUN cat /etc/os-release
 ---> Running in 9e1135a54c66
PRETTY_NAME="Debian GNU/Linux forky/sid"
NAME="Debian GNU/Linux"
VERSION_CODENAME=forky
ID=debian
HOME_URL="https://www.debian.org/"
SUPPORT_URL="https://www.debian.org/support"
BUG_REPORT_URL="https://bugs.debian.org/"
Removing intermediate container 9e1135a54c66
 ---> af8bae1112d6
Step 7/24 : RUN uname -a
 ---> Running in 55251a1b2fcd
Linux 55251a1b2fcd 4.19.130-boot2docker #1 SMP Mon Jun 29 23:52:55 UTC 2020 x86_64 GNU/Linux
Removing intermediate container 55251a1b2fcd
 ---> d3d4ecbe8ea1
Step 8/24 : RUN free -m
 ---> Running in b826ba85c58f
               total        used        free      shared  buff/cache   available
Mem:            3944         403        3416         274         446        3541
Swap:           1904           0        1904
Removing intermediate container b826ba85c58f
 ---> 9908a2cdb3be
Step 9/24 : RUN df -h
 ---> Running in 61637294b9d5
Filesystem      Size  Used Avail Use% Mounted on
overlay          18G  2.4G   15G  14% /
tmpfs            64M     0   64M   0% /dev
tmpfs           2.0G     0  2.0G   0% /sys/fs/cgroup
shm              64M     0   64M   0% /dev/shm
/dev/sda1        18G  2.4G   15G  14% /etc/hosts
tmpfs           2.0G     0  2.0G   0% /proc/asound
tmpfs           2.0G     0  2.0G   0% /proc/acpi
tmpfs           2.0G     0  2.0G   0% /proc/scsi
tmpfs           2.0G     0  2.0G   0% /sys/firmware
Removing intermediate container 61637294b9d5
 ---> ca133b01885e
Step 10/24 : RUN df -i
 ---> Running in 68a6ed6b02e2
Filesystem      Inodes IUsed   IFree IUse% Mounted on
overlay        2434064 71022 2363042    3% /
tmpfs           504954    16  504938    1% /dev
tmpfs           504954    13  504941    1% /sys/fs/cgroup
shm             504954     1  504953    1% /dev/shm
/dev/sda1      2434064 71022 2363042    3% /etc/hosts
tmpfs           504954     1  504953    1% /proc/asound
tmpfs           504954     1  504953    1% /proc/acpi
tmpfs           504954     1  504953    1% /proc/scsi
tmpfs           504954     1  504953    1% /sys/firmware
Removing intermediate container 68a6ed6b02e2
 ---> f2172f159a45
Step 11/24 : RUN apt-get install -d -y libmpfr6
 ---> Running in 794cec9da966
Reading package lists...
Building dependency tree...
Reading state information...
Solving dependencies...
The following NEW packages will be installed:
  libmpfr6
0 upgraded, 1 newly installed, 0 to remove and 5 not upgraded.
Need to get 729 kB of archives.
After this operation, 1250 kB of additional disk space will be used.
Get:1 http://deb.debian.org/debian testing/main amd64 libmpfr6 amd64 4.2.2-3 [729 kB]
Fetched 729 kB in 0s (1570 kB/s)
Download complete and in download only mode
Removing intermediate container 794cec9da966
 ---> a1a86183db4f
Step 12/24 : RUN ls -lh /var/cache/apt/archives/
 ---> Running in 8530e524b223
total 716K
-rw-r--r-- 1 root root 712K Mar 27 08:45 libmpfr6_4.2.2-3_amd64.deb
-rw-r----- 1 root root    0 Jun  2 16:20 lock
drwx------ 2 _apt root 4.0K Jun  2 16:20 partial
Removing intermediate container 8530e524b223
 ---> ef0ab6154c89
Step 13/24 : RUN dpkg-deb -I /var/cache/apt/archives/libmpfr6*.deb
 ---> Running in 409c1a499017
 new Debian package, version 2.0.
 size 729036 bytes: control archive=3744 bytes.
dpkg-deb (subprocess): decompressing archive '/var/cache/apt/archives/libmpfr6_4.2.2-3_amd64.deb' (size=729036) member 'control.tar': lzma error: Cannot allocate memory
tar: This does not look like a tar archive
tar: Exiting with failure status due to previous errors
dpkg-deb: error: tar subprocess failed with exit status 2
The command '/bin/sh -c dpkg-deb -I /var/cache/apt/archives/libmpfr6*.deb' returned a non-zero code: 2
```
```sh
docker build -t docker-latex -f Dockerfile .
```
```text
Sending build context to Docker daemon  79.36kB
Step 1/24 : FROM texlive/texlive:latest-small
 ---> a74deadaad60
Step 2/24 : RUN grep -R Post-Invoke /etc/apt/apt.conf.d/ || true
 ---> Using cache
 ---> 226ca388616f
Step 3/24 : RUN rm -f /etc/apt/apt.conf.d/docker-clean || true
 ---> Using cache
 ---> 0102b8c1f1dd
Step 4/24 : RUN apt-get update
 ---> Using cache
 ---> ac55ed8620d0
Step 5/24 : RUN apt-cache policy libmpfr6 || true
 ---> Using cache
 ---> f278df1ae6a7
Step 6/24 : RUN cat /etc/os-release
 ---> Using cache
 ---> af8bae1112d6
Step 7/24 : RUN uname -a
 ---> Using cache
 ---> d3d4ecbe8ea1
Step 8/24 : RUN free -m
 ---> Using cache
 ---> 9908a2cdb3be
Step 9/24 : RUN df -h
 ---> Using cache
 ---> ca133b01885e
Step 10/24 : RUN df -i
 ---> Using cache
 ---> f2172f159a45
Step 11/24 : RUN apt-get install -d -y libmpfr6
 ---> Using cache
 ---> a1a86183db4f
Step 12/24 : RUN ls -lh /var/cache/apt/archives/
 ---> Using cache
 ---> ef0ab6154c89
Step 13/24 : RUN dpkg-deb -I /var/cache/apt/archives/libmpfr6*.deb || true
 ---> Running in 642154132be9
 new Debian package, version 2.0.
 size 729036 bytes: control archive=3744 bytes.
dpkg-deb (subprocess): decompressing archive '/var/cache/apt/archives/libmpfr6_4.2.2-3_amd64.deb' (size=729036) member 'control.tar': lzma error: Cannot allocate memory
tar: This does not look like a tar archive
tar: Exiting with failure status due to previous errors
dpkg-deb: error: tar subprocess failed with exit status 2
Removing intermediate container 642154132be9
 ---> 02e2d70536e6
Step 14/24 : RUN apt-get install -qqy wget
 ---> Running in 75c497ce3ca4
dpkg-deb (subprocess): decompressing archive '/var/cache/apt/archives/wget_1.25.0-2_amd64.deb' (size=984500) member 'control.tar': lzma error: Cannot allocate memory
tar: This does not look like a tar archive
tar: Exiting with failure status due to previous errors
dpkg-deb: error: tar subprocess failed with exit status 2
dpkg: error processing archive /var/cache/apt/archives/wget_1.25.0-2_amd64.deb (--unpack):
 dpkg-deb --control subprocess failed with exit status 2
Errors were encountered while processing:
 /var/cache/apt/archives/wget_1.25.0-2_amd64.deb
E: Sub-process /usr/bin/dpkg returned an error code (1)
The command '/bin/sh -c apt-get install -qqy wget' returned a non-zero code: 100

```

upgrade Docker Toolbox `default` System Base Memory to `2048`__MB__ 
```powershell
```

examine the failed image:


```sh
docker images
```

```text
docker images
REPOSITORY          TAG                 IMAGE ID            CREATED             SIZE
<none>              <none>              02e2d70536e6        36 seconds ago      1.62GB
texlive/texlive     latest-small        a74deadaad60        2 days ago          1.5GB
```
```sh
ID=$(docker images --format '{{.ID}}' | head -1)
echo $ID
```
```text
02e2d70536e6
```
```sh
docker run -it --rm $ID bash
```
alternatively
```sh        
docker images -f dangling=true -q | head -1
```

```sh
ls -l /var/cache/apt/archives/wget_1.25.0-2_amd64.deb
```
```text
ls: cannot access '/var/cache/apt/archives/wget_1.25.0-2_amd64.deb': No such file or directory
```
```sh
ls -la /var/cache/apt/archives
```
```text
total 724
drwxr-xr-x 3 root root   4096 Jun  2 16:20 .
drwxr-xr-x 1 root root   4096 Jun  2 16:20 ..
-rw-r--r-- 1 root root 729036 Mar 27 08:45 libmpfr6_4.2.2-3_amd64.deb
-rw-r----- 1 root root      0 Jun  2 16:20 lock
drwx------ 2 _apt root   4096 Jun  2 16:20 partial
```
```sh

 docker cp 82068:/var/cache/apt/archives/libmpfr6_4.2.2-3_amd64.deb .
```
```sh
file libmpfr6_4.2.2-3_amd64.deb
```
```text
libmpfr6_4.2.2-3_amd64.deb: Debian binary package (format 2.0), with control.tar.xz , data compression xz
```
```sh
 apt-get install -qqy binutils
```
```text
dpkg-deb (subprocess): decompressing archive '/tmp/apt-dpkg-install-BP3fOn/0-libsframe3_2.46-3_amd64.deb' (size=84668) member 'control.tar': lzma error: Cannot allocate memory
tar: This does not look like a tar archive
tar: Exiting with failure status due to previous errors
dpkg-deb: error: tar subprocess failed with exit status 2
dpkg: error processing archive /tmp/apt-dpkg-install-BP3fOn/0-libsframe3_2.46-3_amd64.deb (--unpack):
 dpkg-deb --control subprocess failed with exit status 2
dpkg-deb (subprocess): decompressing archive '/tmp/apt-dpkg-install-BP3fOn/1-binutils-common_2.46-3_amd64.deb' (size=2631520) member 'control.tar': lzma error: Cannot allocate memory
tar: This does not look like a tar archive
tar: Exiting with failure status due to previous errors
dpkg-deb: error: tar subprocess failed with exit status 2
dpkg: error processing archive /tmp/apt-dpkg-install-BP3fOn/1-binutils-common_2.46-3_amd64.deb (--unpack):
 dpkg-deb --control subprocess failed with exit status 2
dpkg-deb (subprocess): decompressing archive '/tmp/apt-dpkg-install-BP3fOn/2-libbinutils_2.46-3_amd64.deb' (size=548012) member 'control.tar': lzma error: Cannot allocate memory
tar: This does not look like a tar archive
tar: Exiting with failure status due to previous errors
dpkg-deb: error: tar subprocess failed with exit status 2
dpkg: error processing archive /tmp/apt-dpkg-install-BP3fOn/2-libbinutils_2.46-3_amd64.deb (--unpack):
 dpkg-deb --control subprocess failed with exit status 2
dpkg-deb (subprocess): decompressing archive '/tmp/apt-dpkg-install-BP3fOn/3-libgprofng0_2.46-3_amd64.deb' (size=820348) member 'control.tar': lzma error: Cannot allocate memory
tar: This does not look like a tar archive
tar: Exiting with failure status due to previous errors
dpkg-deb: error: tar subprocess failed with exit status 2
dpkg: error processing archive /tmp/apt-dpkg-install-BP3fOn/3-libgprofng0_2.46-3_amd64.deb (--unpack):
 dpkg-deb --control subprocess failed with exit status 2
dpkg-deb (subprocess): decompressing archive '/tmp/apt-dpkg-install-BP3fOn/4-libctf-nobfd0_2.46-3_amd64.deb' (size=160012) member 'control.tar': lzma error: Cannot allocate memory
tar: This does not look like a tar archive
tar: Exiting with failure status due to previous errors
dpkg-deb: error: tar subprocess failed with exit status 2
dpkg: error processing archive /tmp/apt-dpkg-install-BP3fOn/4-libctf-nobfd0_2.46-3_amd64.deb (--unpack):
 dpkg-deb --control subprocess failed with exit status 2
dpkg-deb (subprocess): decompressing archive '/tmp/apt-dpkg-install-BP3fOn/5-libctf0_2.46-3_amd64.deb' (size=92152) member 'control.tar': lzma error: Cannot allocate memory
tar: This does not look like a tar archive
tar: Exiting with failure status due to previous errors
dpkg-deb: error: tar subprocess failed with exit status 2
dpkg: error processing archive /tmp/apt-dpkg-install-BP3fOn/5-libctf0_2.46-3_amd64.deb (--unpack):
 dpkg-deb --control subprocess failed with exit status 2
dpkg-deb (subprocess): decompressing archive '/tmp/apt-dpkg-install-BP3fOn/6-libjansson4_2.14-2+b4_amd64.deb' (size=40116) member 'control.tar': lzma error: Cannot allocate memory
tar: This does not look like a tar archive
tar: Exiting with failure status due to previous errors
dpkg-deb: error: tar subprocess failed with exit status 2
dpkg: error processing archive /tmp/apt-dpkg-install-BP3fOn/6-libjansson4_2.14-2+b4_amd64.deb (--unpack):
 dpkg-deb --control subprocess failed with exit status 2
dpkg-deb (subprocess): decompressing archive '/tmp/apt-dpkg-install-BP3fOn/7-binutils-x86-64-linux-gnu_2.46-3_amd64.deb' (size=1057848) member 'control.tar': lzma error: Cannot allocate memory
tar: This does not look like a tar archive
tar: Exiting with failure status due to previous errors
dpkg-deb: error: tar subprocess failed with exit status 2
dpkg: error processing archive /tmp/apt-dpkg-install-BP3fOn/7-binutils-x86-64-linux-gnu_2.46-3_amd64.deb (--unpack):
 dpkg-deb --control subprocess failed with exit status 2
dpkg-deb (subprocess): decompressing archive '/tmp/apt-dpkg-install-BP3fOn/8-binutils_2.46-3_amd64.deb' (size=282104) member 'control.tar': lzma error: Cannot allocate memory
tar: This does not look like a tar archive
tar: Exiting with failure status due to previous errors
dpkg-deb: error: tar subprocess failed with exit status 2
dpkg: error processing archive /tmp/apt-dpkg-install-BP3fOn/8-binutils_2.46-3_amd64.deb (--unpack):
 dpkg-deb --control subprocess failed with exit status 2
Errors were encountered while processing:
 /tmp/apt-dpkg-install-BP3fOn/0-libsframe3_2.46-3_amd64.deb
 /tmp/apt-dpkg-install-BP3fOn/1-binutils-common_2.46-3_amd64.deb
 /tmp/apt-dpkg-install-BP3fOn/2-libbinutils_2.46-3_amd64.deb
 /tmp/apt-dpkg-install-BP3fOn/3-libgprofng0_2.46-3_amd64.deb
 /tmp/apt-dpkg-install-BP3fOn/4-libctf-nobfd0_2.46-3_amd64.deb
 /tmp/apt-dpkg-install-BP3fOn/5-libctf0_2.46-3_amd64.deb
 /tmp/apt-dpkg-install-BP3fOn/6-libjansson4_2.14-2+b4_amd64.deb
 /tmp/apt-dpkg-install-BP3fOn/7-binutils-x86-64-linux-gnu_2.46-3_amd64.deb
 /tmp/apt-dpkg-install-BP3fOn/8-binutils_2.46-3_amd64.deb
E: Sub-process /usr/bin/dpkg returned an error code (1)
```

```sh
free -m
```
```text
               total        used        free      shared  buff/cache   available
Mem:            3944         401        3306         274         552        3543
Swap:           1904           0        1904
ulimit -a
```
```text
real-time non-blocking time  (microseconds, -R) unlimited
core file size              (blocks, -c) unlimited
data seg size               (kbytes, -d) unlimited
scheduling priority                 (-e) 0
file size                   (blocks, -f) unlimited
pending signals                     (-i) 15563
max locked memory           (kbytes, -l) 64
max memory size             (kbytes, -m) unlimited
open files                          (-n) 1048576
pipe size                (512 bytes, -p) 8
POSIX message queues         (bytes, -q) 819200
real-time priority                  (-r) 0
stack size                  (kbytes, -s) 8192
cpu time                   (seconds, -t) unlimited
max user processes                  (-u) unlimited
virtual memory              (kbytes, -v) unlimited
file locks                          (-x) unlimited
```
```sh
cat /proc/meminfo| head -20
```
```text
MemTotal:        4039632 kB
MemFree:         3386276 kB
MemAvailable:    3628180 kB
Buffers:           20472 kB
Cached:           523160 kB
SwapCached:            0 kB
Active:           270176 kB
Inactive:         312788 kB
Active(anon):     124332 kB
Inactive(anon):   182476 kB
Active(file):     145844 kB
Inactive(file):   130312 kB
Unevictable:           0 kB
Mlocked:               0 kB
SwapTotal:       1949920 kB
SwapFree:        1949920 kB
Dirty:                32 kB
Writeback:             0 kB
AnonPages:         39344 kB
Mapped:            78324 kB
```

```sh
cp /var/cache/apt/archives/libmpfr6_4.2.2-3_amd64.deb .
perl -e '
open(F,"<libmpfr6_4.2.2-3_amd64.deb") or die;
read(F,$buf,8);
print $buf,"\n";
'
```
```text
!<arch>
```

```sh
dpkg-deb --info libmpfr6_4.2.2-3_amd64.deb
```
```
new Debian package, version 2.0.
size 729036 bytes: control archive=3744 bytes.
dpkg-deb (subprocess): decompressing archive 'libmpfr6_4.2.2-3_amd64.deb' (size=729036) member 'control.tar': lzma error: Cannot allocate memory
tar: This does not look like a tar archive
tar: Exiting with failure status due to previous errors
dpkg-deb: error: tar subprocess failed with exit status 2
```

The suspect is docker host kernel:
```
uname -a
cat /proc/version
Linux 82068db3f2f5 4.19.130-boot2docker #1 SMP Mon Jun 29 23:52:55 UTC 2020 x86_64 GNU/Linux
Linux version 4.19.130-boot2docker (root@3e8c56dabc1e) (gcc version 8.3.0 (Debian 8.3.0-6)) #1 SMP Mon Jun 29 23:52:55 UTC 2020
```

```
grep Seccomp: /proc/self/status
Seccomp:        2
root@82068db3f2f5:/workdir# grep -E 'Cap(Eff|Prm|Bnd)' /proc/self/status
CapPrm: 00000000a80425fb
CapEff: 00000000a80425fb
CapBnd: 00000000a80425fb
```

```sh
docker run --rm -it --security-opt seccomp=unconfined texlive/texlive:latest-small bash
```
in that container
```sh
apt-get update
apt-get install -y -qq binutls
```
```sh
which tar
```
```text
/usr/bin/tar
```
```sh
which ar
```
```text
/usr/bin/ar
```

```sh
apt-get install -d -y libmpfr6
```
```text
Reading package lists... Done
Building dependency tree... Done
Reading state information... Done
Solving dependencies... Done
The following NEW packages will be installed:
  libmpfr6
0 upgraded, 1 newly installed, 0 to remove and 5 not upgraded.
Need to get 729 kB of archives.
After this operation, 1250 kB of additional disk space will be used.
Get:1 http://deb.debian.org/debian testing/main amd64 libmpfr6 amd64 4.2.2-3 [729 kB]
Fetched 729 kB in 1s (1085 kB/s)
Download complete and in download only mode


```
```
dpkg-deb --info /var/cache/apt/archives/libmpfr6_4.2.2-3_amd64.deb
```
```text
new Debian package, version 2.0.
 size 729036 bytes: control archive=3744 bytes.
     843 bytes,    18 lines      control
     611 bytes,     9 lines      md5sums
      30 bytes,     1 lines      shlibs
   19177 bytes,   696 lines      symbols
      66 bytes,     2 lines      triggers
 Package: libmpfr6
 Source: mpfr4
 Version: 4.2.2-3
 Architecture: amd64
 Maintainer: Debian GCC Maintainers <debian-gcc@lists.debian.org>
 Installed-Size: 1221
 Depends: libc6 (>= 2.38), libgmp10 (>= 2:6.3.0+dfsg)
 Breaks: libcgal13 (<< 4.11-2+b1), libflint-2.5.2 (<< 2.5.2-17+b2), libgiac0 (<< 1.2.3.57+dfsg1-2+b4), libgmp3 (<< 4.1.4-3), libmpc3 (<< 1.1.0-1~), libnormaliz3 (<< 3.5.1+ds-4), sagemath (<< 8.1-2+b2)
 Section: libs
 Priority: optional
 Multi-Arch: same
 Homepage: https://www.mpfr.org/
 Description: multiple precision floating-point computation
  MPFR provides a library for multiple-precision floating-point computation
  with correct rounding.  The computation is both efficient and has a
  well-defined semantics. It copies the good ideas from the
  ANSI/IEEE-754 standard for double-precision floating-point arithmetic
  (53-bit mantissa).

```
this proves that 2026 Debian Forky/Sid userspace + Docker's default seccomp profile + 2020 Boot2Docker kernel (4.19.130) being an incompatible combination


```sh
docker build --security-opt seccomp=unconfined  -t docker-latex -f Dockerfile .
```
```text
Sending build context to Docker daemon  828.9kB
Error response from daemon: The daemon on this platform does not support setting security options on build
```

the only remaining option is:

```sh
docker run -it \
  --name docker-latex-build \
  --security-opt seccomp=unconfined \
  texlive/texlive:latest-small bash
```
in the container installl everything manually, repeating the original steps
```sh
apt-get update
apt-get install -qqy plantuml xvfb inkscape wget libgbm1
```
```sg
ARCH=$(dpkg --print-architecture)
echo "$ARCH"
```
```sh
case "$ARCH" in
  amd64)
    DRAWIO_URL="https://github.com/jgraph/drawio-desktop/releases/download/v22.1.2/drawio-amd64-22.1.2.deb"
    ;;
  arm64)
    DRAWIO_URL="https://github.com/jgraph/drawio-desktop/releases/download/v22.1.2/drawio-arm64-22.1.2.deb"
    ;;
  *)
    echo "Unsupported architecture: $ARCH"
    exit 1
    ;;
esac
```

```sh
mkdir -p /opt/drawio-desktop
wget -qO /opt/drawio-desktop/drawio.deb "$DRAWIO_URL"
apt-get install -y /opt/drawio-desktop/drawio.deb
```
```sh
docker commit docker-latex-build docker-latex:working
```


```sh
docker images docker-latex:working
```
```text
REPOSITORY          TAG                 IMAGE ID            CREATED
SIZE
docker-latex        working             acf67e938fe8        27 seconds ago      2.5GB
```

```
docker stop docker-latex-build
docker container rm docker-latex-build
```

```sh
docker run -it  docker-latex:working sh
```
```sh
inkscape --version
```
```
(process:18): GLib-ERROR **: 17:40:49.879: file ../../../glib/gthread-posix.c: line 774 (g_system_thread_new): error 'Operation not permitted' during 'pthread_create'
Aborted (core dumped)
# drawio --version
[0602/174058.075309:FATAL:electron_main_delegate.cc(294)] Running as root without --no-sandbox is not supported. See https://crbug.com/638180.
Trace/breakpoint trap (core dumped)
# java -version
[0.030s][warning][os,thread] Failed to start thread "VM Periodic Task Thread" - pthread_create failed (EPERM) for attributes: stacksize: 1024k, guardsize: 4k, detached.
[0.037s][warning][os,thread] Failed to start thread "VM Thread" - pthread_create failed (EPERM) for attributes: stacksize: 1024k, guardsize: 4k, detached.
Error occurred during initialization of VM
Cannot create VM thread. Out of system resources.
```

```
docker run -it --rm --security-opt seccomp=unconfined docker-latex:working sh
```
```sh
xvfb-run -a drawio --no-sandbox --version
```
```text
Checking for beta autoupdate feature for deb/rpm distributions
Found package-type: deb
[100:0602/174604.949528:ERROR:bus.cc(399)] Failed to connect to the bus: Failed to connect to socket /run/dbus/system_bus_socket: No such file or directory
[100:0602/174604.952691:ERROR:bus.cc(399)] Failed to connect to the bus: Failed to connect to socket /run/dbus/system_bus_socket: No such file or directory
[100:0602/174604.957553:ERROR:bus.cc(399)] Failed to connect to the bus: Failed to connect to socket /run/dbus/system_bus_socket: No such file or directory
[100:0602/174604.959019:ERROR:bus.cc(399)] Failed to connect to the bus: Could not parse server address: Unknown address type (examples of valid types are "tcp" and on UNIX "unix")
[100:0602/174604.960497:ERROR:bus.cc(399)] Failed to connect to the bus: Could not parse server address: Unknown address type (examples of valid types are "tcp" and on UNIX "unix")
[100:0602/174604.961759:ERROR:bus.cc(399)] Failed to connect to the bus: Could not parse server address: Unknown address type (examples of valid types are "tcp" and on UNIX "unix")
[100:0602/174604.963264:ERROR:bus.cc(399)] Failed to connect to the bus: Could not parse server address: Unknown address type (examples of valid types are "tcp" and on UNIX "unix")
22.1.2
```
```sh
java -version
```
```text
openjdk version "25.0.3" 2026-04-21
OpenJDK Runtime Environment (build 25.0.3+9-2-Debian)
OpenJDK 64-Bit Server VM (build 25.0.3+9-2-Debian, mixed mode, sharing)
```
```sh
inkscape --version
```
```text
Inkscape 1.4.3 (0d15f75042, 2025-12-25)
```
```sh
Xvfb -help
```
```text
use: X [:<display>] [option]
...
```
```sh
plantuml -version
```
```text
PlantUML version 1.2020.02 (Sun Mar 01 10:22:07 UTC 2020)
(GPL source distribution)
Java Runtime: OpenJDK Runtime Environment
JVM: OpenJDK 64-Bit Server VM
Java Version: 25.0.3+9-2-Debian
Operating System: Linux
Default Encoding: UTF-8
Language: en
Country: null
Machine: 9b265246f2b3
PLANTUML_LIMIT_SIZE: 4096
Processors: 1
Max Memory: 1,001,521,152
Total Memory: 62,849,024
Free Memory: 54,622,776
Used Memory: 8,226,248
Thread Active Count: 1

The environment variable GRAPHVIZ_DOT has not been set
Dot executable is /usr/bin/dot
Dot version: dot - graphviz version 14.1.2 (0)
Warning : cannot determine dot version

```

```sh
xvfb-run -a drawio \
  --no-sandbox \
  --export \
  --format png \
  --output diagram.png \
  diagram.drawio
```

```sh
xvfb-run -a drawio \
  --no-sandbox \
  --export \
  --format pdf \
  --output diagram.pdf \
  diagram.drawio
```
### Font Usage

To download the Source Sans Pro TTF font directly, pull the file from the official Adobe Fonts Source Sans GitHub Repository:

```sh
pushd fonts/SourceSansPro
curl -skLo SourceSansPro-Black.ttf https://github.com/adobe-fonts/source-sans/raw/refs/heads/release/TTF/SourceSans3-Black.ttf
curl -skLo SourceSansPro-BlackItalic.ttf https://github.com/adobe-fonts/source-sans/raw/refs/heads/release/TTF/SourceSans3-BlackIt.ttf
curl -skLo SourceSansPro-Bold.ttf https://github.com/adobe-fonts/source-sans/raw/refs/heads/release/TTF/SourceSans3-Bold.ttf
curl -skLo SourceSansPro-BoldItalic.ttf https://github.com/adobe-fonts/source-sans/raw/refs/heads/release/TTF/SourceSans3-BoldIt.ttf
curl -skLo SourceSansPro-ExtraLight.ttf https://github.com/adobe-fonts/source-sans/raw/refs/heads/release/TTF/SourceSans3-ExtraLight.ttf
curl -skLo SourceSansPro-ExtraLightItalic.ttf https://github.com/adobe-fonts/source-sans/raw/refs/heads/release/TTF/SourceSans3-ExtraLightIt.ttf
curl -skLo SourceSansPro-Italic.ttf https://github.com/adobe-fonts/source-sans/raw/refs/heads/release/TTF/SourceSans3-It.ttf
curl -skLo SourceSansPro-Light.ttf https://github.com/adobe-fonts/source-sans/raw/refs/heads/release/TTF/SourceSans3-Light.ttf
curl -skLo SourceSansPro-LightItalic.ttf https://github.com/adobe-fonts/source-sans/raw/refs/heads/release/TTF/SourceSans3-LightIt.ttf
curl -skLo SourceSansPro-Regular.ttf  https://github.com/adobe-fonts/source-sans/raw/refs/heads/release/TTF/SourceSans3-Regular.ttf
curl -skLo SourceSansPro-SemiBold.ttf https://github.com/adobe-fonts/source-sans/raw/refs/heads/release/TTF/SourceSans3-Semibold.ttf
curl -skLo SourceSansPro-SemiBoldItalic.ttf https://github.com/adobe-fonts/source-sans/raw/refs/heads/release/TTF/SourceSans3-SemiboldIt.ttf
popd
```

```sh
pushd fonts/SourceCodePro
BASE_URL=https://github.com/adobe-fonts/source-code-pro/tree/release/TTF
curl -skLo SourceCodePro-Black.ttf $BASE_URL/SourceCodePro-Black.ttf
curl -skLo SourceCodePro-BlackItalic.ttf $BASE_URL/SourceCodePro-BlackIt.ttf
curl -skLo SourceCodePro-Bold.ttf $BASE_URL/SourceCodePro-Bold.ttf
curl -skLo SourceCodePro-BoldItalic.ttf $BASE_URL/SourceCodePro-BoldIt.ttf
curl -skLo SourceCodePro-ExtraLight.ttf $BASE_URL/SourceCodePro-ExtraLight.ttf
curl -skLo SourceCodePro-ExtraLightItalic.ttf $BASE_URL/SourceCodePro-ExtraLightIt.ttf
curl -skLo SourceCodePro-Italic.ttf $BASE_URL/SourceCodePro-It.ttf
curl -skLo SourceCodePro-Light.ttf $BASE_URL/SourceCodePro-Light.ttf
curl -skLo SourceCodePro-LightItalic.ttf $BASE_URL/SourceCodePro-LightIt.ttf
curl -skLo SourceCodePro-Medium.ttf $BASE_URL/SourceCodePro-Medium.ttf
curl -skLo SourceCodePro-MediumItalic.ttf $BASE_URL/SourceCodePro-MediumIt.ttf
curl -skLo SourceCodePro-Regular.ttf $BASE_URL/SourceCodePro-Regular.ttf
curl -skLo SourceCodePro-SemiBold.ttf $BASE_URL/SourceCodePro-Semibold.ttf
curl -skLo SourceCodePro-SemiBoldItalic.ttf $BASE_URL/SourceCodePro-SemiboldIt.ttf
```

### See Also
   
  * original Dockerfile [](https://gitlab.com/islandoftex/images/texlive/-/blob/master/Dockerfile) extracted via command:
```sh
docker image inspect 'texlive/texlive:latest-small' |jq '.[0].Config.Labels'
```
```json
{
  "org.opencontainers.image.authors": "Island of TeX",
  "org.opencontainers.image.source": "https://gitlab.com/islandoftex/images/texlive/-/blob/master/Dockerfile",
  "org.opencontainers.image.url": "https://gitlab.com/islandoftex/images/texlive"
}
```
  * https://github.com/natlownes/docker-latex

### Author
[Serguei Kouzmine](kouzmine_serguei@yahoo.com)
