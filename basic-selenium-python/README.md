### Info

this directory contains some checks of the [alpinte python chromedriver](https://hub.docker.com/r/joyzoursky/python-chromedriver/) Docker container.

NOTE: the author's `Dockerfile` does not reflect the switch to glibc he is made with his images:
### Usage
pull the image
```sh
docker pull joyzoursky/python-chromedriver:3.9-alpine-selenium
```

run the test script launching Chromium headless and examining the version  via [CDP]() call:
```sh
docker run  -w /usr/workspace -v $(pwd):/usr/workspace -it joyzoursky/python-chromedriver:3.9-alpine-selenium sh
```
in the container prompt `/usr/workspace #`, proceed with

```sh
python chromium_ex.py
```
this will print (the *version* may vary)
```text
Browser.getVersion:
POST to http://127.0.0.1:45035/session/fc364a56de90413a9471d510b81fc641/chromium/send_command_and_get_result
params: {"cmd": "Browser.getVersion", "params": {}}
dict_keys(['value'])
dict_keys(['jsVersion', 'product', 'protocolVersion', 'revision', 'userAgent'])
jsVersion: 9.1.269.36
product: HeadlessChrome/91.0.4472.101
revision: @af52a90bf87030dd1523486a1cd3ae25c5d76c9b
userAgent: "Chromium 95.0.4638.69"
```
proceed with examining the binary dependencies (NOTE, the `/usr/bin/chromium` is a softlink):
```sh
ldd  /usr/lib/chromium/chrome
```	
```text
/lib/ld-musl-x86_64.so.1 (0x7f3a2bf05000)
libatomic.so.1 => /usr/lib/libatomic.so.1 (0x7f3a21b55000)
libglib-2.0.so.0 => /usr/lib/libglib-2.0.so.0 (0x7f3a21a45000)
libgobject-2.0.so.0 => /usr/lib/libgobject-2.0.so.0 (0x7f3a219f5000)
libsmime3.so => /usr/lib/libsmime3.so (0x7f3a219cd000)
libnss3.so => /usr/lib/libnss3.so (0x7f3a218bd000)
libnssutil3.so => /usr/lib/libnssutil3.so (0x7f3a2188d000)
libnspr4.so => /usr/lib/libnspr4.so (0x7f3a21845000)
libatk-1.0.so.0 => /usr/lib/libatk-1.0.so.0 (0x7f3a2181d000)
libatk-bridge-2.0.so.0 => /usr/lib/libatk-bridge-2.0.so.0 (0x7f3a217e5000)
libevent-2.1.so.7 => /usr/lib/libevent-2.1.so.7 (0x7f3a21795000)
libcups.so.2 => /usr/lib/libcups.so.2 (0x7f3a2170d000)
libdrm.so.2 => /usr/lib/libdrm.so.2 (0x7f3a216f5000)
libfontconfig.so.1 => /usr/lib/libfontconfig.so.1 (0x7f3a216b5000)
libdbus-1.so.3 => /usr/lib/libdbus-1.so.3 (0x7f3a21665000)
libgio-2.0.so.0 => /usr/lib/libgio-2.0.so.0 (0x7f3a214b5000)
libpng16.so.16 => /usr/lib/libpng16.so.16 (0x7f3a2147d000)
libz.so.1 => /lib/libz.so.1 (0x7f3a2145d000)
libwebpdemux.so.2 => /usr/lib/libwebpdemux.so.2 (0x7f3a21455000)
libwebpmux.so.3 => /usr/lib/libwebpmux.so.3 (0x7f3a21445000)
libwebp.so.7 => /usr/lib/libwebp.so.7 (0x7f3a213ed000)
libfreetype.so.6 => /usr/lib/libfreetype.so.6 (0x7f3a21335000)
libjpeg.so.8 => /usr/lib/libjpeg.so.8 (0x7f3a212b5000)
libexpat.so.1 => /usr/lib/libexpat.so.1 (0x7f3a2128d000)
libharfbuzz-subset.so.0 => /usr/lib/libharfbuzz-subset.so.0 (0x7f3a21215000)
libharfbuzz.so.0 => /usr/lib/libharfbuzz.so.0 (0x7f3a2116d000)
libxcb.so.1 => /usr/lib/libxcb.so.1 (0x7f3a21145000)
libxkbcommon.so.0 => /usr/lib/libxkbcommon.so.0 (0x7f3a21105000)
libopus.so.0 => /usr/lib/libopus.so.0 (0x7f3a210a5000)
libavcodec.so.58 => /usr/lib/libavcodec.so.58 (0x7f3a1fd65000)
libavformat.so.58 => /usr/lib/libavformat.so.58 (0x7f3a1faed000)
libavutil.so.56 => /usr/lib/libavutil.so.56 (0x7f3a1f82d000)
libX11.so.6 => /usr/lib/libX11.so.6 (0x7f3a1f705000)
libXcomposite.so.1 => /usr/lib/libXcomposite.so.1 (0x7f3a1f6fd000)
libXdamage.so.1 => /usr/lib/libXdamage.so.1 (0x7f3a1f6f5000)
libXext.so.6 => /usr/lib/libXext.so.6 (0x7f3a1f6dd000)
libXfixes.so.3 => /usr/lib/libXfixes.so.3 (0x7f3a1f6d5000)
libXrandr.so.2 => /usr/lib/libXrandr.so.2 (0x7f3a1f6c5000)
libre2.so.9 => /usr/lib/libre2.so.9 (0x7f3a1f67d000)
libgbm.so.1 => /usr/lib/libgbm.so.1 (0x7f3a1f66d000)
libpango-1.0.so.0 => /usr/lib/libpango-1.0.so.0 (0x7f3a1f625000)
libcairo.so.2 => /usr/lib/libcairo.so.2 (0x7f3a1f535000)
libgtk-3.so.0 => /usr/lib/libgtk-3.so.0 (0x7f3a1eddd000)
libgdk-3.so.0 => /usr/lib/libgdk-3.so.0 (0x7f3a1eced000)
libasound.so.2 => /usr/lib/libasound.so.2 (0x7f3a1ebfd000)
libsnappy.so.1 => /usr/lib/libsnappy.so.1 (0x7f3a1ebed000)
libatspi.so.0 => /usr/lib/libatspi.so.0 (0x7f3a1ebb5000)
libxml2.so.2 => /usr/lib/libxml2.so.2 (0x7f3a1ea85000)
libFLAC.so.8 => /usr/lib/libFLAC.so.8 (0x7f3a1ea4d000)
libxshmfence.so.1 => /usr/lib/libxshmfence.so.1 (0x7f3a1ea45000)
libxslt.so.1 => /usr/lib/libxslt.so.1 (0x7f3a1ea05000)
liblcms2.so.2 => /usr/lib/liblcms2.so.2 (0x7f3a1e9ad000)
libstdc++.so.6 => /usr/lib/libstdc++.so.6 (0x7f3a1e805000)
libgcc_s.so.1 => /usr/lib/libgcc_s.so.1 (0x7f3a1e7e5000)
libc.musl-x86_64.so.1 => /lib/ld-musl-x86_64.so.1 (0x7f3a2bf05000)
libpcre.so.1 => /usr/lib/libpcre.so.1 (0x7f3a1e785000)
libintl.so.8 => /usr/lib/libintl.so.8 (0x7f3a1e775000)
libffi.so.7 => /usr/lib/libffi.so.7 (0x7f3a1e765000)
libplc4.so => /usr/lib/libplc4.so (0x7f3a1e75d000)
libplds4.so => /usr/lib/libplds4.so (0x7f3a1e755000)
libavahi-common.so.3 => /usr/lib/libavahi-common.so.3 (0x7f3a1e745000)
libavahi-client.so.3 => /usr/lib/libavahi-client.so.3 (0x7f3a1e72d000)
libgnutls.so.30 => /usr/lib/libgnutls.so.30 (0x7f3a1e55d000)
libuuid.so.1 => /lib/libuuid.so.1 (0x7f3a1e54d000)
libgmodule-2.0.so.0 => /usr/lib/libgmodule-2.0.so.0 (0x7f3a1e545000)
libmount.so.1 => /lib/libmount.so.1 (0x7f3a1e4ed000)
libbz2.so.1 => /usr/lib/libbz2.so.1 (0x7f3a1e4dd000)
libbrotlidec.so.1 => /usr/lib/libbrotlidec.so.1 (0x7f3a1e4cd000)
libgraphite2.so.3 => /usr/lib/libgraphite2.so.3 (0x7f3a1e4ad000)
libXau.so.6 => /usr/lib/libXau.so.6 (0x7f3a1e4a5000)
libXdmcp.so.6 => /usr/lib/libXdmcp.so.6 (0x7f3a1e49d000)
libswresample.so.3 => /usr/lib/libswresample.so.3 (0x7f3a1e47d000)
libvpx.so.6 => /usr/lib/libvpx.so.6 (0x7f3a1e265000)
libdav1d.so.5 => /usr/lib/libdav1d.so.5 (0x7f3a1e115000)
libaom.so.0 => /usr/lib/libaom.so.0 (0x7f3a1dcf5000)
libmp3lame.so.0 => /usr/lib/libmp3lame.so.0 (0x7f3a1dc85000)
libtheoraenc.so.1 => /usr/lib/libtheoraenc.so.1 (0x7f3a1dc45000)
libtheoradec.so.1 => /usr/lib/libtheoradec.so.1 (0x7f3a1dc25000)
libvorbis.so.0 => /usr/lib/libvorbis.so.0 (0x7f3a1dbfd000)
libvorbisenc.so.2 => /usr/lib/libvorbisenc.so.2 (0x7f3a1db4d000)
libx264.so.161 => /usr/lib/libx264.so.161 (0x7f3a1d82d000)
libx265.so.192 => /usr/lib/libx265.so.192 (0x7f3a1d3bd000)
libxvidcore.so.4 => /usr/lib/libxvidcore.so.4 (0x7f3a1d2ed000)
libva.so.2 => /usr/lib/libva.so.2 (0x7f3a1d2c5000)
libsrt.so.1 => /usr/lib/libsrt.so.1 (0x7f3a1d235000)
libssh.so.4 => /usr/lib/libssh.so.4 (0x7f3a1d1d5000)
libva-drm.so.2 => /usr/lib/libva-drm.so.2 (0x7f3a1d1cd000)
libvdpau.so.1 => /usr/lib/libvdpau.so.1 (0x7f3a1d1c5000)
libvulkan.so.1 => /usr/lib/libvulkan.so.1 (0x7f3a1d175000)
libXrender.so.1 => /usr/lib/libXrender.so.1 (0x7f3a1d165000)
libwayland-server.so.0 => /usr/lib/libwayland-server.so.0 (0x7f3a1d14d000)
libfribidi.so.0 => /usr/lib/libfribidi.so.0 (0x7f3a1d12d000)
libpixman-1.so.0 => /usr/lib/libpixman-1.so.0 (0x7f3a1d095000)
libxcb-shm.so.0 => /usr/lib/libxcb-shm.so.0 (0x7f3a1d08d000)
libxcb-render.so.0 => /usr/lib/libxcb-render.so.0 (0x7f3a1d07d000)
libpangocairo-1.0.so.0 => /usr/lib/libpangocairo-1.0.so.0 (0x7f3a1d06d000)
libpangoft2-1.0.so.0 => /usr/lib/libpangoft2-1.0.so.0 (0x7f3a1d055000)
libcairo-gobject.so.2 => /usr/lib/libcairo-gobject.so.2 (0x7f3a1d045000)
libgdk_pixbuf-2.0.so.0 => /usr/lib/libgdk_pixbuf-2.0.so.0 (0x7f3a1d015000)
libepoxy.so.0 => /usr/lib/libepoxy.so.0 (0x7f3a1cf05000)
libXi.so.6 => /usr/lib/libXi.so.6 (0x7f3a1ceed000)
libwayland-client.so.0 => /usr/lib/libwayland-client.so.0 (0x7f3a1cedd000)
libwayland-cursor.so.0 => /usr/lib/libwayland-cursor.so.0 (0x7f3a1cecd000)
libwayland-egl.so.1 => /usr/lib/libwayland-egl.so.1 (0x7f3a1cec5000)
libXcursor.so.1 => /usr/lib/libXcursor.so.1 (0x7f3a1ceb5000)
libXinerama.so.1 => /usr/lib/libXinerama.so.1 (0x7f3a1cead000)
liblzma.so.5 => /usr/lib/liblzma.so.5 (0x7f3a1ce85000)
libogg.so.0 => /usr/lib/libogg.so.0 (0x7f3a1ce75000)
libp11-kit.so.0 => /usr/lib/libp11-kit.so.0 (0x7f3a1cd65000)
libunistring.so.2 => /usr/lib/libunistring.so.2 (0x7f3a1cbed000)
libtasn1.so.6 => /usr/lib/libtasn1.so.6 (0x7f3a1cbd5000)
libnettle.so.8 => /usr/lib/libnettle.so.8 (0x7f3a1cb8d000)
libhogweed.so.6 => /usr/lib/libhogweed.so.6 (0x7f3a1cb45000)
libgmp.so.10 => /usr/lib/libgmp.so.10 (0x7f3a1cadd000)
libblkid.so.1 => /lib/libblkid.so.1 (0x7f3a1ca8d000)
libbrotlicommon.so.1 => /usr/lib/libbrotlicommon.so.1 (0x7f3a1ca65000)
libbsd.so.0 => /usr/lib/libbsd.so.0 (0x7f3a1ca4d000)
libsoxr.so.0 => /usr/lib/libsoxr.so.0 (0x7f3a1c9e5000)
libcrypto.so.1.1 => /lib/libcrypto.so.1.1 (0x7f3a1c75d000)
libmd.so.0 => /usr/lib/libmd.so.0 (0x7f3a1c74d000)
libgomp.so.1 => /usr/lib/libgomp.so.1 (0x7f3a1c70d000)

```

### Usage
the container used to run the scripts from mapped directory, currently as root.
```sh
IMAGE=python-selenium 
docker build -f Dockerfile -t $IMAGE .
```
```sh
docker run -it -w /usr/workspace -v $(pwd):/usr/workspace $IMAGE sh
```
```sh
python test_script.py
```
is failing with glibc related errors:
```text
Error relocating /usr/lib/libpango-1.0.so.0: g_memdup2: symbol not found
Error relocating /usr/lib/chromium/chrome: wl_proxy_marshal_flags: symbol not found
```
and the loader command output is:
```sh
ldd /usr/lib/chromium/chrome
```
```text
/lib/ld-musl-x86_64.so.1 (0x7fcf25340000)
libatomic.so.1 => /usr/lib/libatomic.so.1 (0x7fcf1ab85000)
libglib-2.0.so.0 => /usr/lib/libglib-2.0.so.0 (0x7fcf1aa86000)
libgobject-2.0.so.0 => /usr/lib/libgobject-2.0.so.0 (0x7fcf1aa3c000)
libsmime3.so => /usr/lib/libsmime3.so (0x7fcf1aa16000)
libnss3.so => /usr/lib/libnss3.so (0x7fcf1a908000)
libnssutil3.so => /usr/lib/libnssutil3.so (0x7fcf1a8d9000)
libnspr4.so => /usr/lib/libnspr4.so (0x7fcf1a896000)
libatk-1.0.so.0 => /usr/lib/libatk-1.0.so.0 (0x7fcf1a870000)
libatk-bridge-2.0.so.0 => /usr/lib/libatk-bridge-2.0.so.0 (0x7fcf1a83c000)
libgio-2.0.so.0 => /usr/lib/libgio-2.0.so.0 (0x7fcf1a6ab000)
libcups.so.2 => /usr/lib/libcups.so.2 (0x7fcf1a624000)
libdrm.so.2 => /usr/lib/libdrm.so.2 (0x7fcf1a612000)
libfontconfig.so.1 => /usr/lib/libfontconfig.so.1 (0x7fcf1a5d5000)
libdbus-1.so.3 => /usr/lib/libdbus-1.so.3 (0x7fcf1a58b000)
libevent-2.1.so.7 => /usr/lib/libevent-2.1.so.7 (0x7fcf1a53e000)
libpng16.so.16 => /usr/lib/libpng16.so.16 (0x7fcf1a50e000)
libz.so.1 => /lib/libz.so.1 (0x7fcf1a4f4000)
libwebpdemux.so.2 => /usr/lib/libwebpdemux.so.2 (0x7fcf1a4ee000)
libwebpmux.so.3 => /usr/lib/libwebpmux.so.3 (0x7fcf1a4e3000)
libwebp.so.7 => /usr/lib/libwebp.so.7 (0x7fcf1a48c000)
libfreetype.so.6 => /usr/lib/libfreetype.so.6 (0x7fcf1a3d8000)
libjpeg.so.8 => /usr/lib/libjpeg.so.8 (0x7fcf1a359000)
libexpat.so.1 => /usr/lib/libexpat.so.1 (0x7fcf1a336000)
libharfbuzz-subset.so.0 => /usr/lib/libharfbuzz-subset.so.0 (0x7fcf1a2c4000)
libharfbuzz.so.0 => /usr/lib/libharfbuzz.so.0 (0x7fcf1a220000)
libxcb.so.1 => /usr/lib/libxcb.so.1 (0x7fcf1a1f9000)
libxkbcommon.so.0 => /usr/lib/libxkbcommon.so.0 (0x7fcf1a1ba000)
libopus.so.0 => /usr/lib/libopus.so.0 (0x7fcf1a15a000)
libavcodec.so.58 => /usr/lib/libavcodec.so.58 (0x7fcf18e1c000)
libavformat.so.58 => /usr/lib/libavformat.so.58 (0x7fcf18ba4000)
libavutil.so.56 => /usr/lib/libavutil.so.56 (0x7fcf188ea000)
libX11.so.6 => /usr/lib/libX11.so.6 (0x7fcf187c8000)
libXcomposite.so.1 => /usr/lib/libXcomposite.so.1 (0x7fcf187c3000)
libXdamage.so.1 => /usr/lib/libXdamage.so.1 (0x7fcf187be000)
libXext.so.6 => /usr/lib/libXext.so.6 (0x7fcf187ab000)
libXfixes.so.3 => /usr/lib/libXfixes.so.3 (0x7fcf187a3000)
libXrandr.so.2 => /usr/lib/libXrandr.so.2 (0x7fcf18797000)
libgbm.so.1 => /usr/lib/libgbm.so.1 (0x7fcf18788000)
libre2.so.9 => /usr/lib/libre2.so.9 (0x7fcf18740000)
libpango-1.0.so.0 => /usr/lib/libpango-1.0.so.0 (0x7fcf186fa000)
libcairo.so.2 => /usr/lib/libcairo.so.2 (0x7fcf1860c000)
libasound.so.2 => /usr/lib/libasound.so.2 (0x7fcf18521000)
libsnappy.so.1 => /usr/lib/libsnappy.so.1 (0x7fcf18516000)
libatspi.so.0 => /usr/lib/libatspi.so.0 (0x7fcf184df000)
libxml2.so.2 => /usr/lib/libxml2.so.2 (0x7fcf183b5000)
libFLAC.so.8 => /usr/lib/libFLAC.so.8 (0x7fcf18381000)
libxshmfence.so.1 => /usr/lib/libxshmfence.so.1 (0x7fcf1837c000)
libxslt.so.1 => /usr/lib/libxslt.so.1 (0x7fcf18342000)
liblcms2.so.2 => /usr/lib/liblcms2.so.2 (0x7fcf182ed000)
libstdc++.so.6 => /usr/lib/libstdc++.so.6 (0x7fcf1814c000)
libgcc_s.so.1 => /usr/lib/libgcc_s.so.1 (0x7fcf18132000)
libc.musl-x86_64.so.1 => /lib/ld-musl-x86_64.so.1 (0x7fcf25340000)
libpcre.so.1 => /usr/lib/libpcre.so.1 (0x7fcf180d6000)
libintl.so.8 => /usr/lib/libintl.so.8 (0x7fcf180ca000)
libffi.so.6 => /usr/lib/libffi.so.6 (0x7fcf180c1000)
libplc4.so => /usr/lib/libplc4.so (0x7fcf180ba000)
libplds4.so => /usr/lib/libplds4.so (0x7fcf180b5000)
libgmodule-2.0.so.0 => /usr/lib/libgmodule-2.0.so.0 (0x7fcf180b0000)
libmount.so.1 => /lib/libmount.so.1 (0x7fcf1805d000)
libavahi-common.so.3 => /usr/lib/libavahi-common.so.3 (0x7fcf1804f000)
libavahi-client.so.3 => /usr/lib/libavahi-client.so.3 (0x7fcf1803d000)
libgnutls.so.30 => /usr/lib/libgnutls.so.30 (0x7fcf17e70000)
libuuid.so.1 => /lib/libuuid.so.1 (0x7fcf17e67000)
libbz2.so.1 => /usr/lib/libbz2.so.1 (0x7fcf17e58000)
libbrotlidec.so.1 => /usr/lib/libbrotlidec.so.1 (0x7fcf17e4b000)
libgraphite2.so.3 => /usr/lib/libgraphite2.so.3 (0x7fcf17e2c000)
libXau.so.6 => /usr/lib/libXau.so.6 (0x7fcf17e27000)
libXdmcp.so.6 => /usr/lib/libXdmcp.so.6 (0x7fcf17e1f000)
libswresample.so.3 => /usr/lib/libswresample.so.3 (0x7fcf17dff000)
libvpx.so.6 => /usr/lib/libvpx.so.6 (0x7fcf17beb000)
libdav1d.so.5 => /usr/lib/libdav1d.so.5 (0x7fcf17aa1000)
libaom.so.0 => /usr/lib/libaom.so.0 (0x7fcf17681000)
libmp3lame.so.0 => /usr/lib/libmp3lame.so.0 (0x7fcf17615000)
libtheoraenc.so.1 => /usr/lib/libtheoraenc.so.1 (0x7fcf175dc000)
libtheoradec.so.1 => /usr/lib/libtheoradec.so.1 (0x7fcf175c2000)
libvorbis.so.0 => /usr/lib/libvorbis.so.0 (0x7fcf1759a000)
libvorbisenc.so.2 => /usr/lib/libvorbisenc.so.2 (0x7fcf174f0000)
libx264.so.161 => /usr/lib/libx264.so.161 (0x7fcf171d4000)
libx265.so.192 => /usr/lib/libx265.so.192 (0x7fcf16d67000)
libxvidcore.so.4 => /usr/lib/libxvidcore.so.4 (0x7fcf16c9a000)
libva.so.2 => /usr/lib/libva.so.2 (0x7fcf16c73000)
libsrt.so.1 => /usr/lib/libsrt.so.1 (0x7fcf16be8000)
libssh.so.4 => /usr/lib/libssh.so.4 (0x7fcf16b8b000)
libva-drm.so.2 => /usr/lib/libva-drm.so.2 (0x7fcf16b86000)
libvdpau.so.1 => /usr/lib/libvdpau.so.1 (0x7fcf16b81000)
libvulkan.so.1 => /usr/lib/libvulkan.so.1 (0x7fcf16b31000)
libXrender.so.1 => /usr/lib/libXrender.so.1 (0x7fcf16b25000)
libwayland-server.so.0 => /usr/lib/libwayland-server.so.0 (0x7fcf16b11000)
libfribidi.so.0 => /usr/lib/libfribidi.so.0 (0x7fcf16af4000)
libpixman-1.so.0 => /usr/lib/libpixman-1.so.0 (0x7fcf16a5e000)
libxcb-shm.so.0 => /usr/lib/libxcb-shm.so.0 (0x7fcf16a59000)
libxcb-render.so.0 => /usr/lib/libxcb-render.so.0 (0x7fcf16a4a000)
libXi.so.6 => /usr/lib/libXi.so.6 (0x7fcf16a38000)
liblzma.so.5 => /usr/lib/liblzma.so.5 (0x7fcf16a15000)
libogg.so.0 => /usr/lib/libogg.so.0 (0x7fcf16a0b000)
libblkid.so.1 => /lib/libblkid.so.1 (0x7fcf169c1000)
libp11-kit.so.0 => /usr/lib/libp11-kit.so.0 (0x7fcf168ab000)
libunistring.so.2 => /usr/lib/libunistring.so.2 (0x7fcf16735000)
libtasn1.so.6 => /usr/lib/libtasn1.so.6 (0x7fcf16721000)
libnettle.so.8 => /usr/lib/libnettle.so.8 (0x7fcf166dc000)
libhogweed.so.6 => /usr/lib/libhogweed.so.6 (0x7fcf16695000)
libgmp.so.10 => /usr/lib/libgmp.so.10 (0x7fcf1662e000)
libbrotlicommon.so.1 => /usr/lib/libbrotlicommon.so.1 (0x7fcf16609000)
libbsd.so.0 => /usr/lib/libbsd.so.0 (0x7fcf165f6000)
libsoxr.so.0 => /usr/lib/libsoxr.so.0 (0x7fcf16593000)
libcrypto.so.1.1 => /lib/libcrypto.so.1.1 (0x7fcf16314000)
libmd.so.0 => /usr/lib/libmd.so.0 (0x7fcf16308000)
libgomp.so.1 => /usr/lib/libgomp.so.1 (0x7fcf162c8000)
Error relocating /usr/lib/libpango-1.0.so.0: g_memdup2: symbol not found

```
### See Also
 
  * https://github.com/joyzoursky/docker-python-chromedriver/blob/master/deprecated/py3.6-xvfb-selenium/Dockerfile
  * https://github.com/joyzoursky/docker-python-chromedriver/blob/master/py-alpine/3.8-alpine-selenium/Dockerfile
  * https://github.com/cypress-io/cypress/issues/419
