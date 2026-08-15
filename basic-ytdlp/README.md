### Info

[yt-dlp](https://github.com/yt-dlp/yt-dlp) is a fork of youtube-dl.

### Usage

* check [docker hub](https://hub.docker.com/r/jauderho/yt-dlp) for updates but uness it is failing can pin

```sh
TAG=latest
TAG=2026.03.17
docker pull jauderho/yt-dlp:$TAG
docker run --rm jauderho/yt-dlp:$TAG
```
This will pull the image and print the help info about `yt-dlp`:
```text
Usage: yt-dlp [OPTIONS] URL [URL...]

Options:

  General Options:
    -h, --help                      Print this help text and exit
    --version                       Print program version and exit
    -U, --update                    Update this program to the latest stable
                                    version
    --no-update                     Do not check for updates (default)
    --update-to [CHANNEL]@[TAG]     Upgrade/downgrade to a specific version.
                                    CHANNEL can be a repository as well. CHANNEL
                                    and TAG default to "stable" and "latest"
                                    respectively if omitted; See "UPDATE" for
                                    details. Supported channels: stable,
                                    nightly, master
    -i, --ignore-errors             Ignore download and postprocessing errors.
                                    The download will be considered successful
                                    even if the postprocessing fails
    --no-abort-on-error             Continue with next video on download errors;
                                    e.g. to skip unavailable videos in a
                                    playlist (default)
    --abort-on-error                Abort downloading of further videos if an
                                    error occurs (Alias: --no-ignore-errors)
    --list-extractors               List all supported extractors and exit
    --extractor-descriptions        Output descriptions of all supported
                                    extractors and exit
    --use-extractors NAMES          Extractor names to use separated by commas.
                                    You can also use regexes, "all", "default"
                                    and "end" (end URL matching); e.g. --ies
                                    "holodex.*,end,youtube". Prefix the name
                                    with a "-" to exclude it, e.g. --ies
                                    default,-generic. Use --list-extractors for
                                    a list of extractor names. (Alias: --ies)
    --default-search PREFIX         Use this prefix for unqualified URLs. E.g.
                                    "gvsearch2:python" downloads two videos from
                                    google videos for the search term "python".
                                    Use the value "auto" to let yt-dlp guess
                                    ("auto_warning" to emit a warning when
                                    guessing). "error" just throws an error. The
                                    default value "fixup_error" repairs broken
                                    URLs, but emits an error if this is not
                                    possible instead of searching
    --ignore-config                 Don't load any more configuration files
                                    except those given to --config-locations.
                                    For backward compatibility, if this option
                                    is found inside the system configuration
                                    file, the user configuration is not loaded.
                                    (Alias: --no-config)
...
```
NOTE, sometimes downloads do not finish

```sh
TAG=2026.03.17
NAME=ytdlp
docker run --name $NAME --entrypoint='' -it jauderho/yt-dlp:$TAG sh
```
in the container do the intended download. E.g. for subtitles

```sh
URL="https://www.youtube.com/watch?v=byK_Qta0Yyo"
yt-dlp --write-auto-sub --sub-lang en --skip-download $URL
```
if you see an `429`, will need to retry:
```text

WARNING: Your yt-dlp version (2026.03.17) is older than 90 days!
         It is strongly recommended to always use the latest version.
         Run "yt-dlp --update" or "yt-dlp -U" to update.
         To suppress this warning, add --no-update to your command/config.
[youtube] Extracting URL: https://www.youtube.com/watch?v=byK_Qta0Yyo
[youtube] byK_Qta0Yyo: Downloading webpage
[youtube] byK_Qta0Yyo: Downloading android vr player API JSON
[youtube] byK_Qta0Yyo: Downloading player bed7a914-main
[youtube] [jsc:deno] Solving JS challenges using deno
[youtube] byK_Qta0Yyo: Downloading m3u8 information
[info] byK_Qta0Yyo: Downloading subtitles: en
[info] byK_Qta0Yyo: Downloading 1 format(s): 135+140
[info] Writing video subtitles to: Johnny Boyd and Indigo Swing - ＂Pot Luck Boogie＂ LIVE [byK_Qta0Yyo].en.vtt
WARNING: The extractor specified to use impersonation for this download, but no impersonate target is available. If you encounter errors, then see  https://github.com/yt-dlp/yt-dlp#impersonation  for information on installing the required dependencies
ERROR: Unable to download video subtitles for 'en': HTTP Error 429: Too Many Requests
```
```sh
yt-dlp --write-auto-sub --sub-lang en --skip-download $URL
```
```text
WARNING: Your yt-dlp version (2026.03.17) is older than 90 days!
         It is strongly recommended to always use the latest version.
         Run "yt-dlp --update" or "yt-dlp -U" to update.
         To suppress this warning, add --no-update to your command/config.
[youtube] Extracting URL: https://www.youtube.com/watch?v=byK_Qta0Yyo
[youtube] byK_Qta0Yyo: Downloading webpage
[youtube] byK_Qta0Yyo: Downloading android vr player API JSON
[info] byK_Qta0Yyo: Downloading subtitles: en
[info] byK_Qta0Yyo: Downloading 1 format(s): 135+140
[info] Writing video subtitles to: Johnny Boyd and Indigo Swing - ＂Pot Luck Boogie＂ LIVE [byK_Qta0Yyo].en.vtt
WARNING: The extractor specified to use impersonation for this download, but no impersonate target is available. If you encounter errors, then see  https://github.com/yt-dlp/yt-dlp#impersonation  for information on installing the required dependencies
[download] Destination: Johnny Boyd and Indigo Swing - ＂Pot Luck Boogie＂ LIVE [byK_Qta0Yyo].en.vtt
[download] 100% of    1.03KiB in 00:00:03 at 327.15B/s
```
For audio use the different command
```sh
URL="https://www.youtube.com/watch?v=byK_Qta0Yyo&list=RDbyK_Qta0Yyo&start_radio=1"
yt-dlp -x --audio-format mp3 --audio-quality 320K "$URL"
```

i am trying to downoad playlist using  yt-dlp  for offline listening
```
TAG=2026.03.17
NAME=ytdlp
docker pull jauderho/yt-dlp:$TAG
docker run --name $NAME --entrypoint='' -it jauderho/yt-dlp:$TAG sh

```

```
URL="https://www.youtube.com/watch?v=V5TDl7EtpjY&list=PLN9Z4tXPWmyn
70o52-zWkNBjcTrIqKcQV"
yt-dlp -x --audio-format mp3 --audio-quality 320K "$URL"

```
One may be getting the following error on each of the audios:

```text
[download] Downloading item 5 of 6
[youtube] Extracting URL: https://www.youtube.com/watch?v=V5TDl7EtpjY
[youtube] V5TDl7EtpjY: Downloading webpage
[youtube] V5TDl7EtpjY: Downloading android vr player API JSON
[info] V5TDl7EtpjY: Downloading 1 format(s): 251
ERROR: unable to download video data: HTTP Error 403: Forbidden
```

__Reason__:  YouTube [has](https://news.ycombinator.com/item?id=45358980) been progressively enforcing [Proof of Origin](https://www.rfc-editor.org/rfc/rfc7444.txt) (PO) tokens on media requests, and missing/invalid tokens can produce exactly this sort of 403.

> NOTE: BotGuard-issued Proof-of-Origin (PO) token enforcement.The Mechanism: This token requires video playback requests to prove they are originating from a genuine client (such as an official Web browser, iOS, or Android app).The Impact: It actively prevents automated scripts, scrapers, and third-party downloaders (like yt-dlp) from fetching video URLs without a verified device attestation. Lacking this proof results in 403 Forbidden errors or IP blocks.2. The Content Frontier: C2PA CredentialsTo combat deepfakes and generic automation, YouTube introduced visual proof of origin markers:"Captured with a camera": YouTube supports the C2PA (Coalition for Content Provenance and Authenticity) standard. If a creator uses a device or software supporting C2PA (version 2.1 or higher) and the video remains completely unedited, YouTube automatically displays a verification label guaranteeing the footage is a real, unaltered recording.AI Disclosure Labels: Conversely, videos that are synthetically generated or meaningfully altered must bear a "Made with AI" label in the description. If creators do not self-disclose, YouTube uses automated tools to apply the label retroactively.3. Monetization Integrity: Crackdown on "Inauthentic Content"YouTube has rebranded its old "Repetitious Content" monetization guidelines to "Inauthentic Content".The Rule: To protect advertisers, YouTube's Partner Program aggressively strips monetization from channels that utilize mass-produced AI visuals, text-to-speech templated scripts, or low-effort generic formats.The Requirement: Creators must prove a "unique human contribution" or significant transformative value to keep earning ad revenue.I'll tailor my next response. Just tell me:Developer/scraping perspective regarding PO tokensCreator/monetization perspective regarding content labelsSecurity/compliance perspective regarding AI toolsJust reply with your choice or a new query to continue.GitHubYouTube PO Token Guide - yt-dlp/yt-dlp Wiki - GitHubProof of Origin (PO) Token is a parameter that YouTube requires to be sent with requests from some clients. Without it, requests f...SSRN eLibraryA Blockchain Framework for Automated Copyright ...Jul 17, 2025 — Proof of Origin: A Blockchain Framework for Automated Copyright Enforcement in the. Generative AI Era. Non-Fungible Token Provenan...Punchy StudioYouTube's New Move to Verify Authentic Video ContentOct 30, 2024 — How the Label Works. YouTube's new label relies on technology designed to verify the origin of videos. It uses the C2PA standard . The  token requires video playback requests to prove they are originating from a genuine clien

First reproduce with a later tag:

```sh
TAG=2026.07.04
NAME=ytdlp
docker pull jauderho/yt-dlp:$TAG
docker container stop $NAME
docker container rm $NAME
docker run --name $NAME --entrypoint='' -it jauderho/yt-dlp:$TAG sh
```
then make the command arguments verbose
```
yt-dlp -v  --extractor-args "youtube:player_client=tv" -f 251 "https://www.youtube.com/watch?v=V5TDl7EtpjY"
```
examine the error (truncated):
```text
[youtube] Extracting URL: https://www.youtube.com/watch?v=V5TDl7EtpjY
[youtube] V5TDl7EtpjY: Downloading webpage
[youtube] V5TDl7EtpjY: Downloading tv client config
[debug] [youtube] Forcing "main" player JS variant for player b0d2d49a
        original url = /s/player/b0d2d49a/player_es6.vflset/en_US/base.js
[youtube] V5TDl7EtpjY: Downloading player b0d2d49a-main
[youtube] V5TDl7EtpjY: Downloading tv player API JSON
[debug] [youtube] V5TDl7EtpjY: Detected a 15s ad skippable after 5s for tv
WARNING: [youtube] V5TDl7EtpjY: Some tv client https formats have been skipped as they are DRM protected. The current session may have an experiment that applies DRM to all videos on the tv client. See  https://github.com/yt-dlp/yt-dlp/issues/12563  for more details.
[youtube] [jsc:deno] Solving JS challenges using deno
[debug] [youtube] [jsc:deno] Using challenge solver lib script v0.8.0 (source: python package, variant: minified)
[debug] [youtube] [jsc:deno] Using challenge solver core script v0.8.0 (source: python package, variant: minified)
[debug] [youtube] [jsc:deno] Running deno: /usr/bin/deno run --ext=js --no-code-cache --no-prompt --no-remote --no-lock --node-modules-dir=none --no-config --no-npm --cached-only -
WARNING: This video is drm protected and only images are available for download. use --list-formats to see them
[debug] Sort order given by extractor: quality, res, fps, hdr:12, source, vcodec, channels, acodec, lang, proto
[debug] Formats sorted by: hasvid, ie_pref, quality, res, fps, hdr:12(7), source, vcodec, channels, acodec, lang, proto, size, br, asr, vext, aext, hasaud, id
ERROR: [youtube] V5TDl7EtpjY: Requested format is not available. Use --list-formats for a list of available formats
Traceback (most recent call last):
  File "/usr/local/bin/yt-dlp/yt_dlp/YoutubeDL.py", line 1732, in wrapper
    return func(self, *args, **kwargs)
  File "/usr/local/bin/yt-dlp/yt_dlp/YoutubeDL.py", line 1888, in __extract_info
    return self.process_ie_result(ie_result, download, extra_info)
           ~~~~~~~~~~~~~~~~~~~~~~^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  File "/usr/local/bin/yt-dlp/yt_dlp/YoutubeDL.py", line 1947, in process_ie_result
    ie_result = self.process_video_result(ie_result, download=download)
  File "/usr/local/bin/yt-dlp/yt_dlp/YoutubeDL.py", line 3097, in process_video_result
    raise ExtractorError(
        'Requested format is not available. Use --list-formats for a list of available formats',
        expected=True, video_id=info_dict['id'], ie=info_dict['extractor'])
yt_dlp.utils.ExtractorError: [youtube] V5TDl7EtpjY: Requested format is not available. Use --list-formats for a list of available formats

```
query the formats present in the media
```sh
yt-dlp -x --list-formats --audio-quality 320K "$URL"
```
```text
sb2 mhtml 48x27        0    │                 mhtml │ images                               storyboard
sb1 mhtml 45x45        1    │                 mhtml │ images                               storyboard
sb0 mhtml 90x90        1    │                 mhtml │ images                               storyboard
139 m4a   audio only      2 │    1.47MiB  49k https │ audio only       mp4a.40.5   49k 22k [en] low, m4a_dash
249 webm  audio only      2 │    1.57MiB  52k https │ audio only       opus        52k 48k [en] low, webm_dash
140 m4a   audio only      2 │    3.91MiB 129k https │ audio only       mp4a.40.2  129k 44k [en] medium, m4a_dash
251 webm  audio only      2 │    4.06MiB 134k https │ audio only       opus       134k 48k [en] medium, webm_dash
160 mp4   144x144     30    │  760.34KiB  25k https │ avc1.4d400b  25k video only          144p, mp4_dash
278 webm  144x144     30    │  451.94KiB  15k https │ vp9          15k video only          144p, webm_dash
133 mp4   240x240     30    │    1.42MiB  47k https │ avc1.4d400d  47k video only          240p, mp4_dash
242 webm  240x240     30    │  619.38KiB  20k https │ vp9          20k video only          240p, webm_dash
134 mp4   360x360     30    │    1.20MiB  40k https │ avc1.4d4015  40k video only          360p, mp4_dash
18  mp4   360x360     30  2 │ ≈  5.08MiB 168k https │ avc1.42001E      mp4a.40.2       44k [en] 360p
243 webm  360x360     30    │  875.94KiB  28k https │ vp9          28k video only          360p, webm_dash
135 mp4   480x480     30    │    1.94MiB  64k https │ avc1.4d401e  64k video only          480p, mp4_dash
244 webm  480x480     30    │    1.15MiB  38k https │ vp9          38k video only          480p, webm_dash
298 mp4   720x720     60    │    3.59MiB 119k https │ avc1.4d4020 119k video only          720p60, mp4_dash
302 webm  720x720     60    │    2.26MiB  75k https │ vp9          75k video only          720p60, webm_dash
299 mp4   1080x1080   60    │    6.18MiB 205k https │ avc1.64002a 205k video only          1080p60, mp4_dash
303 webm  1080x1080   60    │    3.77MiB 125k https │ vp9         125k video only          1080p60, webm_dash
308 webm  1440x1440   60    │    7.10MiB 235k https │ vp9         235k video only          1440p60, webm_dash
315 webm  1920x1920   60    │   10.77MiB 357k https │ vp9         357k video only          2160p60, webm_dash
```
update the ofiginal upload command. Also replace audio-qualiity argument to fetch the original audio
```
yt-dlp -x --audio-format m4a -f bestaudio "$URL"
```
this now logs success
```text
[download] Downloading item 6 of 6
[youtube] Extracting URL: https://www.youtube.com/watch?v=q1gfKOBBvuA
[youtube] q1gfKOBBvuA: Downloading webpage
[youtube] q1gfKOBBvuA: Downloading android vr player API JSON
[info] q1gfKOBBvuA: Downloading 1 format(s): 251
[download] Destination: Santa Claus Is Coming To Town - the Speakeasies' Swing Band! [q1gfKOBBvuA].webm
[download] 100% of    3.06MiB in 00:00:04 at 655.77KiB/s
[ExtractAudio] Destination: Santa Claus Is Coming To Town - the Speakeasies' Swing Band! [q1gfKOBBvuA].m4a
Deleting original file Santa Claus Is Coming To Town - the Speakeasies' Swing Band! [q1gfKOBBvuA].webm (pass -k to keep)
```

confirm the files
```
ls -1 *.m4a
```

```text
Christmas In New Orleans - the Speakeasies' Swing Band! [yPWOYbwcpmY].m4a
Merry Christmas Baby - the Speakeasies' Swing Band! [V5TDl7EtpjY].m4a
Santa Baby - the Speakeasies' Swing Band! [H1_Y9OkKyPY].m4a
Santa Claus Is Coming To Town - the Speakeasies' Swing Band! [q1gfKOBBvuA].m4a
White Christmas - the Speakeasies' Swing Band! [gmLNzNlwr1o].m4a
Zat You Santa Claus - the Speakeasies' Swing Band! [6gRWSruPUN8].m4a
```
> NOTE when rerun, may fail do detect the earlier (un)finished download and start over

```sh
NAME=ytdlp
docker exec -it $NAME sh -c "ls /downloads/*.mp3"
```

```sh
NAME=ytdlp
docker cp $NAME:/downloads .
```
```text
Successfully copied 209MB to .
```


```sh
export URL='https://www.youtube.com/watch?v=8gXkDz4yqBk&list=RD8gXkDz4yqBk&start_radio=1'; yt-dlp -x --audio-format mp3 --audio-quality 320K "$URL"

URL='https://www.youtube.com/watch?v=8gXkDz4yqBk&list=RD8gXkDz4yqBk&start_radio=1'

FILENAME='Sootmouth Boggie long version [8gXkDz4yqBk].mp3'
# FILENAME=$(echo $FILENAME|sed 's|\([[]] \)|\\\\\1|g')
FILENAME2=$(echo $FILENAME|sed 's|\[|\\\[|g; s|\]|\\\]|g; s| |\\ |g')
export FILENAME2
ID=$(docker container ls -a | grep $NAME|awk '{print $1}')
docker cp $ID:"/downloads/$FILENAME" .
```
> NOTE: the destination directory will be world read,write, execute

### Cleanup

```sh
export TAG=2026.03.17
export IMAGE=jauderho/yt-dlp:$TAG
docker stop $NAME
docker container prune -f
docker volume prune -f
docker image prune -f
docker image rm $IMAGE
```

### Author
[Serguei Kouzmine](kouzmine_serguei@yahoo.com)
