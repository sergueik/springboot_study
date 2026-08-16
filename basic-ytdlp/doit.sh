#!/bin/sh
URL=${1:-https://www.youtube.com/watch?v=6Y9ow_0y9tI}
PRUNE=${2:-true}
export TAG=2026.07.04
export NAME=ytdlp
docker pull jauderho/yt-dlp:$TAG
if [ "$PRUNE" = true ]; then
  rm downloads/*
  echo 'cleaned up downloads'
fi
docker container stop $NAME 2>/dev/null
docker container rm $NAME 2>/dev/null
docker run -d --name $NAME --entrypoint='' -it jauderho/yt-dlp:$TAG sh 
docker exec $NAME yt-dlp --list-formats "$URL" | tee /tmp/a.$$.log

QUALITY=$(grep audio /tmp/a.$$.log |awk '{print $8" "$1}' | sed 's|k$||g'|sort -n|tail -1|cut -f 2 -d ' ')
echo "Quality: ${QUALITY}"
# TODO: ERROR: [youtube] 6Y9ow_0y9tI: Sign in to confirm you’re not a bot. Use --cookies-from-browser or --cookies for the authentication. See  https://github.com/yt-dlp/yt-dlp/wiki/FAQ#how-do-i-pass-cookies-to-yt-dlp  for how to manually pass cookies. Also see  https://github.com/yt-dlp/yt-dlp/wiki/Extractors#exporting-youtube-cookies  for tips on effectively exporting YouTube cookies
# TODO: WARNING: [youtube] 6Y9ow_0y9tI: Unable to download webpage: HTTP Error 429: Too Many Requests (caused by <HTTPError 429: Too Many Requests>)

docker exec $NAME yt-dlp --write-info-json -x -f bestaudio "$URL"
docker cp $NAME:/downloads .
docker container stop $NAME 
docker container rm $NAME 
docker image prune -f 
docker image rm jauderho/yt-dlp:$TAG
# TODO: does not work with whitespace containing filenames
find 'downloads'  -maxdepth 1 -iname '*opus' -a -type f | xargs -IX echo X | while IFS= read -r D ; do ffmpeg -i "$D" -c:a flac "${D%.opus}.flac"; done 
ls -hl downloads/*

exit 0
	
