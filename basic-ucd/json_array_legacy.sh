#!/bin/sh

# extract RELEASE_COMPONENT_NAMES into bash array variable from release descriptor custom JSON rowset keys
DEFAULT_RELEASE_DESCRIPTOR='release.json'
RELEASE_DESCRIPTOR=${1-$DEFAULT_RELEASE_DESCRIPTOR}
echo "Processing release descriptor file $RELEASE_DESCRIPTOR"
RELEASE_COMPONENT_NAMES=""
RESULT_FILE='release_components-1.txt'
TMP_FILE=/tmp/a$$.txt

(
jq -r '.versions|flatten' $RELEASE_DESCRIPTOR|jq -r '[.[]|keys]'|jq -cr 'flatten|.[]'| while read RELEASE_COMPONENT ;
do
  echo $RELEASE_COMPONENT
done
) |sort|tee $TMP_FILE >/dev/null
mv $TMP_FILE $RESULT_FILE
echo "Release components in $RESULT_FILE"
RELEASE_COMPONENT_NAMES=$(cat $RESULT_FILE | tr '\n' ' ' )
echo "RELEASE_COMPONENT_NAMES=$RELEASE_COMPONENT_NAMES"

# NOTE: $RELEASE_COMPONENT_NAMES is not a bash array in this  script
RESULT_FILE='release_components-2.txt'
(
for NAME in "$RELEASE_COMPONENT_NAMES"; do echo $NAME; done
) |sort|tee $TMP_FILE >/dev/null
mv $TMP_FILE $RESULT_FILE
echo "Release components in $RESULT_FILE"
cat $RESULT_FILE


# intersection
# NOTE: one should use back ticks for pre-bash command invocation
EXAMPLE_RESULT=$(sed 's|  *|\n|g' < $RESULT_FILE|tail -1)
echo "Looking for $EXAMPLE_RESULT"

cat $RESULT_FILE| tr ' ' '\n'  | while read LINE ; do
  if [ ! -z "$(echo "$LINE" | grep "$EXAMPLE_RESULT")" ]; then
    echo "Found $EXAMPLE_RESULT"
  fi
done
