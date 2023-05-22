#!/bin/sh
INPUT=$1
if [ -z "${INPUT}" ] ; then
  INPUT='setting: {{*key||default_value*}}'
fi
KEY=${2:name}
VALUE=$3 # may be empty
# the UCD property replacer format is
# setting: {{*key||default_value*}}
# see also: https://github.com/UrbanCode/Property-Utils-UCD

echo "INPUT=${INPUT}"
echo "KEY=${KEY}"
echo "VALUE=${VALUE}"

if [ -z "${VALUE}" ] ; then
  1>&2 echo "using default value for property $KEY"
  echo "$INPUT" | sed -n "s+{{\\*$KEY||\\(..*\\)\\*}}+\\1+p" 

else
  1>&2 echo "using provided value $VALUE for property $KEY"
  # NOTE: the VALUE may contain slashes
  echo "$INPUT" | sed -n "s+{{\\*$KEY||..*\\*}}+$VALUE+p"
fi


exit 0
# ./demo_property_update_yaml.sh '' key
# INPUT=setting: {{*key||default_value*}}
# KEY=key
# VALUE=
# using default value for property key
# setting: default_value
# ./demo_property_update_yaml.sh '' key value
# INPUT=setting: {{*key||default_value*}}
# KEY=key
# VALUE=value
# using provided value value for property key
# setting: value

