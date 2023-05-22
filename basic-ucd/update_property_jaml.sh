#!/bin/sh
FILE=${1:-application.yaml}
KEY=${2:name}
VALUE=${3} # may be empty
# the UCD property replacer format is
# key: {{*value||default_value*}}
#
if [ -z $VALUE ] ; then
  1>&2 echo "using default value for property $KEY"
  sed -i "" $FILE
  sed -i "s+{{\\*$KEY||\\(..*\\)\\*}}+\\1+g" $FILE

else
  1>&2 echo "using provided value $VALUE for property $KEY"
  # NOTE: the VALUE may contain slashes
  sed -i "s+{{\\*$KEY||..*\\*}}+$VALUE+g" $FILE
fi


exit 0
# 
# cat application.yaml
# ---
# application:
#   property1: {{*name1||default1*}}
#   property2: {{*name2||default2*}}
# 
# ./update_property_jaml.sh application.yaml name1
# ./update_property_jaml.sh application.yaml name1 ''
# using default value for property name1
# 
# 
# cat application.yaml
# ---
# application:
#   property1: default1
#   property2: {{*name2||default2*}}
# 
# ./update_property_jaml.sh application.yaml name2 value2
# using provided value value2 for property name2
#
#  cat application.yaml
# ---
# application:
#   property1: default1
#   property2: value2
# 
