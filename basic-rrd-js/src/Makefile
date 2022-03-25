#
# This make file creates the javascriptrdd.js file
# that contains all the other library files
# It also strips out any indentation, to save space
# 
# While the users can opt to include the single modules
# having a single file makes for easier development.
#

all: lib/javascriptrrd.js

lib/javascriptrrd.js: \
   lib/binaryXHR.js lib/rrdFile.js \
   lib/rrdFlotSupport.js lib/rrdFlot.js lib/rrdFlotMatrix.js \
   lib/rrdFilter.js lib/rrdMultiFile.js \
   lib/rrdFlotAsync.js
	cat $^ > $@

wlibs: lib/javascriptrrd.wlibs.js

lib/javascriptrrd.wlibs.js: lib/javascriptrrd.js \
   ../flot/jquery.min.js ../flot/jquery.flot.min.js  ../flot/jquery.flot.time.min.js \
   ../flot/jquery.flot.selection.min.js ../flot/jquery.flot.tooltip.min.js
	cat $^ > $@

#the flot.time binaru only got added in v0.8 flot
#create an empty file if it does not exist for consistency
../flot/jquery.flot.time.min.js:
	touch -a ../flot/jquery.flot.time.min.js
	touch -a ../flot/jquery.flot.time.js

# in the past we were not shipping the min version, so just use the unminimized one, if needed
../flot/jquery.flot.tooltip.min.js:
	cp ../flot/jquery.flot.tooltip.js ../flot/jquery.flot.tooltip.min.js

clean:
	rm -f lib/javascriptrrd.js lib/javascriptrrd.wlibs.js
