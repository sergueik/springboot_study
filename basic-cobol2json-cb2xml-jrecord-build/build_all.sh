#!/usr/bin/env sh
set -eu

##############################################################################
# CONFIGURATION
##############################################################################

WORKDIR="${WORKDIR:-$PWD/build}"
MVN_REPO="$WORKDIR/m2"

JDK_REQUIRED_MAJOR=11

##############################################################################
# TOOLS CHECK
##############################################################################

command -v git >/dev/null 2>&1 || { echo "git not found"; exit 1; }
command -v mvn >/dev/null 2>&1 || { echo "maven not found"; exit 1; }
command -v java >/dev/null 2>&1 || { echo "java not found"; exit 1; }

JAVA_MAJOR=$(java -version 2>&1 | awk -F[\".] '/version/ {print $2}')
[ "$JAVA_MAJOR" -ge "$JDK_REQUIRED_MAJOR" ] || {
  echo "Java $JDK_REQUIRED_MAJOR+ required"
  exit 1
}

##############################################################################
# CLEAN START
##############################################################################

echo "==> Cleaning work directory"
rm -rf "$WORKDIR"
mkdir -p "$WORKDIR" "$MVN_REPO"

export MVN_OPTS="-Dmaven.repo.local=$MVN_REPO -DskipTests"

##############################################################################
# FUNCTION: build from source with overridden pom
##############################################################################

build_component() {
  NAME="$1"
  REPO="$2"
  COMMIT="$3"
  OVERRIDE_POM="$4"
  AVOID_FILE="$5"

  echo
  echo "==> Building $NAME"
  cd "$WORKDIR"

  git clone "$REPO" "$NAME"
  cd "$NAME"
  git checkout "$COMMIT"

  echo "    Overwriting pom.xml with $OVERRIDE_POM"
  cp "../$OVERRIDE_POM" pom.xml

  if [ -f "../$AVOID_FILE" ]; then
    cp "../$AVOID_FILE" avoid_deps.txt
  fi
  # TODO: run two separately
  # -o / --offline Work offline
  mvn $MVN_OPTS -f pom.xml -DskipTests clean install
}

##############################################################################
# COPY OVERRIDDEN POMS & AVOID FILES INTO WORKDIR
##############################################################################

cp pom.cb2xml.xml pom.jrecord.xml pom.cobol2json.xml \
   avoid_cb2xml_deps.txt avoid_jrecord_deps.txt \
   "$WORKDIR"

##############################################################################
# 1) cb2xml
##############################################################################

build_component \
  cb2xml \
  https://github.com/bmTas/cb2xml \
  97f8dc8 \
  pom.cb2xml.xml \
  avoid_cb2xml_deps.txt

##############################################################################
# 2) JRecord
##############################################################################

build_component \
  jrecord \
  https://github.com/bmTas/JRecord \
  f50ece71 \
  pom.jrecord.xml \
  avoid_jrecord_deps.txt

##############################################################################
# 3) CobolToJson application
##############################################################################

echo
echo "==> Building CobolToJson application"

cd "$WORKDIR"
git clone https://github.com/bmTas/CobolToJson cobol2json
cd cobol2json
git checkout 99b0aa2

echo "    Overwriting pom.xml"
cp ../pom.cobol2json.xml pom.xml

# TODO: run two separately
# -o / --offline Work offline
mvn $MVN_OPTS -f pom.xml -DskipTests clean package

APP_JAR=$(ls target/*-shaded.jar 2>/dev/null || ls target/*.jar | grep -v original)
echo "    Built $APP_JAR"

##############################################################################
# 4) Inspect shaded JAR
##############################################################################

echo
echo "==> Inspecting $APP_JAR"
unzip -qc $APP_JAR META-INF/MANIFEST.MF
unzip -ql $APP_JAR | grep 'META-INF/MANIFEST.MF' >/dev/null || {
  echo 'ERROR: missing MANIFEST.MF'
  exit 1
}

echo "---- MANIFEST ----"
unzip -c $APP_JAR META-INF/MANIFEST.MF

echo
echo "---- Checking class layout ----"

unzip -ql $APP_JAR | grep '\.class$' | head -n 10

if unzip -ql $APP_JAR | grep -q '\.jar$'; then
  echo "ERROR: nested jars found (shade misconfigured)"
  exit 1
fi

echo "OK: dependencies are exploded"

##############################################################################
# 5) Validate Maven repo cleanliness
##############################################################################

echo
echo "==> Validating Maven repo isolation"

FORBIDDEN_PATTERNS="
/com/github/
/org/springframework/
/org/apache/httpcomponents/
/org/slf4j/
/ch/qos/logback/
"

for p in $FORBIDDEN_PATTERNS; do
  if find "$MVN_REPO" -type d | grep -q "$p"; then
    echo "ERROR: forbidden dependency leaked into repo: $p"
    exit 1
  fi
done

echo "OK: Maven repo contains only pinned/vendor dependencies"

##############################################################################
# DONE
##############################################################################

echo
echo "==> BUILD COMPLETE"
echo "    Shaded JAR: $APP_JAR"
echo "    Maven repo: $MVN_REPO"

