#!/bin/sh
CLASS=ChartEx
mvn clean package
java -Dprism.order=sw -cp target/gui-0.3.0-SNAPSHOT.jar:"$HOME/.m2/repository/org/openjfx/javafx-base/11.0.1/javafx-base-11.0.1.jar":"$HOME/.m2/repository/org/openjfx/javafx-base/11.0.1/javafx-base-11.0.1-linux.jar":"$HOME/.m2/repository/org/openjfx/javafx-graphics/11.0.1/javafx-graphics-11.0.1.jar":"$HOME/.m2/repository/org/openjfx/javafx-graphics/11.0.1/javafx-graphics-11.0.1-linux.jar":"$HOME/.m2/repository/org/openjfx/javafx-controls/11.0.1/javafx-controls-11.0.1-linux.jar" \
--module-path "$HOME/.m2/repository/org/openjfx/javafx-base/11.0.1/javafx-base-11.0.1.jar":"$HOME/.m2/repository/org/openjfx/javafx-base/11.0.1/javafx-base-11.0.1-linux.jar":"$HOME/.m2/repository/org/openjfx/javafx-graphics/11.0.1/javafx-graphics-11.0.1.jar":"$HOME/.m2/repository/org/openjfx/javafx-graphics/11.0.1/javafx-graphics-11.0.1-linux.jar":"$HOME/.m2/repository/org/openjfx/javafx-controls/11.0.1/javafx-controls-11.0.1-linux.jar" \
--add-modules=javafx.controls,javafx.base,javafx.graphics \
example.$CLASS
mvn install
java -Dprism.order=sw -cp target/gui-0.3.0-SNAPSHOT.jar:target/lib.* \
--module-path target/lib \
--add-modules=javafx.controls,javafx.base,javafx.graphics \
example.$CLASS
