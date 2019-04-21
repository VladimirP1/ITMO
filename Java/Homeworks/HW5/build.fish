#!/usr/bin/fish

mkdir -p build
cp -rLv src/* build

set ARGS -cp ../../java-advanced-2019/artifacts/info.kgeorgiy.java.advanced.implementor.jar

find build -name '*.java' | while read f ;
    set SOURCES $SOURCES "$f"
end

echo $SOURCES
javac $ARGS $SOURCES

cd build
jar -m ../MANIFEST.MF -c -f ../Implementor.jar *
