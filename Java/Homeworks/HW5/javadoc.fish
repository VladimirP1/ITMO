#!/usr/bin/fish

set EXTERNAL_FILES \
    ../../java-advanced-2019/modules/info.kgeorgiy.java.advanced.implementor/info/kgeorgiy/java/advanced/implementor/Impler.java \
    ../../java-advanced-2019/modules/info.kgeorgiy.java.advanced.implementor/info/kgeorgiy/java/advanced/implementor/ImplerException.java \
    ../../java-advanced-2019/modules/info.kgeorgiy.java.advanced.implementor/info/kgeorgiy/java/advanced/implementor/JarImpler.java \
    ../../java-advanced-2019/modules/info.kgeorgiy.java.advanced.implementor/info/kgeorgiy/java/advanced/implementor/package-info.java
    

javadoc --class-path .:$MODULEPATH -private -link https://docs.oracle.com/en/java/javase/11/docs/api -html4 -d /tmp/x (find src -name '*.java') $EXTERNAL_FILES

#xdg-open /tmp/x/index.html
