#!/usr/bin/fish

PATH=/home/vladimir/Apps/JDK/jdk-11.0.2/bin:$PATH

modulesclear() {
    MODULEPATH=.
}

moduleload() {
    for f in $1/* ; do
        MODULEPATH="$MODULEPATH":"$f"
        echo "Added" $f
    done
}

modulesload() {
    for f in $@ ; do
        echo $(readlink -f "$f")
        moduleload $(readlink -f "$f")
    done
}
