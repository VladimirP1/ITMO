#!/usr/bin/fish

set PATH /home/vladimir/Apps/JDK/jdk-11.0.2/bin $PATH

function modulesclear
    set -g MODULEPATH .
end

function moduleload
    for f in $argv[1]/*
        set MODULEPATH "$MODULEPATH":"$f"
        echo "Added" $f
    end
    set -g MODULEPATH "$MODULEPATH"
end

function modulesload
    for f in $argv
        echo (readlink -f "$f")
        moduleload (readlink -f "$f")
    end
    set -g MODULEPATH "$MODULEPATH"
end
