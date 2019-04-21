package ru.ifmo.rain.vladimirp1.reflection;

import info.kgeorgiy.java.advanced.implementor.Impler;
import info.kgeorgiy.java.advanced.implementor.ImplerException;

import java.io.File;
import java.io.IOException;
import java.lang.reflect.Method;
import java.nio.charset.Charset;
import java.nio.file.Files;
import java.nio.file.InvalidPathException;
import java.nio.file.Path;

public class Implementor implements Impler {
    private final String CLASS_SUFFIX = "Impl";
    private final String FILENAME_SUFFIX = CLASS_SUFFIX + ".java";
    private final String PACKAGE = "package ";
    private final String CLASS = " class ";
    private final String IMPLEMENTS = " implements ";
    private final String NEWLINE = String.format("%n");

    public static void main(String[] args) {
        if (args.length < 1) {
            System.err.println("Not enough arguments: please specify class name to implement");
            return;
        }

        if (args[0] == null) {
            System.err.println("Argument cannot be null");
            return;
        }

        Class<?> clazz;
        try {
            clazz = Class.forName(args[0]);
        } catch (ClassNotFoundException e) {
            System.err.println("Could not find class " + args[0]);
            return;
        }

        Implementor impl = new Implementor();
        try {
            impl.implement(clazz, Path.of("."));
        } catch (ImplerException e) {
            System.err.println("Could not implement class: " + e.getMessage());
            e.printStackTrace();
        }
    }

    private void printPackage(Class<?> clazz, StringBuilder builder) {
        String pack = clazz.getPackageName();
        if(pack != "") {
            builder.append(PACKAGE);
            builder.append(pack);
            builder.append(";" + NEWLINE + NEWLINE);
        }
    }


    @Override
    public void implement(Class<?> token, Path root) throws ImplerException {
        if (token == null || root == null) {
            throw new ImplerException("Arguments to implement() cannot be null");
        }

        if (!token.isInterface()) {
            throw new ImplerException("Cannot implement a class");
        }

        StringBuilder builder = new StringBuilder();

        printPackage(token, builder);
        builder.append(CLASS);
        builder.append(token.getSimpleName());
        builder.append(CLASS_SUFFIX);
        builder.append(IMPLEMENTS);
        builder.append(token.getCanonicalName());
        builder.append(" {");

        builder.append(NEWLINE);
        Method[] methods = token.getMethods();
        for (Method method : methods) {
            builder.append("    ");
            builder.append(MethodImplementor.implement(method));
            builder.append(NEWLINE);
        }
        builder.append(NEWLINE);

        builder.append("};");

        try {
            ImplFileWriter.writeFile(
                    root
                            .resolve(token.getPackageName().replace(".", File.separator))
                            .resolve(token.getSimpleName() + FILENAME_SUFFIX),
                    builder.toString()
            );
        } catch (ImplerException e) {
            throw new ImplerException(e);
        } catch (InvalidPathException e) {
            throw new ImplerException("Invalid path provided", e);
        }
    }

    private static class ImplFileWriter {
        static void writeFile(Path p, String content) throws ImplerException {
            try {
                Path parent = p.getParent();
                if (parent == null) {
                    throw new ImplerException("Invalid path provided");
                }

                Files.createDirectories(p.getParent());
            } catch (SecurityException e) {
                throw new ImplerException("Could not create parent directories for path " + p.toString() + ": denied by SecurityManager", e);
            } catch (IOException e) {
                throw new ImplerException("Could not create parent directories for path " + p.toString() + ": IO error", e);
            }

            try {
                Files.writeString(p, content, Charset.forName("UTF-8"));
            } catch (SecurityException e) {
                throw new ImplerException("Could not write to file " + p.toString() + ": denied by SecurityManager", e);
            } catch (IOException e) {
                throw new ImplerException("Could not write to file " + p.toString() + ": IO error", e);
            }

        }

    }

}
