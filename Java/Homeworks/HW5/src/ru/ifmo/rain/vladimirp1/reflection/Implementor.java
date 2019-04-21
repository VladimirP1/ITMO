package ru.ifmo.rain.vladimirp1.reflection;

import info.kgeorgiy.java.advanced.implementor.Impler;
import info.kgeorgiy.java.advanced.implementor.ImplerException;
import info.kgeorgiy.java.advanced.implementor.JarImpler;

import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.lang.reflect.Method;
import java.lang.ClassLoader;
import java.nio.charset.Charset;
import java.nio.file.Files;
import java.nio.file.InvalidPathException;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.*;
import java.util.jar.Attributes;
import java.util.jar.JarOutputStream;
import java.util.jar.Manifest;
import java.util.zip.ZipEntry;
import javax.tools.JavaCompiler;
import javax.tools.ToolProvider;

/**
 * Class to generate dummy implementations for interfaces, like the IDEA's implement tool.
 */
public class Implementor implements Impler, JarImpler {
    /**
     * Constructs an Implementor instance
     */
    public Implementor() {}

    /**
     * Constatnt used in code generation
     */
    private static final String CLASS_SUFFIX = "Impl";
    /**
     * Constatnt used in code generation
     */
    private static final String FILENAME_SUFFIX = CLASS_SUFFIX + ".java";
    /**
     * Constatnt used in code generation
     */
    private static final String PACKAGE = "package ";
    /**
     * Constatnt used in code generation
     */
    private static final String CLASS = " class ";
    /**
     * Constatnt used in code generation
     */
    private static final String IMPLEMENTS = " implements ";
    /**
     * Constatnt used in code generation
     */
    private static final String NEWLINE = String.format("%n");

    /**
     * Creates dummy implementations of java interfaces, optionally exporting them as jar.
     * Pass a full class name put it's implementation's files in the current direcotry.
     * Pass {@code "-jar"} to get a compiled jar with the implementation.
     *
     * @param args command line arguments.
     *             Can be {@code ["-jar", <full class name to implement>, <output jar>]} or
     *             {@code [<full class name to implement>]}
     */
    public static void main(String[] args) {
        String className;
        String jarName = null;

        if (args.length < 1) {
            System.err.println("Not enough arguments: please specify class name to implement");
            return;
        }

        if (Arrays.stream(args).anyMatch(Objects::isNull)) {
            System.err.println("Argument cannot be null");
            return;
        }

        if (args[0].equals("-jar")) {
            if (args.length != 3) {
                System.err.println("Wrong argument count");
                return;
            }
            className = args[1];
            jarName = args[2];
        } else {
            className = args[0];
        }


        Class<?> clazz;
        try {
            clazz = Class.forName(className);
        } catch (ClassNotFoundException e) {
            System.err.println("Could not find class " + className);
            return;
        }

        Implementor impl = new Implementor();
        try {
            if (jarName == null) {
                impl.implement(clazz, Path.of("."));
            } else {
                impl.implementJar(clazz, Path.of(jarName));
            }
        } catch (ImplerException e) {
            System.err.println("Could not implement class: " + e.getMessage());
            e.printStackTrace();
        }


    }

    /**
     * Appends a correct class's package directive to {@link StringBuilder}, for inclusion in generated code.
     *
     * @param clazz   The class, whose package name to append.
     * @param builder StringBuilder, where to append the output.
     */
    private void printPackage(Class<?> clazz, StringBuilder builder) {
        String pack = clazz.getPackageName();
        if (!pack.equals("")) {
            builder.append(PACKAGE);
            builder.append(pack);
            builder.append(";" + NEWLINE + NEWLINE);
        }
    }

    /**
     * Given an interface and a directory, resolves the path, where the interface's implemetation belongs to.
     *
     * @param clazz The class, for which to resolve the path
     * @param root  Base path for resolving
     * @return Path, where <i>interface name</i>Impl.java should be put.
     * @throws ImplerException when the path cannot be resolved.
     */
    private static Path getPathToSource(Class<?> clazz, Path root) throws ImplerException {
        try {
            return root
                    .resolve(clazz.getPackageName().replace(".", File.separator))
                    .resolve(clazz.getSimpleName() + FILENAME_SUFFIX);
        } catch (InvalidPathException e) {
            throw new ImplerException("Invalid path provided", e);
        }
    }


    /**
     * Produces code implementing class or interface specified by provided <i>token</i>.
     * <p>
     * Generated class classes name should be same as classes name of the type token with <i>Impl</i> suffix
     * added. Generated source code should be placed in the correct subdirectory of the specified
     * <i>root</i> directory and have correct file name. For example, the implementation of the
     * interface {@link java.util.List} should go to <i>$root/java/util/ListImpl.java</i>
     *
     * @param token type token to create implementation for.
     * @param root  root directory.
     * @throws info.kgeorgiy.java.advanced.implementor.ImplerException when implementation cannot be
     *                                                                 generated.
     */
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
                    getPathToSource(token, root),
                    builder.toString()
            );
        } catch (ImplerException e) {
            throw new ImplerException(e);
        }
    }

    /**
     * Produces <i>.jar</i> file implementing class or interface specified by provided <i>token</i>.
     * <p>
     * Generated class classes name should be same as classes name of the type token with <i>Impl</i> suffix
     * added.
     *
     * @param token   type token to create implementation for.
     * @param jarFile target <i>.jar</i> file.
     * @throws ImplerException when implementation cannot be generated.
     */
    @Override
    public void implementJar(Class<?> token, Path jarFile) throws ImplerException {
        Path root;
        try {
            root = Files.createTempDirectory(Paths.get("."), "__tmp");
        } catch (IOException | IllegalArgumentException | UnsupportedOperationException | SecurityException e) {
            throw new ImplerException("Could not create temporary direcotry", e);
        }
        try {
            implement(token, root);
            ImplJarCompiler.compileToJar(token, jarFile, root);
        } finally {
            ImplFileWriter.removeTempDirectory(root);
        }
    }

    /**
     * Helper class for File I/O.
     */
    private static class ImplFileWriter {
        /**
         * Does nothing and is private. The class is not intended for instination.
         */
        private ImplFileWriter() {}

        /**
         * Removes a temporary directory recursvely.
         *
         * @param path The directory to delete.
         * @throws ImplerException if an {@link IOException} occurs, when deleting one of the files.
         */
        static void removeTempDirectory(Path path) throws ImplerException {
            try {
                Files.walk(path)
                        .sorted(Comparator.reverseOrder())
                        .map(Path::toFile)
                        .forEach(File::delete);
            } catch (IOException | SecurityException e) {
                throw new ImplerException("Cannot delete temporary dir", e);
            }
        }

        /**
         * Creates the directory and, if neccecary its parent directories, like Unix's mkdir -p.
         *
         * @param dir The directory to create.
         * @throws ImplerException if the creation fails.
         */
        static void mkParentDir(Path dir) throws ImplerException {
            try {
                Path parent = dir.getParent();
                if (parent == null) {
                    throw new ImplerException("Invalid path provided");
                }

                Files.createDirectories(dir.getParent());
            } catch (SecurityException e) {
                throw new ImplerException("Could not create parent directories for path " + dir.toString() + ": denied by SecurityManager", e);
            } catch (IOException e) {
                throw new ImplerException("Could not create parent directories for path " + dir.toString() + ": IO error", e);
            }
        }

        /**
         * Writes content, given as {@link String} into file, encoding it in UTF-8.
         *
         * @param file    The file to write.
         * @param content String to be written into file.
         * @throws ImplerException when encounters errors
         */
        static void writeFile(Path file, String content) throws ImplerException {
            mkParentDir(file);

            StringBuilder builder = new StringBuilder();
            for (char c : content.toCharArray()) {
                builder.append(c < 128 ? Character.toString(c) : String.format("\\u%04x", (int) c));
            }

            content = builder.toString();

            try {
                Files.writeString(file, content, Charset.forName("UTF-8"));
            } catch (SecurityException e) {
                throw new ImplerException("Could not write to file " + file.toString() + ": denied by SecurityManager", e);
            } catch (IOException e) {
                throw new ImplerException("Could not write to file " + file.toString() + ": IO error", e);
            }

        }

    }

    /**
     * Helper class for compilation of generated code
     */
    private static class ImplJarCompiler {

        /**
         * Does nothing and is private. The class is not intended for instination.
         */
        private ImplJarCompiler() {}

        /**
         * Compiles a java file.
         *
         * @param source Source file to compile
         * @param root   Root dir with sources
         * @throws ImplerException is the compiler gives an error or if fails to get classpath
         */
        static void compileClass(String source, String root) throws ImplerException {
            final JavaCompiler compiler = ToolProvider.getSystemJavaCompiler();
            final String classpath;

            try {
                classpath = root + File.pathSeparator + System.getProperty("java.class.path");
            } catch (SecurityException e) {
                throw new ImplerException("Could not get classpath: denied by SecurityManager", e);
            }

            if (compiler == null) {
                throw new ImplerException("No java compilers available");
            }

            int result = compiler.run(null, null, null, "-cp", classpath, source, "-encoding", "utf8");

            if (result != 0) {
                throw new ImplerException("Internal error: could not compile the generated class");
            }
        }

        /**
         * Compiles generated code for interface <i>token</i>, from directory <i>root</i>, and exports the results into a JAR <i>jarFile</i>
         *
         * @param token   Interface to implement.
         * @param jarFile Output filename.
         * @param root    Path to implementation of interface {@code token}
         * @throws ImplerException when encounters errors
         */
        static void compileToJar(Class<?> token, Path jarFile, Path root) throws ImplerException {
            final String source = getPathToSource(token, root).toString();

            compileClass(source, root.toString());


            final Manifest manifest = new Manifest();
            final Attributes attrs = manifest.getMainAttributes();

            attrs.put(Attributes.Name.MANIFEST_VERSION, "1.0");
            attrs.put(Attributes.Name.IMPLEMENTATION_VENDOR, "VladimirP1");

            ImplFileWriter.mkParentDir(jarFile);

            try (FileOutputStream outputStream = new FileOutputStream(jarFile.toFile())) {
                try (JarOutputStream stream = new JarOutputStream(outputStream, manifest)) {

                    stream.putNextEntry(
                            new ZipEntry(
                                    token.getPackageName().replace(".", "/") + "/" +
                                            token.getSimpleName() + "Impl.class")
                    );

                    Files.copy(Paths.get(source.substring(0, source.lastIndexOf(".java")) + ".class"), stream);
                } catch (IOException e) {
                    e.printStackTrace();
                    throw new ImplerException("Internal error: cannot create jar");
                }
            } catch (IOException e) {
                e.printStackTrace();
                throw new ImplerException("Could not open jar for writing: IO error", e);
            } catch (SecurityException e) {
                throw new ImplerException("Could not open jar for writing: denied by SecurityManager", e);
            }
        }
    }

}
