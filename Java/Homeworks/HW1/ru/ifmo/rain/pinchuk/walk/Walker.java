package ru.ifmo.rain.pinchuk.walk;

import java.nio.charset.Charset;
import java.nio.file.*;
import java.io.*;
import java.util.stream.Stream;

public class Walker {
    private static String fileHash(String path) {
        FnvHash hash = new FnvHash();

        try (InputStream input = new FileInputStream(path)) {
            int nRead;
            byte[] buf = new byte[1024];

            while ((nRead = input.read(buf)) >= 0) {
                hash.update(buf, nRead);
            }
        } catch (Throwable e) {
            return String.format("%08x %s%n", 0, path);
        }

        return String.format("%08x %s%n", hash.getHash(), path);
    }

    public static void main(String[] args) {

        if (args == null) {
            System.err.println("Bad arguments");
            return;
        }

        if (args.length != 2) {
            System.err.println("Bad argument count");
            return;
        }

        for (String arg : args) {
            if (arg == null) {
                System.err.println("Bad argument");
                return;
            }
        }

        try {
            Path path = Paths.get(args[1]);

            if (!Files.exists(path)) {
                Path parent = path.getParent();

                if (parent != null) {
                    Files.createDirectories(parent);
                }
            }
        } catch(UnsupportedOperationException | FileSystemNotFoundException | InvalidPathException | UncheckedIOException | IOException | SecurityException e) {
            System.err.println("Could not create parent directory");
            return;
        }

        try (FileWriter writer = new FileWriter(args[1])) {
            try (Stream<String> lines = Files.lines(Paths.get(args[0]), Charset.forName("UTF-8"))) {
                for (String line : (Iterable<String>) lines.map(Walker::fileHash)::iterator) {
                    try {
                        writer.write(line);
                    } catch (IOException e) {
                        System.err.println("Could write to output file");
                        return;
                    }
                }
            } catch (IOException | SecurityException | FileSystemNotFoundException | IllegalArgumentException | UncheckedIOException e) {
                System.err.println("Error while reading input file");
            }
        } catch (IOException e) {
            System.err.println("Error while opening output file");
        }
    }
}
