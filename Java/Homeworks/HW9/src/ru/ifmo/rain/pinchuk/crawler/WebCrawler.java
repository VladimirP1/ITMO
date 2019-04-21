package ru.ifmo.rain.pinchuk.crawler;

import info.kgeorgiy.java.advanced.crawler.*;

import java.io.IOException;
import java.net.MalformedURLException;
import java.util.*;
import java.util.concurrent.*;
import java.util.concurrent.atomic.AtomicLong;

public class WebCrawler implements Crawler {
    private final int DOWNLOADERS_LIMIT = 64;
    private final int EXTRACTORS_LIMIT = 64;

    private final int perHost;
    private final Downloader downloader;
    private final ExecutorService downloaderPool;
    private final ExecutorService extractorPool;

    /**
     * Construct a WebCrawler.
     * @param downloader Downloader to download pages with.
     * @param downloaders Number of parallel download jobs.
     * @param extractors Number of parallel link extract jobs.
     * @param perHost Max connections per host.
     */
    public WebCrawler(Downloader downloader, int downloaders, int extractors, int perHost) {
        downloaders = constrain(downloaders, 0, DOWNLOADERS_LIMIT);
        extractors = constrain(extractors, 0, EXTRACTORS_LIMIT);

        this.perHost = perHost;
        this.downloader = downloader;
        this.downloaderPool = new ThreadPoolExecutor(downloaders, downloaders, 1, TimeUnit.SECONDS, new LinkedBlockingQueue<>());
        this.extractorPool = new ThreadPoolExecutor(extractors, extractors, 1, TimeUnit.SECONDS, new LinkedBlockingQueue<>());
    }


    /**
     * Crawls web sites recursively.
     *
     * @param url Starting URL.
     * @param depth Max depth to crawl.
     * @return Result containing errors and visited pages.
     */
    @Override
    public Result download(String url, int depth) {
        final DoneDetector d = new DoneDetector();
        final Set<String> visited = Collections.newSetFromMap(new ConcurrentHashMap<>());
        final Map<String, Semaphore> limit = new ConcurrentHashMap<>();
        final Set<String> downloaded = Collections.newSetFromMap(new ConcurrentHashMap<>());
        final Map<String, IOException> errors = new ConcurrentHashMap<>();

        visited.add(url);

        downloaderPool.submit(() -> {
            download(url, visited, limit, downloaded, errors, depth, d);
        });

        try {
            d.waitCompleted();
        } catch (InterruptedException e) {
            Thread.currentThread().interrupt();
        }

        return new Result(new ArrayList<>(downloaded), errors);
    }

    private void extract(
            String url,
            Set<String> visited,
            Map<String, Semaphore> limit,
            Set<String> downloaded,
            Map<String, IOException> errors,
            int depth,
            DoneDetector det,
            Document d
    ) {
        List<String> links;
        try {
            links = d.extractLinks();
        } catch (IOException e) {
            errors.put(url, e);
            det.refDec();
            return;
        } catch (Throwable e) {
            det.refDec();
            throw e;
        }

        for (String link : links) {
            if (visited.add(link)) {
                det.refInc();
                downloaderPool.submit(() -> {
                    download(link, visited, limit, downloaded, errors, depth - 1, det);
                });
            }
        }

        det.refDec();
    }

    /**
     * Stop all leftover threads.
     */
    @Override
    public void close() {
        downloaderPool.shutdownNow();
        extractorPool.shutdownNow();
    }

    private void download(String url,
                          Set<String> visited,
                          Map<String, Semaphore> limit,
                          Set<String> downloaded,
                          Map<String, IOException> errors,
                          int depth,
                          DoneDetector det
    ) {
        final Document doc;
        Semaphore sem = null;
        try {
            final String host = URLUtils.getHost(url);
            limit.putIfAbsent(host, new Semaphore(perHost));
            sem = limit.get(host);

            sem.acquireUninterruptibly();

            doc = downloader.download(url);

            downloaded.add(url);
        } catch (IOException e) {
            errors.put(url, e);
            det.refDec();
            return;
        } catch (Throwable e) {
            det.refDec();
            throw e;
        } finally {
            if (sem != null) {
                sem.release();
            }
        }

        if (depth == 1) {
            det.refDec();
            return;
        }

        det.refInc();
        extractorPool.submit(() -> extract(url, visited, limit, downloaded, errors, depth, det, doc));

        det.refDec();
    }

    public static void main(String... args) {
        if (args == null) {
            System.err.println("Args cannot be null");
            return;
        }

        if (Arrays.stream(args).anyMatch(x -> x == null)) {
            System.err.println("None of args can be null");
            return;
        }

        if (args.length > 5 || args.length < 1) {
            System.err.println("Usage: WebCrawler url [depth [downloads [extractors [perHost]]]]");
            return;
        }

        String url;
        int depth;
        int downloaders;
        int extractors;
        int perHost;

        try {
            url = args[0];
            depth = args.length > 1 ? Integer.parseInt(args[1]) : 2;
            downloaders = args.length > 2 ? Integer.parseInt(args[2]) : 8;
            extractors = args.length > 3 ? Integer.parseInt(args[3]) : 8;
            perHost = args.length > 4 ? Integer.parseInt(args[4]) : 4;

        } catch (NumberFormatException e) {
            System.err.println("Invalid number entered");
            return;
        }

        Downloader downloader;

        try {
            downloader = new CachingDownloader();
        } catch (IOException e) {
            System.err.println("Could not construct CachingDownloader");
            e.printStackTrace();
            return;
        }

        WebCrawler crawler = new WebCrawler(downloader, downloaders, extractors, perHost);

        crawler.download(url, depth);

        crawler.close();
    }

    private int constrain(int value, int min, int max) {
        if (value > max) {
            value = max;
        } else if (value < min) {
            value = min;
        }
        return value;
    }

    private static class DoneDetector {
        private final AtomicLong refs = new AtomicLong(1);


        void refInc() {
            refs.incrementAndGet();
        }

        void refDec() {
            if (refs.decrementAndGet() == 0) {
                synchronized (refs) {
                    refs.notifyAll();
                }
            }
        }

        void waitCompleted() throws InterruptedException {
            synchronized (refs) {
                while (refs.get() != 0) {
                    refs.wait();
                }
            }
        }
    }

    ;
}

