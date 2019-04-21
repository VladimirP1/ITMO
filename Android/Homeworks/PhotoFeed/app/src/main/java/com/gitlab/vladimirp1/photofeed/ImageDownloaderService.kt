package com.gitlab.vladimirp1.photofeed

import android.app.Service
import android.content.Intent
import android.graphics.Bitmap
import android.graphics.BitmapFactory
import android.os.AsyncTask
import android.os.Binder
import android.os.IBinder
import android.util.LruCache
import java.io.*
import java.lang.ref.WeakReference
import java.net.HttpURLConnection
import java.net.URL

class ImageDownloaderService : Service() {
    val maxCacheSize = 5 * 1024 * 1024
    private var myCacheDir: File? = null

    override fun onCreate() {
        if (myCacheDir == null) {
            myCacheDir = cacheDir
        }
        super.onCreate()
    }

    override fun onBind(intent: Intent?): IBinder? {
        return ImageDownloaderBinder(this)
    }

    val bitmapCache = object : LruCache<String, Bitmap>(maxCacheSize) {
        override fun sizeOf(key: String?, value: Bitmap?): Int {
            return value!!.byteCount
        }
    }

    data class DownloadParams(
            val url: String,
            val service: WeakReference<ImageDownloaderService>,
            var result: Bitmap?,
            val callback: WeakReference<MyCallback>,
            val cacheDir: File
    )

    fun queueTask(url: String, callback: MyCallback) {
        val bitmap = bitmapCache.get(url)
        val params = DownloadParams(url, weak(), bitmap, callback.weak(), myCacheDir!!)
        if (bitmap != null) {
            callback.onImageDownloaded(params)
        } else {
            DownloadTask().execute(params)
        }
    }

    class DownloadTask :
            AsyncTask<DownloadParams, Void, DownloadParams>() {
        fun dumpInputStreamToFile(imgFile: File, input: InputStream): InputStream {
            val out = BufferedOutputStream(FileOutputStream(imgFile))
            val buf = ByteArray(1024)
            while (true) {
                var len = input.read(buf)
                if (len <= 0) break;
                out.write(buf, 0, len)
            }
            out.close()
            return BufferedInputStream(FileInputStream(imgFile))
        }

        override fun doInBackground(vararg params: DownloadParams?): DownloadParams? {
            val downloadTimeout = 5000
            var stream: InputStream? = null
            try {
                val imgFile = File(params[0]!!.cacheDir, "_cached_" + params[0]!!.url.hashCode() + "_")
                if (imgFile.exists() && imgFile.isFile()) {
                    stream = BufferedInputStream(FileInputStream(imgFile))
                } else {
                    val url = URL(params[0]!!.url)
                    val conn = url.openConnection() as HttpURLConnection

                    conn.connectTimeout = downloadTimeout
                    conn.readTimeout = downloadTimeout

                    try {
                        stream = BufferedInputStream(conn.inputStream)
                        stream = dumpInputStreamToFile(imgFile, stream)
                    } catch (e: Throwable) {
                        params[0]!!.result = null
                        return params[0]
                    } finally {
                        conn.disconnect()
                    }
                }
                params[0]!!.result = BitmapFactory.decodeStream(stream)
                stream!!.close()
            } catch (e: Throwable) {
                ;
            }
            return params[0]
        }

        override fun onPostExecute(result: DownloadParams?) {
            val service = result?.service?.get()
            if (service != null) {
                if (result.result != null) {
                    service.bitmapCache.put(result.url, result.result)
                }
                result.callback.get()?.onImageDownloaded(result)
            }
            super.onPostExecute(result)
        }

    }

    class ImageDownloaderBinder(private val service: Service) : Binder() {
        fun download(url: String, cb: MyCallback) {
            (service as ImageDownloaderService).queueTask(url, cb)
        }
    }

    interface MyCallback {
        fun onImageDownloaded(params: DownloadParams)
    }
}
