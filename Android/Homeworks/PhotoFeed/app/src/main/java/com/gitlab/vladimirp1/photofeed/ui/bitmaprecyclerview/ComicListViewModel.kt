package com.gitlab.vladimirp1.photofeed.ui.bitmaprecyclerview

import android.arch.lifecycle.LiveData
import android.arch.lifecycle.MutableLiveData
import android.arch.lifecycle.ViewModel
import android.os.AsyncTask
import java.net.HttpURLConnection
import java.net.URL
import com.fasterxml.jackson.databind.ObjectMapper
import com.gitlab.vladimirp1.photofeed.comics.Comics
import java.lang.ref.WeakReference


class ComicListViewModel : ViewModel() {
    private val METADATA_URL = "http://46.229.215.195/xkcd_all.json"

    private var initialized: Boolean = false
    private var data: MutableLiveData<Comics> = MutableLiveData()

    fun maybeInitialize() : Comics? {
        if (!initialized) {
            initialized = true
            loadJson()
        }
        return data.value
    }

    fun getData(): LiveData<Comics> {
        return data
    }

    private fun loadJson() {
        MyTask(WeakReference(this)).execute(METADATA_URL)
    }

    class MyTask(val parent: WeakReference<ComicListViewModel>) : AsyncTask<String, Void, Comics?>() {
        override fun doInBackground(vararg params: String?): Comics? {
            val conn = URL(params[0]).openConnection() as HttpURLConnection
            val mapper = ObjectMapper()

            try {
                conn.connect()
                return mapper.readValue(conn.inputStream, Comics::class.java)
            } catch(e : Throwable) {
                return Comics().apply { downloadError = e }
            } finally {
                conn.disconnect()
            }
        }

        override fun onPostExecute(result: Comics?) {
            val model = parent.get()
            model?.data?.value = result
            super.onPostExecute(result)
        }
    }
}
