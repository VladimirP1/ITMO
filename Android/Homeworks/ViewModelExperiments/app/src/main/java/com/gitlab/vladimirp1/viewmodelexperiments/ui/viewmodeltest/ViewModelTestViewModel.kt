package com.gitlab.vladimirp1.viewmodelexperiments.ui.viewmodeltest

import android.arch.lifecycle.LiveData
import android.arch.lifecycle.MutableLiveData
import android.arch.lifecycle.ViewModel
import android.os.AsyncTask

class ViewModelTestViewModel : ViewModel() {
    var initialized : Boolean = false
    var data : MutableLiveData<String>? = MutableLiveData()

    fun maybeInit() {
        if(!initialized){
            initialized = true
            data!!.value = "abcd"
            test()
        }
    }

    fun getData() : LiveData<String> {
        //test()
        return data!!
    }

    fun test() {
        object : AsyncTask<String, Void, String>() {
            override fun doInBackground(vararg params: String?): String {
                Thread.sleep(1000)
                return "AB"
            }

            override fun onPostExecute(result: String?) {
                data!!.value = result!!
                super.onPostExecute(result)
            }

        }.execute()
    }

}
