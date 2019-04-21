package com.gitlab.vladimirp1.retrofitexperiments

import android.app.Application
import android.arch.lifecycle.LiveData
import android.arch.lifecycle.MutableLiveData
import android.arch.persistence.room.Room
import com.gitlab.vladimirp1.nasa.Nasa
import com.gitlab.vladimirp1.retrofitexperiments.helpers.LikeDatabase
import retrofit2.Call
import retrofit2.Callback
import retrofit2.Response
import retrofit2.Retrofit
import retrofit2.converter.jackson.JacksonConverterFactory

class  MyApplication : Application() {
    lateinit var retrofit : retrofit2.Retrofit
    lateinit var nasaApi : NasaImageryApi
    lateinit var database : LikeDatabase
    var liveData : MutableLiveData<Response<Nasa>> = MutableLiveData()

    fun reloadFeed() {
        nasaApi.search("moon", "image").enqueue (
            object: Callback<Nasa> {
                override fun onFailure(call: Call<Nasa>, t: Throwable) {
                    TODO("not implemented") //To change body of created functions use File | Settings | File Templates.
                }

                override fun onResponse(call: Call<Nasa>, response: Response<Nasa>) {
                    liveData.postValue(response)
                }
            }
        )
    }

    override fun onCreate() {
        retrofit = Retrofit.Builder().baseUrl("https://images-api.nasa.gov")
            .addConverterFactory(JacksonConverterFactory.create()).build()
        nasaApi = retrofit.create(NasaImageryApi::class.java)

        database = Room.databaseBuilder(this, LikeDatabase::class.java, "likes").build()


        reloadFeed()

        super.onCreate()
    }
}