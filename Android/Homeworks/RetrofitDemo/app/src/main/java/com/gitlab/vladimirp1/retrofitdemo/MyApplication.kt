package com.gitlab.vladimirp1.retrofitdemo

import android.app.Application
import retrofit2.Retrofit
import retrofit2.converter.jackson.JacksonConverterFactory

class MyApplication : Application() {
    public lateinit var retrofit : Retrofit
    public lateinit var nasaApi : NasaImageryApi
    override fun onCreate() {
        super.onCreate()
        retrofit = Retrofit.Builder().baseUrl("https://images-api.nasa.gov").addConverterFactory(JacksonConverterFactory.create()).build()
        nasaApi = retrofit.create(NasaImageryApi::class.java)
    }
}