package com.gitlab.vladimirp1.retrofitdemo

import android.support.v7.app.AppCompatActivity
import android.os.Bundle
import android.util.Log
import com.gitlab.vladimirp1.nasa.Nasa
import retrofit2.*
import retrofit2.converter.jackson.JacksonConverterFactory

class MainActivity : AppCompatActivity() {

    override fun onCreate(savedInstanceState: Bundle?) {
        super.onCreate(savedInstanceState)
        setContentView(R.layout.activity_main)

        val retrofit = Retrofit.Builder().baseUrl("https://images-api.nasa.gov").addConverterFactory(JacksonConverterFactory.create()).build()
        val nasaApi = retrofit.create(NasaImageryApi::class.java)

        val resp = nasaApi.search("", "image").enqueue (
            object : Callback<Nasa> {
                override fun onFailure(call: Call<Nasa>, t: Throwable) {

                }

                override fun onResponse(call: Call<Nasa>, response: Response<Nasa>) {
                    val body = response.body()!!
                    for(i in body.collection.items) {
                        for(j in i.links) {
                            if(j.render == "image") {
                                Log.d("TAG_S", j.href.toString())
                            }
                        }
                    }

                }

            }
        )
    }
}
