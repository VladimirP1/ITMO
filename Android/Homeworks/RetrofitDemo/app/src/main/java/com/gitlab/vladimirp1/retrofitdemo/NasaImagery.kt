package com.gitlab.vladimirp1.retrofitdemo

import retrofit2.Call
import retrofit2.http.GET
import retrofit2.http.Query
import com.gitlab.vladimirp1.nasa.Nasa

//KbvqsiWb8clBCgRSoFYzdNR5PyUlxLDlj9WDq5DO
//https://images-api.nasa.gov/search?q=sun

public interface NasaImageryApi {
    @GET("/search")
    public fun search(@Query("q") query : String, @Query("media_type") media_type: String) : Call<Nasa>
    public fun defaultList() : Call<Nasa> {
        return search("", "image")
    }

}