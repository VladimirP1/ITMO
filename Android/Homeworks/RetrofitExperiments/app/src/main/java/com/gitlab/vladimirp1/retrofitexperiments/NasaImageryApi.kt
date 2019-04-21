package com.gitlab.vladimirp1.retrofitexperiments

import com.gitlab.vladimirp1.nasa.Nasa
import retrofit2.Call
import retrofit2.http.GET
import retrofit2.http.Query

public interface NasaImageryApi {
    @GET("/search")
    public fun search(@Query("q") query : String, @Query("media_type") media_type: String) : Call<Nasa>

}