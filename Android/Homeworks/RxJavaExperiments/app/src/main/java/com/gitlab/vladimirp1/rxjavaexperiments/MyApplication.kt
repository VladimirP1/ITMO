package com.gitlab.vladimirp1.rxjavaexperiments

import android.app.Application
import android.arch.persistence.room.Room
import okhttp3.Cache
import okhttp3.OkHttpClient
import org.kodein.di.Kodein
import org.kodein.di.KodeinAware
import org.kodein.di.android.androidModule
import org.kodein.di.generic.*
import retrofit2.Retrofit
import retrofit2.adapter.rxjava2.RxJava2CallAdapterFactory
import retrofit2.converter.jackson.JacksonConverterFactory


class MyApplication : Application(), KodeinAware {
    private val cacheSize = (10 * 1024 * 1024).toLong()

    override val kodein = Kodein.lazy {
        import(androidModule(this@MyApplication))
        bind<Cache>() with singleton { Cache(cacheDir, cacheSize) }
        bind<OkHttpClient>() with singleton { OkHttpClient.Builder().cache(instance()).build() }
        bind<PhotoDatabase>() with singleton {
            Room.databaseBuilder(
                this@MyApplication,
                PhotoDatabase::class.java,
                "likes"
            ).build()
        }
        bind<Retrofit>() with singleton {
            Retrofit.Builder().baseUrl("https://images-api.nasa.gov/")
                .addConverterFactory(JacksonConverterFactory.create())
                .addCallAdapterFactory(RxJava2CallAdapterFactory.create())
                .client(instance())
                .build()
        }
        bind<NasaRxApi>() with singleton { instance<Retrofit>().create(NasaRxApi::class.java) }
        bind<PhotoLiker>() with singleton {
            PhotoLiker(
                kodein
            )
        }
        bind<AssetUrlFetcher>() with singleton {
            AssetUrlFetcher(
                kodein
            )
        }
    }
}

