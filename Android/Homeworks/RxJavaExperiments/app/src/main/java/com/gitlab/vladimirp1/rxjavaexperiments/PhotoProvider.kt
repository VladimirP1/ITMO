package com.gitlab.vladimirp1.rxjavaexperiments

import android.os.Parcelable
import io.reactivex.Completable
import io.reactivex.Maybe
import io.reactivex.Observable
import io.reactivex.android.schedulers.AndroidSchedulers
import io.reactivex.schedulers.Schedulers
import kotlinx.android.parcel.Parcelize
import org.kodein.di.Kodein
import org.kodein.di.KodeinAware
import org.kodein.di.generic.instance

class ApiQueryPhotosProvider(override val kodein : Kodein, val query : String = "") : PhotoProvider, KodeinAware {
    val nasaApi : NasaRxApi by instance()

    override fun getObservable(): Maybe<PhotoList> {
        return nasaApi.search(query, "image").subscribeOn(Schedulers.io())!!.observeOn(AndroidSchedulers.mainThread())!!.map { x -> Photo.fromNasa(x) }
    }
}

class LikedPhotosProvider(override val kodein : Kodein) : PhotoProvider, KodeinAware {
    val database : PhotoDatabase by instance()

    override fun getObservable(): Maybe<PhotoList> {
        return database.photoDao().getAll().map { x -> PhotoList(x) }
    }
}

class AssetUrlFetcher(override val kodein: Kodein) : KodeinAware {
    val nasaApi : NasaRxApi by instance()

    fun getHiResUrl(id : String) : Maybe<String> {
        return nasaApi.getAsset(id).map {asset -> asset.collection!!.items!![0].href}
    }
}

class PhotoLiker(override val kodein: Kodein) : KodeinAware {
    val database : PhotoDatabase by instance()

    fun likePhoto(p : Photo, like: Boolean = true) : Completable {
        if(like) {
            return Completable.fromCallable { database.photoDao().insert(arrayOf(p)) }
        } else {
            return unlikePhoto(p.nasaId)
        }
    }

    fun unlikePhoto(id : String) : Completable {
        return Completable.fromCallable{ database.photoDao().delete(id) }
    }

    fun isLiked(id:String) : Maybe<Boolean> {
        return database.photoDao().getByIds(arrayOf(id)).map{ it.isNotEmpty() }
    }
}

interface PhotoProvider {
    fun getObservable() : Maybe<PhotoList>
}

@Parcelize
class DatabaseProviderDesc : ProviderDescriptor, Parcelable {
    override fun build(kodein : Kodein) : PhotoProvider {
        return LikedPhotosProvider(kodein)
    }

}

@Parcelize
class NetworkProviderDesc(val query : String = "") : ProviderDescriptor, Parcelable {
    override fun build(kodein : Kodein) : PhotoProvider {
        return ApiQueryPhotosProvider(kodein, query)
    }

}

interface ProviderDescriptor : Parcelable {
    fun build(kodein : Kodein) : PhotoProvider
}

fun <T> Observable<T>.moveToIoThread() : Observable<T> {
    return subscribeOn(Schedulers.io())!!.observeOn(AndroidSchedulers.mainThread())!!
}

fun <T> Maybe<T>.moveToIoThread() : Maybe<T> {
    return subscribeOn(Schedulers.io())!!.observeOn(AndroidSchedulers.mainThread())!!
}

fun Completable.moveToIoThread() : Completable {
    return subscribeOn(Schedulers.io())!!.observeOn(AndroidSchedulers.mainThread())!!
}