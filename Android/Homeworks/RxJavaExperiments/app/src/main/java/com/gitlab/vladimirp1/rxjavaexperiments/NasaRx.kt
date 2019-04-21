package com.gitlab.vladimirp1.rxjavaexperiments

import android.arch.persistence.room.ColumnInfo
import android.arch.persistence.room.Entity
import android.arch.persistence.room.PrimaryKey
import android.os.Parcelable
import com.fasterxml.jackson.annotation.JsonIgnoreProperties
import com.fasterxml.jackson.annotation.JsonInclude
import com.fasterxml.jackson.annotation.JsonProperty
import io.reactivex.Maybe
import kotlinx.android.parcel.Parcelize
import retrofit2.http.GET
import retrofit2.http.Path
import retrofit2.http.Query

@Parcelize
data class PhotoList(val data : List<Photo>) : Parcelable

@Entity
@Parcelize
data class Photo(
    @PrimaryKey val nasaId: String,
    @ColumnInfo val name: String,
    @ColumnInfo val description: String,
    @ColumnInfo val previewUrl: String
) : Parcelable {

    companion object {
        fun fromNasa(data: Nasa): PhotoList {
            val result = mutableListOf<Photo>()
            for (item in data.collection!!.items!!) {
                val data0 = item.data?.get(0)
                val links = item.links
                if (data0 != null && links != null) {
                    val nasaId = data0.nasaId
                    val name = data0.title
                    val description = data0.description
                    for (link in links) {
                        if (link.rel == "preview" && link.render == "image") {
                            result.add(Photo(nasaId!!, name!!, description!!, link.href!!))
                        }
                    }
                }
            }
            return PhotoList(result)
        }
    }

    override fun toString(): String {
        return "$nasaId $name $previewUrl"
    }
}

@JsonInclude(JsonInclude.Include.NON_NULL)
@JsonIgnoreProperties(ignoreUnknown = true)
class Metadata {
    @JsonProperty("total_hits")
    var totalHits: Int? = null
}


@JsonInclude(JsonInclude.Include.NON_NULL)
@JsonIgnoreProperties(ignoreUnknown = true)
class Datum {

    @JsonProperty("nasa_id")
    var nasaId: String? = null

    @JsonProperty("date_created")
    var dateCreated: String? = null

    @JsonProperty("title")
    var title: String? = null

    @JsonProperty("media_type")
    var mediaType: String? = null

    @JsonProperty("description")
    var description: String? = null

    @JsonProperty("center")
    var center: String? = null

    @JsonProperty("photographer")
    var photographer: String? = null

    @JsonProperty("keywords")
    var keywords: List<String>? = null

    @JsonProperty("location")
    var location: String? = null

    @JsonProperty("description_508")
    var description508: String? = null

}


@JsonInclude(JsonInclude.Include.NON_NULL)
@JsonIgnoreProperties(ignoreUnknown = true)
class Link {
    @JsonProperty("rel")
    var rel: String? = null

    @JsonProperty("render")
    var render: String? = null

    @JsonProperty("href")
    var href: String? = null
}


@JsonInclude(JsonInclude.Include.NON_NULL)
@JsonIgnoreProperties(ignoreUnknown = true)
class Item {

    @JsonProperty("links")
    var links: List<Link>? = null
    @JsonProperty("data")
    var data: List<Datum>? = null
    @JsonProperty("href")
    var href: String? = null

}

@JsonInclude(JsonInclude.Include.NON_NULL)
@JsonIgnoreProperties(ignoreUnknown = true)
class Collection {

    @JsonProperty("version")
    var version: String? = null

    @JsonProperty("items")
    var items: List<Item>? = null

    @JsonProperty("metadata")
    var metadata: Metadata? = null

    @JsonProperty("href")
    var href: String? = null

}

@JsonInclude(JsonInclude.Include.NON_NULL)
@JsonIgnoreProperties(ignoreUnknown = true)
class Nasa {
    @JsonProperty("collection")
    var collection: Collection? = null
}

@JsonInclude(JsonInclude.Include.NON_NULL)
@JsonIgnoreProperties(ignoreUnknown = true)
class ItemAsset {
    @JsonProperty("href")
    var href: String? = null

}

@JsonInclude(JsonInclude.Include.NON_NULL)
@JsonIgnoreProperties(ignoreUnknown = true)
class CollectionAsset {
    @JsonProperty("items")
    var items: List<ItemAsset>? = null

    @JsonProperty("href")
    var href: String? = null

}

@JsonInclude(JsonInclude.Include.NON_NULL)
@JsonIgnoreProperties(ignoreUnknown = true)
class NasaAsset {
    @JsonProperty("collection")
    var collection: CollectionAsset? = null
}


interface NasaRxApi {
    @GET("search")
    fun search(@Query("q") query: String, @Query("media_type") media_type: String): Maybe<Nasa>

    @GET("asset/{id}")
    fun getAsset(
        @Path("id") nasaId: String
    ): Maybe<NasaAsset>
}