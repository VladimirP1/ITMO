package com.gitlab.vladimirp1.photofeed.comics

import android.os.Parcelable
import com.fasterxml.jackson.annotation.JsonIgnore
import com.fasterxml.jackson.annotation.JsonIgnoreProperties
import kotlinx.android.parcel.Parcelize
import java.util.ArrayList

@JsonIgnoreProperties(ignoreUnknown = true)
class Comics {
    val comics: MutableList<Comic> = ArrayList()

    @JsonIgnore
    var downloadError : Throwable? = null

    @Parcelize
    @JsonIgnoreProperties(ignoreUnknown = true)
    data class Comic(val safe_title: String, val transcript: String, val alt: String, val img: String) : Parcelable {
        override fun toString(): String = safe_title
        constructor() : this("", "", "", "")
    }
}
