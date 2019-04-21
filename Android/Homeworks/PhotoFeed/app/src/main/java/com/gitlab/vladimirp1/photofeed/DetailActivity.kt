package com.gitlab.vladimirp1.photofeed

import android.content.res.Configuration
import android.os.Bundle
import android.support.v7.app.AppCompatActivity
import com.gitlab.vladimirp1.photofeed.comics.Comics


class DetailActivity : AppCompatActivity() {

    override fun onCreate(savedInstanceState: Bundle?) {
        super.onCreate(savedInstanceState)
        if (resources.configuration.orientation == Configuration.ORIENTATION_LANDSCAPE) {
            finish()
            return
        }
        setContentView(R.layout.activity_detail)

        val comic = intent.getParcelableExtra<Comics.Comic>(COMIC_KEY)

        supportFragmentManager.beginTransaction().add(R.id.detailFragmentPlaceHolder, DetailFragment.newInstance(comic)).commit()
    }
    companion object {
        @JvmStatic
        val COMIC_KEY = "COMIC"
    }

}
