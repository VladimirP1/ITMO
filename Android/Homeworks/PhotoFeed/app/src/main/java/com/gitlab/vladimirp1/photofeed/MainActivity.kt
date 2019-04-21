package com.gitlab.vladimirp1.photofeed

import android.content.Intent
import android.support.v7.app.AppCompatActivity
import android.os.Bundle
import android.support.v4.app.FragmentManager
import com.gitlab.vladimirp1.photofeed.comics.Comics
import kotlinx.android.synthetic.main.activity_main.*

class MainActivity : AppCompatActivity(), ComicListFragment.OnListFragmentInteractionListener {
    private val DETAIL_BACKSTACK_TAG = "MainActivity.detailFragment"
    private val KEY_LASTCOMIC = "MainActivity.lastComic"
    private var lastComic: Comics.Comic? = null

    override fun onListFragmentInteraction(item: Comics.Comic?) {
        lastComic = item

        supportFragmentManager.popBackStack(DETAIL_BACKSTACK_TAG, FragmentManager.POP_BACK_STACK_INCLUSIVE)
        if(detailFragmentPlaceHolder == null) {
            val intent = Intent(this, DetailActivity::class.java)
            intent.putExtra(DetailActivity.COMIC_KEY, item)
            startActivity(intent)
        } else {
            supportFragmentManager.beginTransaction()
                    .replace(R.id.detailFragmentPlaceHolder, DetailFragment.newInstance(item!!))
                    .addToBackStack(DETAIL_BACKSTACK_TAG)
                    .commit()
        }
    }

    private fun handleConfigurationChange() {
        supportFragmentManager.popBackStack(DETAIL_BACKSTACK_TAG, FragmentManager.POP_BACK_STACK_INCLUSIVE)

        if(lastComic != null && detailFragmentPlaceHolder != null) {
            onListFragmentInteraction(lastComic)
        }
    }

    override fun onCreate(savedInstanceState: Bundle?) {
        super.onCreate(savedInstanceState)

        setContentView(R.layout.activity_main)

        lastComic = savedInstanceState?.getParcelable(KEY_LASTCOMIC)

        handleConfigurationChange()

        startService(Intent(this, ImageDownloaderService::class.java))
    }

    override fun onSaveInstanceState(outState: Bundle?) {
        outState?.putParcelable(KEY_LASTCOMIC, lastComic)
        super.onSaveInstanceState(outState)

    }
}
