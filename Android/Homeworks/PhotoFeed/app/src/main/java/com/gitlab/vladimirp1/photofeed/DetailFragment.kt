package com.gitlab.vladimirp1.photofeed

import android.content.ComponentName
import android.content.Context
import android.content.Intent
import android.content.ServiceConnection
import android.os.Bundle
import android.os.IBinder
import android.support.v4.app.Fragment
import android.view.LayoutInflater
import android.view.View
import android.view.ViewGroup
import android.widget.TextView
import com.gitlab.vladimirp1.photofeed.comics.Comics
import kotlinx.android.synthetic.main.fragment_detail.*

private const val COMIC_PARAM = "DetailFragment.ComicMetadata"

class DetailFragment : Fragment(), ImageDownloaderService.MyCallback {
    override fun onImageDownloaded(i: ImageDownloaderService.DownloadParams) {
        if(view != null) {
            if (i.result != null) {
                imageView.setImageBitmap(i.result)
            } else {
                imageView.setImageResource(R.drawable.ic_loading_failed)
            }
        }
    }

    private var connection : ServiceConnection? = null
    private var downloaderService : ImageDownloaderService.ImageDownloaderBinder? = null
    private var comic: Comics.Comic? = null

    override fun onCreate(savedInstanceState: Bundle?) {
        super.onCreate(savedInstanceState)
        arguments?.let {
            comic = it.getParcelable(COMIC_PARAM)
        }
    }

    override fun onCreateView(inflater: LayoutInflater, container: ViewGroup?,
                              savedInstanceState: Bundle?): View? {
        return inflater.inflate(R.layout.fragment_detail, container, false).apply {
            findViewById<TextView>(R.id.description).text = comic?.alt
        }
    }

    override fun onAttach(context: Context) {
        connection = ImageDownloaderConnection()
        context.bindService(Intent(context, ImageDownloaderService::class.java), connection!!, 0)
        super.onAttach(context)
    }

    override fun onDetach() {
        context?.unbindService(connection!!)
        super.onDetach()
    }

    companion object {
        @JvmStatic
        fun newInstance(param1: Comics.Comic) =
                DetailFragment().apply {
                    arguments = Bundle().apply {
                        putParcelable(COMIC_PARAM, param1)
                    }
                }
    }

    inner class ImageDownloaderConnection : ServiceConnection {
        override fun onServiceDisconnected(name: ComponentName?) {
            downloaderService = null
        }

        override fun onServiceConnected(name: ComponentName?, service: IBinder?) {
            downloaderService = service as ImageDownloaderService.ImageDownloaderBinder
            downloaderService?.download(
                    this@DetailFragment.comic?.img!!, this@DetailFragment

            )
        }

    }
}
