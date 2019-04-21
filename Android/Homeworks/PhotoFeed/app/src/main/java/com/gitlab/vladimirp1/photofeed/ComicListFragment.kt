package com.gitlab.vladimirp1.photofeed

import android.arch.lifecycle.Observer
import android.arch.lifecycle.ViewModelProviders
import android.content.Context
import android.os.Bundle
import android.support.v4.app.Fragment
import android.support.v7.widget.LinearLayoutManager
import android.support.v7.widget.RecyclerView
import android.util.Log
import android.view.LayoutInflater
import android.view.View
import android.view.ViewGroup
import android.widget.ProgressBar
import android.widget.Toast
import com.gitlab.vladimirp1.photofeed.comics.Comics

import com.gitlab.vladimirp1.photofeed.ui.bitmaprecyclerview.ComicListViewModel

class ComicListFragment : Fragment() {
    private var listener: OnListFragmentInteractionListener? = null

    private fun onDataLoaded(it : Comics?, targetView : View?) {
        val recycler = targetView?.findViewById<RecyclerView>(R.id.list)
        val progressBar = targetView?.findViewById<ProgressBar>(R.id.progressBar)

        progressBar?.visibility = View.INVISIBLE

        if(it?.downloadError != null) {
            val throwable = it.downloadError
            Toast.makeText(context, context?.resources?.getString(R.string.error_downloading), Toast.LENGTH_LONG).show()
            Log.e(this::class.qualifiedName, throwable?.stackTrace.toString())
        }

        if(recycler != null) {
            with(recycler) {
                if(adapter == null) {
                    adapter = ComicListAdapter(it!!.comics, listener)
                    layoutManager = LinearLayoutManager(context)
                }
            }
        }
    }

    override fun onCreateView(inflater: LayoutInflater, container: ViewGroup?,
                              savedInstanceState: Bundle?): View? {
        val contentsView = inflater.inflate(R.layout.fragment_comic_list_view, container, false)

        val model = ViewModelProviders.of(this).get(ComicListViewModel::class.java)
        val data = model.maybeInitialize()

        if(data != null) {
            onDataLoaded(data, contentsView)
        }

        model.getData().observe(this, Observer {
            onDataLoaded(it, view)
        })

        return contentsView
    }


    override fun onAttach(context: Context) {
        super.onAttach(context)

        if (context is OnListFragmentInteractionListener) {
            listener = context
        } else {
            throw RuntimeException(context.toString() + " must implement OnListFragmentInteractionListener")
        }
    }

    override fun onDetach() {
        super.onDetach()
        listener = null
    }

    interface OnListFragmentInteractionListener {
        fun onListFragmentInteraction(item: Comics.Comic?)
    }
}
