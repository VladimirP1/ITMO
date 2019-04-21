package com.gitlab.vladimirp1.retrofitexperiments.helpers

import android.support.v7.widget.RecyclerView
import android.view.LayoutInflater
import android.view.View
import android.view.ViewGroup
import android.widget.Adapter
import android.widget.FrameLayout
import android.widget.TextView
import com.gitlab.vladimirp1.nasa.Nasa
import com.gitlab.vladimirp1.retrofitexperiments.R
import retrofit2.Response

class FeedRecylerAdapter(val response: Response<Nasa>?) : RecyclerView.Adapter<FeedRecylerAdapter.FeedRecyclerViewHolder>(){

    override fun onCreateViewHolder(parent: ViewGroup, p1: Int): FeedRecyclerViewHolder {
        val view = LayoutInflater.from(parent.context)
            .inflate(R.layout.feed_recycler_element, parent, false) as FrameLayout
        return FeedRecyclerViewHolder(view)
    }

    override fun onBindViewHolder(p0: FeedRecyclerViewHolder, p1: Int) {
        p0.textView.text = response?.body()?.collection?.items?.get(p1)?.data?.get(0)?.title ?: "Error"
    }

    override fun getItemCount(): Int {
        return response?.body()?.collection?.items?.size ?: 0
    }

    class FeedRecyclerViewHolder : RecyclerView.ViewHolder {
        lateinit var textView: TextView
        constructor(frame : View) : super(frame) {
            textView = frame.findViewById(R.id.feedRecyclerElementText)
        }

    }
}