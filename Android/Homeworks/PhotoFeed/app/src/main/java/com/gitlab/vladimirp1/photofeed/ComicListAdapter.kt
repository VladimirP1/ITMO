package com.gitlab.vladimirp1.photofeed

import android.support.v7.widget.RecyclerView
import android.view.LayoutInflater
import android.view.View
import android.view.ViewGroup
import android.widget.TextView


import com.gitlab.vladimirp1.photofeed.ComicListFragment.OnListFragmentInteractionListener
import com.gitlab.vladimirp1.photofeed.comics.Comics

import kotlinx.android.synthetic.main.fragment_comic_for_recycler.view.*


class ComicListAdapter(
        private val mValues: List<Comics.Comic>,
        private val mListener: OnListFragmentInteractionListener?)
    : RecyclerView.Adapter<ComicListAdapter.ViewHolder>() {

    private val mOnClickListener: View.OnClickListener

    init {
        mOnClickListener = View.OnClickListener { v ->
            val item = v.tag as Comics.Comic
            mListener?.onListFragmentInteraction(item)
        }
    }

    override fun onCreateViewHolder(parent: ViewGroup, viewType: Int): ViewHolder {
        val view = LayoutInflater.from(parent.context)
                .inflate(R.layout.fragment_comic_for_recycler, parent, false)
        return ViewHolder(view)
    }

    override fun onBindViewHolder(holder: ViewHolder, position: Int) {
        val item = mValues[position]
        holder.mTitle.text = item.safe_title

        with(holder.mView) {
            tag = item
            setOnClickListener(mOnClickListener)
        }
    }

    override fun getItemCount(): Int = mValues.size

    inner class ViewHolder(val mView: View) : RecyclerView.ViewHolder(mView) {
        val mTitle: TextView = mView.item_title

        override fun toString(): String {
            return super.toString() + " '" + mTitle + "'"
        }
    }
}
