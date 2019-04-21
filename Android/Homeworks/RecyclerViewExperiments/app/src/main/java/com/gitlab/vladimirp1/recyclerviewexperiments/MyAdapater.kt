package com.gitlab.vladimirp1.recyclerviewexperiments

import android.support.v7.widget.RecyclerView
import android.view.ViewGroup
import android.widget.TextView

class MyViewHolder(val myTextView : TextView) : RecyclerView.ViewHolder(myTextView) {
}

class MyAdapter : RecyclerView.Adapter<MyViewHolder>() {
    override fun onCreateViewHolder(p0: ViewGroup, p1: Int): MyViewHolder {

        return MyViewHolder(TextView(p0.context).apply {
            text = "abcd " + p1.toString()
            layoutParams = ViewGroup.LayoutParams(ViewGroup.LayoutParams.WRAP_CONTENT, ViewGroup.LayoutParams.WRAP_CONTENT)
         })
    }

    override fun getItemCount(): Int {
        return 100;
    }

    override fun onBindViewHolder(p0: MyViewHolder, p1: Int) {
        p0.myTextView.text = "abcd " + p1.toString()
    }

}