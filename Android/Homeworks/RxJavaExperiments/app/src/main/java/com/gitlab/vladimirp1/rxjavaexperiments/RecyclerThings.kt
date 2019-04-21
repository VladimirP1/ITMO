package com.gitlab.vladimirp1.rxjavaexperiments

import android.databinding.DataBindingUtil
import android.support.v7.widget.RecyclerView
import android.view.LayoutInflater
import android.view.View
import android.view.ViewGroup
import android.widget.ImageView
import com.gitlab.vladimirp1.rxjavaexperiments.databinding.RecyclerElementBinding
import com.squareup.picasso.Picasso
import java.lang.ref.WeakReference

class MyClickListener(onClick : (p : Photo) -> Unit) {
    private val onClickRef : WeakReference<(p : Photo) -> Unit> = WeakReference(onClick)
    fun run(p : Photo) {
        onClickRef.get()?.invoke(p)
    }
}

class MyAdapter(private val data: List<Photo>, private val onClick : (p : Photo) -> Unit) : RecyclerView.Adapter<MyAdapter.MyViewHolder>() {

    override fun onCreateViewHolder(p0: ViewGroup, p1: Int): MyViewHolder {
        val view = LayoutInflater.from(p0.context).inflate(R.layout.recycler_element, p0, false)
        //val dataBinding: DatabindTestBinding = DataBindingUtil.inflate(LayoutInflater.from(p0.context), R.layout.recycler_element, p0, false)

        return MyViewHolder(view)
    }

    override fun getItemCount(): Int {
        return data.size
    }

    override fun onBindViewHolder(vh: MyViewHolder, index: Int) {
        vh.dataBinding.photo = data[index]
        vh.dataBinding.onclicked = MyClickListener (onClick)
        Picasso.get().load(data[index].previewUrl).placeholder(R.drawable.ic_emblem_synchronizing_symbolic)
            .error(R.drawable.ic_emblem_fail).into(vh.image)
    }

    override fun onViewRecycled(vh: MyViewHolder) {
        Picasso.get().cancelRequest(vh.image)
        super.onViewRecycled(vh)
    }

    class MyViewHolder(val view: View) : RecyclerView.ViewHolder(view) {
        var dataBinding : RecyclerElementBinding
        var image : ImageView

        init {
            dataBinding = DataBindingUtil.bind(view)!!
            image = view.findViewById(R.id.previewImg)
        }
    }
}