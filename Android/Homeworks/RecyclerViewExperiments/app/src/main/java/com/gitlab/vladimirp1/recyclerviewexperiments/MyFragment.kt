package com.gitlab.vladimirp1.recyclerviewexperiments


import android.os.Bundle
import android.support.v4.app.Fragment
import android.support.v7.widget.LinearLayoutManager
import android.support.v7.widget.RecyclerView
import android.view.LayoutInflater
import android.view.View
import android.view.ViewGroup


/**
 * A simple [Fragment] subclass.
 * Use the [MyFragment.newInstance] factory method to
 * create an instance of this fragment.
 *
 */
class MyFragment : Fragment() {

    override fun onCreate(savedInstanceState: Bundle?) {
        super.onCreate(savedInstanceState)

    }

    override fun onCreateView(inflater: LayoutInflater, container: ViewGroup?,
                              savedInstanceState: Bundle?): View? {
        // Inflate the layout for this fragment
        val view = inflater.inflate(R.layout.fragment_my, container, false)
        with(view.findViewById<RecyclerView>(R.id.myRecycler)) {
            adapter = MyAdapter()
            layoutManager = LinearLayoutManager(context)
        }
        return view
    }


    companion object {
        @JvmStatic
        fun newInstance(param1: String, param2: String) = MyFragment()
    }
}
