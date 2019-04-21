package com.gitlab.vladimirp1.retrofitexperiments.fragments


import android.app.Application
import android.arch.lifecycle.Observer
import android.os.Bundle
import android.support.v4.app.Fragment
import android.support.v7.widget.LinearLayoutManager
import android.support.v7.widget.RecyclerView
import android.view.LayoutInflater
import android.view.View
import android.view.ViewGroup
import com.gitlab.vladimirp1.retrofitexperiments.MyApplication

import com.gitlab.vladimirp1.retrofitexperiments.R
import com.gitlab.vladimirp1.retrofitexperiments.helpers.FeedRecylerAdapter
import kotlinx.android.synthetic.main.fragment_feed.*

// TODO: Rename parameter arguments, choose names that match
// the fragment initialization parameters, e.g. ARG_ITEM_NUMBER
private const val ARG_PARAM1 = "param1"
private const val ARG_PARAM2 = "param2"

/**
 * A simple [Fragment] subclass.
 * Use the [FeedFragment.newInstance] factory method to
 * create an instance of this fragment.
 *
 */
class FeedFragment : Fragment() {
    // TODO: Rename and change types of parameters
    private var param1: String? = null
    private var param2: String? = null
    private lateinit var application : MyApplication

    override fun onCreate(savedInstanceState: Bundle?) {
        super.onCreate(savedInstanceState)
        arguments?.let {
            param1 = it.getString(ARG_PARAM1)
            param2 = it.getString(ARG_PARAM2)
        }
    }

    override fun onCreateView(
        inflater: LayoutInflater, container: ViewGroup?,
        savedInstanceState: Bundle?
    ): View? {
        application = context?.applicationContext as MyApplication

        val view = inflater.inflate(R.layout.fragment_feed, container, false)
        with(view.findViewById<RecyclerView>(R.id.feedRecycler)) {
            layoutManager = LinearLayoutManager(this@FeedFragment.context)
            adapter = FeedRecylerAdapter(application.liveData.value)

        }
        application.liveData.observe(this@FeedFragment, Observer {
            feedRecycler.layoutManager = LinearLayoutManager(context)
            feedRecycler.adapter = FeedRecylerAdapter(it!!)
        })
        return view
    }


    companion object {
        /**
         * Use this factory method to create a new instance of
         * this fragment using the provided parameters.
         *
         * @param param1 Parameter 1.
         * @param param2 Parameter 2.
         * @return A new instance of fragment FeedFragment.
         */
        // TODO: Rename and change types and number of parameters
        @JvmStatic
        fun newInstance(param1: String, param2: String) =
            FeedFragment().apply {
                arguments = Bundle().apply {
                    putString(ARG_PARAM1, param1)
                    putString(ARG_PARAM2, param2)
                }
            }
    }
}
