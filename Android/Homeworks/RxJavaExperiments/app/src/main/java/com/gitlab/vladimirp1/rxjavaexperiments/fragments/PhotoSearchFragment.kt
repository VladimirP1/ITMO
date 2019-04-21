package com.gitlab.vladimirp1.rxjavaexperiments.fragments


import android.os.Bundle
import android.support.v4.app.Fragment
import android.view.LayoutInflater
import android.view.View
import android.view.ViewGroup

import com.gitlab.vladimirp1.rxjavaexperiments.R
import com.gitlab.vladimirp1.rxjavaexperiments.NetworkProviderDesc
import kotlinx.android.synthetic.main.fragment_photo_search.*


class PhotoSearchFragment : Fragment() {
    private var query : String? = null
    private var resultsView : View? = null

    private val listener = {
        val text = searchQuery.text.toString()
        query = text
        val myFrag = PhotoRecyclerFragment.newInstance(NetworkProviderDesc(text))
        activity?.supportFragmentManager?.beginTransaction()?.replace(R.id.frame, myFrag)?.commit()
    }

    override fun onCreateView(
        inflater: LayoutInflater, container: ViewGroup?,
        savedInstanceState: Bundle?
    ): View? {
        if (resultsView == null) {
            resultsView = inflater.inflate(R.layout.fragment_photo_search, container, false)
        }

        return resultsView
    }

    override fun onStart() {
        super.onStart()

        searchButton.setOnClickListener{listener()}
    }

}
