package com.gitlab.vladimirp1.viewmodelexperiments.ui.viewmodeltest

import android.arch.lifecycle.Observer
import android.arch.lifecycle.ViewModelProviders
import android.os.Bundle
import android.support.v4.app.Fragment
import android.view.LayoutInflater
import android.view.View
import android.view.ViewGroup
import com.gitlab.vladimirp1.viewmodelexperiments.R
import kotlinx.android.synthetic.main.view_model_test_fragment.*

class ViewModelTestFragment : Fragment() {

    companion object {
        fun newInstance() = ViewModelTestFragment()
    }

    private lateinit var viewModel: ViewModelTestViewModel

    override fun onCreateView(inflater: LayoutInflater, container: ViewGroup?,
                              savedInstanceState: Bundle?): View {
        return inflater.inflate(R.layout.view_model_test_fragment, container, false)
    }

    override fun onActivityCreated(savedInstanceState: Bundle?) {
        super.onActivityCreated(savedInstanceState)
        viewModel = ViewModelProviders.of(this).get(ViewModelTestViewModel::class.java)
        viewModel.maybeInit()
        viewModel.getData().observe(this, Observer { s -> message.text = s })
        // TODO: Use the ViewModel
    }

}
