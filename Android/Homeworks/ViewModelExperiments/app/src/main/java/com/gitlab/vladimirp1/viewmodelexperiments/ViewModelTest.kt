package com.gitlab.vladimirp1.viewmodelexperiments

import android.support.v7.app.AppCompatActivity
import android.os.Bundle
import com.gitlab.vladimirp1.viewmodelexperiments.ui.viewmodeltest.ViewModelTestFragment

class ViewModelTest : AppCompatActivity() {

    override fun onCreate(savedInstanceState: Bundle?) {
        super.onCreate(savedInstanceState)
        setContentView(R.layout.view_model_test_activity)
        if (savedInstanceState == null) {
            supportFragmentManager.beginTransaction()
                    .replace(R.id.container, ViewModelTestFragment.newInstance())
                    .commitNow()
        }
    }

}
