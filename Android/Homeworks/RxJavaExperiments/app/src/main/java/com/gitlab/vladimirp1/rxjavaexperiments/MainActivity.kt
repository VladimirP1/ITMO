package com.gitlab.vladimirp1.rxjavaexperiments

import android.app.FragmentManager.POP_BACK_STACK_INCLUSIVE
import android.os.Bundle
import android.support.v7.app.AppCompatActivity
import com.gitlab.vladimirp1.rxjavaexperiments.fragments.ISelectedCallback
import com.gitlab.vladimirp1.rxjavaexperiments.fragments.PhotoDetailFragment
import com.gitlab.vladimirp1.rxjavaexperiments.fragments.PhotoRecyclerFragment
import com.gitlab.vladimirp1.rxjavaexperiments.fragments.PhotoSearchFragment
import io.reactivex.disposables.CompositeDisposable
import kotlinx.android.synthetic.main.activity_main.*
import org.kodein.di.Kodein
import org.kodein.di.KodeinAware
import org.kodein.di.android.closestKodein
import org.kodein.di.generic.instance
import retrofit2.Retrofit


class MainActivity : AppCompatActivity(), KodeinAware, ISelectedCallback {
    override val kodein: Kodein by closestKodein()

    private var isSetup = false

    private var disp: CompositeDisposable = CompositeDisposable()


    override fun onCreate(savedInstanceState: Bundle?) {
        super.onCreate(savedInstanceState)
        isSetup = savedInstanceState?.getBoolean("isSetup") ?: false
        setContentView(R.layout.activity_main)



        if (!isSetup) {
            val fragTransaction = supportFragmentManager.beginTransaction()
            supportFragmentManager.popBackStack("MainFrame", POP_BACK_STACK_INCLUSIVE)
            val myFrag = PhotoRecyclerFragment.newInstance(DatabaseProviderDesc())
            fragTransaction.replace(fragment.id, myFrag).commit()
        }
        isSetup = true

        navigation.setOnNavigationItemSelectedListener { it ->
            val myFrag = when(it.itemId){
                R.id.action_likes -> PhotoRecyclerFragment.newInstance(DatabaseProviderDesc())
                R.id.action_feed -> PhotoRecyclerFragment.newInstance(NetworkProviderDesc())
                R.id.action_search -> PhotoSearchFragment()
                else -> PhotoRecyclerFragment.newInstance(DatabaseProviderDesc())
            }

            supportFragmentManager.popBackStack("details", POP_BACK_STACK_INCLUSIVE)
            supportFragmentManager.popBackStack("MainFrame", POP_BACK_STACK_INCLUSIVE)
            supportFragmentManager.beginTransaction().replace(R.id.fragment, myFrag).commit()

            true
        }

    }

    override fun onSaveInstanceState(outState: Bundle?) {
        super.onSaveInstanceState(outState)
        outState?.putBoolean("isSetup", isSetup)
        //outState?.putParcelable("RecyclerState", mainRecycler.layoutManager?.onSaveInstanceState())
    }

    override fun onDestroy() {
        super.onDestroy()
        disp.dispose()
    }

    override fun itemClicked(p: Photo) {
        val myFrag = PhotoDetailFragment.newInstance(p)
        supportFragmentManager.popBackStack("details", POP_BACK_STACK_INCLUSIVE)
        supportFragmentManager.beginTransaction().replace(R.id.fragment, myFrag).addToBackStack("details").commit()
    }
}
