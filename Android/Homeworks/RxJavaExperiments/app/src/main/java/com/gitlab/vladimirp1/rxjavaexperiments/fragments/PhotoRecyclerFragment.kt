package com.gitlab.vladimirp1.rxjavaexperiments.fragments


import android.os.Bundle
import android.os.Parcelable
import android.support.v4.app.Fragment
import android.support.v7.widget.LinearLayoutManager
import android.util.Log
import android.view.LayoutInflater
import android.view.View
import android.view.ViewGroup
import com.gitlab.vladimirp1.rxjavaexperiments.MyAdapter
import com.gitlab.vladimirp1.rxjavaexperiments.Photo
import com.gitlab.vladimirp1.rxjavaexperiments.R
import com.gitlab.vladimirp1.rxjavaexperiments.PhotoProvider
import com.gitlab.vladimirp1.rxjavaexperiments.ProviderDescriptor
import com.gitlab.vladimirp1.rxjavaexperiments.moveToIoThread
import io.reactivex.disposables.CompositeDisposable
import kotlinx.android.synthetic.main.fragment_photo_recycler.*
import org.kodein.di.Kodein
import org.kodein.di.KodeinAware
import org.kodein.di.android.support.closestKodein

private const val ARG_PARAM1 = "param1"

interface ISelectedCallback{
    fun itemClicked(p: Photo)
}

class PhotoRecyclerFragment : Fragment(), KodeinAware {
    override val kodein: Kodein by closestKodein()
    private lateinit var providerFactory : ProviderDescriptor
    private lateinit var provider : PhotoProvider
    private val disposable = CompositeDisposable()


    override fun onCreate(savedInstanceState: Bundle?) {
        super.onCreate(savedInstanceState)
        arguments?.let {
            providerFactory = it.getParcelable(ARG_PARAM1)!!
            provider = providerFactory.build(kodein)
        }
    }

    override fun onActivityCreated(savedInstanceState: Bundle?) {

        recyclerState = savedInstanceState?.getParcelable("RecyclerState") ?: recyclerState
        super.onActivityCreated(savedInstanceState)
    }

    override fun onCreateView(
        inflater: LayoutInflater, container: ViewGroup?,
        savedInstanceState: Bundle?
    ): View? {

        return inflater.inflate(R.layout.fragment_photo_recycler, container, false)
    }

    private var recyclerState: Parcelable? = null

    override fun onStart() {
        super.onStart()

        disposable.addAll(provider.getObservable().moveToIoThread().subscribe
            {
                with(photoRecycler) {
                    adapter = MyAdapter(it.data) {
                        Log.d("Click", it.toString())
                        (activity as ISelectedCallback).itemClicked(it)
                    }
                    layoutManager = LinearLayoutManager(activity)
                    layoutManager?.onRestoreInstanceState(recyclerState)
                }
            }
        )
    }

    override fun onSaveInstanceState(outState: Bundle) {
        super.onSaveInstanceState(outState)
        if (photoRecycler != null) {
            outState.putParcelable("RecyclerState", photoRecycler.layoutManager?.onSaveInstanceState())
        }
    }

    override fun onDestroy() {
        super.onDestroy()
        disposable.dispose()
    }

    override fun onDestroyView() {
        super.onDestroyView()
        recyclerState = photoRecycler.layoutManager?.onSaveInstanceState();
    }

    companion object {
        @JvmStatic
        fun newInstance(param1: ProviderDescriptor) =
            PhotoRecyclerFragment().apply {
                arguments = Bundle().apply {
                    putParcelable(ARG_PARAM1, param1)
                }
            }
    }

}
