package com.gitlab.vladimirp1.rxjavaexperiments.fragments


import android.databinding.DataBindingUtil
import android.databinding.ObservableBoolean
import android.databinding.ObservableField
import android.os.Bundle
import android.support.v4.app.Fragment
import android.view.LayoutInflater
import android.view.View
import android.view.ViewGroup
import com.gitlab.vladimirp1.rxjavaexperiments.*

import com.gitlab.vladimirp1.rxjavaexperiments.databinding.FragmentPhotoDetailBinding
import com.squareup.picasso.Picasso
import io.reactivex.disposables.CompositeDisposable
import kotlinx.android.synthetic.main.fragment_photo_detail.*
import org.kodein.di.Kodein
import org.kodein.di.KodeinAware
import org.kodein.di.android.support.closestKodein
import org.kodein.di.generic.instance

private const val PHOTO_PARAM = "param1"

class PhotoDetailFragment : Fragment(), KodeinAware {
    override val kodein: Kodein by closestKodein()
    private val liker: PhotoLiker by instance()
    private val fetcher: AssetUrlFetcher by instance()
    private val disposable = CompositeDisposable()

    val liked = ObservableBoolean()
    val description = ObservableField<String>()


    private lateinit var binding: FragmentPhotoDetailBinding

    private var photo: Photo? = null

    override fun onCreate(savedInstanceState: Bundle?) {
        super.onCreate(savedInstanceState)
        arguments?.let {
            photo = it.getParcelable(PHOTO_PARAM)
            description.set(photo!!.name + " " + photo!!.description)
        }
    }

    override fun onCreateView(
        inflater: LayoutInflater, container: ViewGroup?,
        savedInstanceState: Bundle?
    ): View? {
        val view = inflater.inflate(R.layout.fragment_photo_detail, container, false)

        binding = DataBindingUtil.bind(view)!!
        binding.frag = this

        updateLiked()

        disposable.addAll(
            fetcher.getHiResUrl(photo!!.nasaId).moveToIoThread().subscribe({
                Picasso.get().load(it).placeholder(R.drawable.ic_emblem_synchronizing_symbolic)
                    .error(R.drawable.ic_emblem_fail).into(imageView)
            }, {
                Picasso.get().load(photo?.previewUrl).placeholder(R.drawable.ic_emblem_synchronizing_symbolic)
                    .error(R.drawable.ic_emblem_fail).into(imageView)
            })
        )

        return view
    }

    override fun onDestroyView() {
        Picasso.get().cancelRequest(imageView)
        super.onDestroyView()
    }

    override fun onDestroy() {
        disposable.dispose()
        super.onDestroy()
    }


    companion object {
        @JvmStatic
        fun newInstance(param1: Photo) =
            PhotoDetailFragment().apply {
                arguments = Bundle().apply {
                    putParcelable(PHOTO_PARAM, param1)
                }
            }
    }

    fun onLikeClicked(v: View) {
        val newstate = !liked.get()
        disposable.addAll(liker.likePhoto(photo!!, !liked.get()).moveToIoThread().subscribe {
            liked.set(newstate)
        })
    }

    private fun updateLiked() {
        disposable.addAll(liker.isLiked(photo!!.nasaId).moveToIoThread().subscribe {
            liked.set(it)
        })
    }
}
