package com.gitlab.vladimirp1.roomexperiments

import android.arch.persistence.room.Room
import android.support.v7.app.AppCompatActivity
import android.os.Bundle

class MainActivity : AppCompatActivity() {

    override fun onCreate(savedInstanceState: Bundle?) {
        super.onCreate(savedInstanceState)
        setContentView(R.layout.activity_main)

        val db = Room.databaseBuilder(this, LikeDatabase::class.java, "likes").build()
        db.likeDao().insert(arrayOf(Like(12, "a", "b")))
        val r = db.likeDao().getAll()
        r.get(0)
    }
}

