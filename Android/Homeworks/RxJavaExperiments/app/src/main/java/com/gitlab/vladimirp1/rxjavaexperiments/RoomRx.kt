package com.gitlab.vladimirp1.rxjavaexperiments

import android.arch.persistence.room.*
import io.reactivex.Maybe

@Dao
interface PhotoDao {
    @Query("SELECT * FROM `Photo`")
    fun getAll(): Maybe<List<Photo>>

    @Query("SELECT * FROM `Photo` WHERE nasaId IN (:nasaIds)")
    fun getByIds(nasaIds: Array<String>): Maybe<List<Photo>>

    @Insert(onConflict = OnConflictStrategy.REPLACE)
    fun insert(likes: Array<Photo>)

    @Delete
    fun delete(like: Photo)

    @Query("DELETE FROM `Photo` WHERE nasaId = :nasaId")
    fun delete(nasaId : String)
}

@Database(entities = [Photo::class], version = 1)
abstract class PhotoDatabase : RoomDatabase() {
    abstract fun photoDao(): PhotoDao
}