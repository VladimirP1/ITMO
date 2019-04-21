package com.gitlab.vladimirp1.retrofitexperiments.helpers

import android.arch.persistence.room.*
import android.os.AsyncTask

@Entity
data class Like(
    @PrimaryKey var assetId : Int,
    @ColumnInfo var title : String?,
    @ColumnInfo var description : String?
)

@Dao
interface LikeDao {
    @Query("SELECT * FROM `Like`")
    fun getAll() : List<Like>
    @Query("SELECT * FROM `Like` WHERE assetId IN (:assetIds)")
    fun getByIds(assetIds: Array<Int>) : List<Like>
    @Insert
    fun insert(likes : Array<Like>)
    @Delete
    fun delete(like : Like)
}

@Database(entities = arrayOf(Like::class), version = 1)
abstract class LikeDatabase : RoomDatabase() {
    abstract fun likeDao() : LikeDao
}



class LikeDatabaseInsertTask : AsyncTask<Array<Like>, Unit, Unit>() {
    override fun doInBackground(vararg params: Array<Like>?) {

    }

    override fun onPostExecute(result: Unit?) {
        super.onPostExecute(result)
    }

}