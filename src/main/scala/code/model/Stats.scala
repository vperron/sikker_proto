package code
package model

import net.liftweb.mapper._
import net.liftweb.util._
import net.liftweb.http._
import net.liftweb.common._


class Stats extends KeyedMapper[String, Stats] {
  def getSingleton = Stats // what's the "meta" server

  def primaryKeyField = id

  object id extends MappedStringIndex(this, 32)

  object timestamp extends MappedDateTime(this) {
    override def dbIndexed_? = false
  }

  object temperature extends MappedInt(this)
  object cardio extends MappedInt(this)
  object accel extends MappedInt(this)
  object noise extends MappedInt(this)
}


object Stats extends Stats with KeyedMetaMapper[String, Stats] {

}
