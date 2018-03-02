package util

import com.google.common.collect._


/**
  * The unique map is used for the psdd node cache
  */

class UniqueMap[Key <: Object, Value <: Object](){

  val cache = new MapMaker()
    .weakValues()
    .makeMap[Key, Value]()

  def apply(k: Key, v: =>Value): Value = {
    var cached = cache.get(k)
    if(cached == null){
      cached = v
      cache.put(k, cached)
    }
    cached
  }

  def get(k: Key) = if (cache.containsKey(k)) Some(cache.get(k)) else None
  def put(k: Key, v: Value){
    cache.put(k, v)
  }

  def remove(k: Key): Unit = {
    cache.remove(k)
  }

  def copy(): UniqueMap[Key,Value] = {
    val copied = new UniqueMap[Key,Value]()
    copied.cache.putAll(this.cache)
    return copied
  }
}