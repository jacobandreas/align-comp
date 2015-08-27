package framework.igor.util

/**
 * @author jda
 */

import collection.mutable

case class Memoize[K,V](f: K=>V) {
  private val cache = mutable.Map[K,V]()
  def apply(k: K): V = cache.getOrElseUpdate(k, f(k))
}

