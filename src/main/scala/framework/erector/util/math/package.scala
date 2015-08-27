package framework.erector.util

import breeze.numerics.log

/**
 * @author jda
 */
package object math {

  def logPois(k: Int, lambda: Double): Double = {
    breeze.stats.distributions.Poisson(lambda).logProbabilityOf(k)
  }

}
