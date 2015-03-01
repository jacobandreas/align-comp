package model

import breeze.numerics.log
import framework.fodor._
import framework.igor.util.Memoize

/**
 * @author jda
 */
object Featurizer {

  def discretize(feature: Feature): Seq[IndicatorFeature] = {
    feature match {
      case f: IndicatorFeature => Seq(f)
      case f: RealFeature => Seq(SimpleFeature(s"${f.name}__SIGN+=" + (f.rvalue >= 0).toString),
                                 SimpleFeature(s"${f.name}__MAG=" + (log(f.rvalue.abs) * 2).toInt))
    }
  }

  def join(textFeature: IndicatorFeature, pathFeature: IndicatorFeature): Seq[IndicatorFeature] = {
//  val join = Memoize[(IndicatorFeature,IndicatorFeature),Seq[IndicatorFeature]] { case (textFeature: IndicatorFeature, pathFeature: IndicatorFeature) =>
    {
      (textFeature, pathFeature) match {

        case (tf:StringFeature, pf:StringFeature) =>
          val matchFeature: Seq[SimpleFeature] =
            if (stringMatch(tf.svalue, pf.svalue))
              Seq(SimpleFeature(s"StringMatch__${tf.name}__IN__${pf.name}"))
            else
              Seq[SimpleFeature]()

          matchFeature :+ SimpleFeature(s"${tf.value}__${pf.value}")

        case (tf:IndicatorFeature, pf:IndicatorFeature) =>
          Seq(SimpleFeature(s"${tf.value}__AND__${pf.value}"))

        case _ =>
          Seq[SimpleFeature]()
      }
    }.toSeq // ++ Seq(SimpleFeature(s"${textFeature.value}__CONST"),
            //       SimpleFeature(s"CONST__${pathFeature.value}"))
  }

  def stringMatch(needle: String, haystack: String): Boolean = {
    haystack.split("""\s+""").contains(needle)
  }

}
