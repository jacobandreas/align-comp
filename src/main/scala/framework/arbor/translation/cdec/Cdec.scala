package framework.arbor.translation.cdec

import java.io._

import framework.igor.logging.Logging
//import summarization.corpus.CorpusLoader
//import summarization.main.Config

/**
 * @author jda
 */
class Cdec(cdecLocation: String, options: (String,String)*) extends Logging {

  def translate(input: IndexedSeq[String]): IndexedSeq[IndexedSeq[String]] = {
    val opts = options.map(t => IndexedSeq(t._1, t._2)).flatten.toArray.filterNot(_.isEmpty)
    val cmd = s"$cdecLocation ${opts.mkString(" ")}"
    val pb = new ProcessBuilder((cdecLocation +: opts): _*)
    pb.redirectErrorStream(true)
    val cdec = pb.start()
    logger.info(s"running $cmd")

    val in = new BufferedWriter(new OutputStreamWriter(cdec.getOutputStream))
    val out = new BufferedReader(new InputStreamReader(cdec.getInputStream))

    input.foreach { s =>
      in.write(s)
      in.newLine()
      in.flush()
    }

    cdec.getOutputStream.flush()
    cdec.getOutputStream.close()

    out.lines
       .toArray
       .toIndexedSeq
       .asInstanceOf[IndexedSeq[String]]
       .filter(_.contains("|||"))
       .map { s => s.split(" \\|\\|\\| ") }
       .groupBy(_(0).toInt)
       .toIndexedSeq
       .sortBy(_._1)
       .map { case (i, sents) => sents.map(_(1)) }
  }
}

//object Cdec {
//  def main(args: Array[String]): Unit = {
//    val cdec = new Cdec("/Users/jda/Code/3p/cdec/decoder/cdec",
//                        "-f" -> "SCFG",
//                        "-g" -> "/Users/jda/Corpora/ppdb/nocat/ppdb-1.0-s-lexical.gz",
//                        "-g" -> "/Users/jda/Corpora/ppdb/nocat/ppdb-1.0-s-lexical-self.gz",
//                        "-g" -> "/Users/jda/Corpora/ppdb/nocat/ppdb-1.0-s-phrasal.gz",
//                        "-i" -> "-",
//                        "-k" -> "50",
//                        "-r" -> "",
//                        "-P" -> "",
//                        "--goal" -> "X")
//    val corpus = CorpusLoader.load()(Config())
//    corpus.foreach { datum =>
//      val goldUnigrams: Set[IndexedSeq[Int]] = datum.summary.sentences.map(_.sliding(2)).flatten.toSet
//      val docUnigrams: Set[IndexedSeq[Int]] = datum.documents.flatMap(_.sentences.map(_.sliding(2)).flatten).toSet
//
//      val stringSentences = datum.documents.flatMap { doc =>
//        doc.sentences.map { sent =>
//          sent.map(datum.vocabulary.get).mkString(" ")
//        }
//      }
//      val translations = cdec.translate(stringSentences)
//      val trIds = translations.flatMap { trSet =>
//        trSet.map { translation =>
//          val words = translation.split(" ")
//          val ids = words map datum.vocabulary
//          ids
//        }
//      }
//      val trUnigrams: Set[IndexedSeq[Int]] = trIds.map(_.sliding(2).map(_.toIndexedSeq)).flatten.toSet
//
//      val docRecall = 1d * (goldUnigrams intersect docUnigrams).size / goldUnigrams.size
//      val trRecall = 1d * (goldUnigrams intersect trUnigrams).size / goldUnigrams.size
//
//      println(s"doc: $docRecall, tr: $trRecall")
//    }
//  }
//}
