package framework.arbor.segmentation

/**
 * @author jda
 */
object EpicWordSegmenter extends WordSegmenter {
  val segmenter = new epic.preprocess.TreebankTokenizer()
  override def apply(arg: String): IndexedSeq[String] = segmenter(arg)
}
