package framework.arbor.segmentation

/**
 * @author jda
 */
trait SentenceSegmenter extends (String => IndexedSeq[String])
