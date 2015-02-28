correspondences.txt
  provides mapping between dialogues and maps

images/
  contains images of the maps

landmarks/*g.xml
  XML files describing landmark location

landmarks/*.seg.txt
  human-segmented paths (numbers are indices into `stop` nodes)
  (not used in any of my experiments)
  
transcripts/*.txt.clean
  dialog transcripts, stripped of metadata and "follower" utterances

transcripts/*.ptb
  parsed versions of the above

