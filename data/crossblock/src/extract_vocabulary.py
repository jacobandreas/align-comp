import sys, os, re
from config import *

set_Words = set ()
map_Config = {}
rgx_Whitespace = re.compile ('[ \t]+')



#													
def get_words (_sLine):
	sLine = _sLine

	sCleanLine = ''
	iLast = 0
	iStart = sLine.find ('<b>')
	iEnd = iStart
	while -1 != iStart:
		iEnd = sLine.find ('</b>', iStart)
		if -1 == iEnd:
			print sLine
		assert (-1 != iEnd)

		sCleanLine += sLine [iLast:iStart]
		iLast = iEnd
		for i in xrange (iStart, iEnd):
			if ' ' == sLine [i]:
				sCleanLine += '\x01'
			else:
				sCleanLine += sLine [i]
		iStart = sLine.find ('<b>', iEnd)

	sCleanLine += sLine [iLast:]

	sCleanLine = sCleanLine.replace ('</b>', ' ')
	sCleanLine = sCleanLine.replace ('<b>', '\x02')
	sCleanLine = rgx_Whitespace.sub (' ', sCleanLine)

	return sCleanLine.split ()


#													
def load_articles ():
	print '   loading articles ...'

	global set_Words

	file = open (get_config ('article_file'))
	lstLines = map (lambda x: x.strip (), file.readlines ())
	file.close ()

	sObjectMarker = '\x02'

	setArticleIDs = set ()
	bInArticle = False
	iArticleCount = 0
	iIndex = 0
	for sLine in lstLines:
		if len (sLine) == 0:
			continue
		if '@' == sLine [0]:
			continue
		if '#' == sLine [0]:
			sLine = sLine [1:].strip ()
			if sLine.isdigit ():
				if sLine in setArticleIDs:
					print 'duplicate article? ' + sLine
				setArticleIDs.add (sLine)
			continue

		sLine = sLine.replace (',', ' ')
		sLine = sLine.strip ().lower ()
		if '' == sLine:
			if True == bInArticle:
				iArticleCount += 1
			bInArticle = False
			continue
		
		bInArticle = True
		lstRawWords = map (lambda x: x.strip (), get_words (sLine))
		for sWord in lstRawWords:
			sWord = sWord.replace ('\x02', ' ')
			sWord = sWord.strip ().replace ('\x01', ' ').strip ()
			set_Words.add (sWord)

	if True == bInArticle:
		iArticleCount += 1

	print '      ' + str (iArticleCount) + ' articles.'
	print '      ' + str (len (set_Words)) + ' words.'



#													
def main ():

	load_config (sys.argv)
	load_articles ()

	sVocabularyFile = get_config ('vocabulary_file')
	print 'writing vocabulary to : ' + sVocabularyFile
	file = open (sVocabularyFile, 'w')
	
	file.write ('\n'.join (set_Words))
	file.close ()



if __name__ == '__main__':
	sys.exit (main ())
