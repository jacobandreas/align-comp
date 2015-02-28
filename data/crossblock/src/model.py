

file_InteractionLog = None
map_StartingEnvToArticle = {}
set_HeldoutArticles = set ()
i_TotalArticles = 0



#													
def get_words (_sLine):
	sLine = _sLine

	sCleanLine = ''
	iLast = 0
	iStart = sLine.find ('<b>')
	while -1 != iStart:
		iEnd = sLine.find ('</b>', iStart)
		assert (-1 != iEnd)

		sCleanLine = sCleanLine + sLine [iLast:iStart]
		iLast = iEnd
		for i in xrange (iStart, iEnd):
			if ' ' == sLine [i]:
				sCleanLine = sCleanLine + '\x01'
			else:
				sCleanLine = sCleanLine + sLine [i]
		iStart = sLine.find ('<b>', iEnd)

	sCleanLine = sCleanLine + sLine [iLast:]

	sCleanLine = sCleanLine.replace ('</b>', ' ')
	sCleanLine = sCleanLine.replace ('<b>', '\x02')
	return sCleanLine.split ()



#													
def load_heldout_articles ():
	print '   loading heldout artiles ...'
	global set_HeldoutArticles

	sHeldoutFile = get_config ('heldout_file')
	if '' == sHeldoutFile:
		return

	file = open (sHeldoutFile)
	lstLines = file.readlines ()
	file.close ()

	for sLine in lstLines:
		set_HeldoutArticles.add (sLine.strip ())

	print '      ' + str (len (set_HeldoutArticles)) + ' heldout articles'



#													
def load_articles ():
	print '   loading articles ...'
	global map_StartingEnvToArticle

	setIgnore = set ()
	file = open (get_config ('ignore_list_file'))
	for sLine in file.readlines ():
		sID = sLine.split ()[0].strip ()
		setIgnore.add (sID)
	file.close ()

	map_StartingEnvToArticle = {}

	file = open (get_config ('article_file'))
	lstLines = file.readlines ()
	file.close ()

	sObjectMarker = '\x02'

	iIndex = 0
	iID = None
	oArticle = None
	for sLine in lstLines:
		sLine = sLine.strip ()

		if (len (sLine) > 0) and ('#' == sLine [0]):
			lstValues = sLine.split ()
			if True == lstValues [1].replace ('-','').isdigit ():
				iID = lstValues [1]
			continue


		sLine = sLine.strip ()
		if '' == sLine:
			if None != oArticle:
				assert (None != oArticle.s_RequiredStartingEnvState)
				if None == map_StartingEnvToArticle.get (oArticle.s_RequiredStartingEnvState, None):
					map_StartingEnvToArticle [oArticle.s_RequiredStartingEnvState] = []
				map_StartingEnvToArticle [oArticle.s_RequiredStartingEnvState].append (oArticle)
			oArticle = None
			continue
		
		if None == oArticle:
			oArticle = Article ()
			oArticle.i_Index = iIndex
			oArticle.i_ID = iID
			iIndex = iIndex + 1

		if '@' == sLine [0]:
			oArticle.s_RequiredStartingEnvState = sLine [1:].strip ()
			continue

		sLine = sLine.lower ()
		lstRawWords = []
		for sWord in get_words (sLine):
			lstRawWords.append (sWord.strip ().replace ('\x01', ' ').strip ())

		lstWords = []
		for sWord in lstRawWords:
			lstWords.append (sWord.replace (sObjectMarker, '').strip ())

		oArticle.lst_Instructions.append (lstWords)

		iWords = len (lstWords)
	
	if None != oArticle:
		assert (None != oArticle.s_RequiredStartingEnvState)
		if None == map_StartingEnvToArticle.get (oArticle.s_RequiredStartingEnvState, None):
			map_StartingEnvToArticle [oArticle.s_RequiredStartingEnvState] = []
		map_StartingEnvToArticle [oArticle.s_RequiredStartingEnvState].append (oArticle)

	print '      ' + str (iIndex) + ' articles.'
	return iIndex



#													
def main (_bTestMode):

	global o_Game
	global map_StartingEnvToArticle
	global file_InteractionLog
	global i_TotalArticles

	# load configurations & data			
	load_config (sys.argv)
	shutil.copyfile (sys.argv [1], get_config ('output_path') + '/run.cfg')
	random.seed ()

	i_TotalArticles = load_articles ()
	load_heldout_articles ()
	learner = Learner ()
	learner.init_learner ()
	if '1' == get_config ('use_partial_annotation'):
		learner.load_partial_annotation ()

	bStartAfresh = ('restart' in sys.argv)
	if True == bStartAfresh:
		if 'Y' != raw_input ('   -- initialize weights and erase previous values?? [Y/n] : '):
			bStartAfresh = False
	if False == bStartAfresh:
		learner.load_weights ()
	else:
		learner.clear_weight_files ()


	# perform prerun check of articles ...	
	bPrerunCheckOk = True
	for lstArticles in map_StartingEnvToArticle.values ():
		for oArticle in lstArticles:
			if False == learner.prerun_check_article (oArticle):
				bPrerunCheckOk = False
	if False == bPrerunCheckOk:
		return

	o_Game = Game ()
	o_Game.load_game ()

	file_InteractionLog = open (get_config ('interaction_logfile'), 'a')


	# learn !								
	print
	print '-------------------------------------------------'
	iLearningIterations = int (get_config ('learning_iterations'))
	iTestIterationPeriod = int (get_config ('test_iteration_period'))
	iMaxRepeatIterations = int (get_config ('max_repeat_iterations'))


	lstStartStates = map_StartingEnvToArticle.keys ()
	for i in xrange (iLearningIterations):

		learner.b_TestMode = False
		learner.d_ExplorationRate = float (get_config ('exploration_rate'))
		if ((0 != i) or (True == _bTestMode)) and (0 == (i % iTestIterationPeriod)):
			learner.b_TestMode = True
			learner.d_ExplorationRate = 1.0

			file_InteractionLog.write ('=================================================\n')
			file_InteractionLog.write ('[[ TEST MODE ]]\n')
			file_InteractionLog.write ('=================================================\n')


		bLearningComplete = True
		iTotalReward = 0
		iArticleIndex = 0
		random.shuffle (lstStartStates)
		for sStartingEnv in lstStartStates:
			lstArticles = map_StartingEnvToArticle [sStartingEnv]
			random.shuffle (lstArticles)

			lstArticlesToBeReProcessed = []
			for oArticle in lstArticles:
				assert (sStartingEnv == oArticle.s_RequiredStartingEnvState)
				iArticleIndex = iArticleIndex + 1

				if False == learner.b_TestMode:
					if oArticle.i_ID in set_HeldoutArticles:
						continue

				learner.d_ExplorationRate = float (get_config ('exploration_rate'))
				
				sHeldoutMarker = ''
				if oArticle.i_ID in set_HeldoutArticles:
					sHeldoutMarker = 'heldout'

				if True == learner.b_TestMode:
					file_InteractionLog.write ('-------------------------------------------------\n')
					file_InteractionLog.write ('   ' + str(i) + ':0' + \
												' article [' + oArticle.i_ID + '] ' +\
												str (iArticleIndex) + '/' + \
												str (i_TotalArticles) + ', ' +\
												str (len (oArticle.lst_Instructions)) +\
												' instructions ' + sHeldoutMarker + '\n')

				(bGameCompleted, iReward) = learner.process_article (oArticle)
				iTotalReward += iReward
				bLearningComplete &= bGameCompleted

		print i, iReward

		file_InteractionLog.flush ()
