

#																		
class Sentence:
	def __init__ (self, _sText):
		self.s_Text = _sText
		self.lst_Actions = []


class Annotation:
	def __init__ (self, _sID, _iIndex, _bHeldout):
		self.s_ID = _sID
		self.i_Index = _iIndex
		self.lst_Sentences = []



# --------------------------------------------------------------
class Object:
	def __init__ (self):
		self.i_Index = None
		self.s_ID = ''
		self.s_MainWndID = ''
		self.s_ActivationWndID = ''
		self.s_Name = ''
		self.s_CleanName = ''
		self.i_ObjectClass = 0
		self.s_ObjectClass = ''
		self.v_RawFeatures = []
		self.d_ReplyFeatures = []



# --------------------------------------------------------------
class Article:
	def __init__ (self):
		self.i_Index = None
		self.i_ID = None
		self.lst_Instructions = []



# --------------------------------------------------------------
class Learner:

	#															
	def prerun_check_article (self, _oArticle):
		bArticleOK = True
		for iSentence, lstWords in enumerate (_oArticle.lst_Instructions):
			self.lst_CurrentInstructionWords = list (lstWords)

			bSentenceOK = True
			for x in self.lst_CurrentInstructionWords:
				if None == self.map_WordToIndex.get (x, None):
					if True == bArticleOK:
						print '[ERROR] prerun check failed on article [' + str(_oArticle.i_ID) + ']'
					if True == bSentenceOK:
						print '   ' + str (self.lst_CurrentInstructionWords)
					print '       word [' + x + '] missing from vocabulary'
					bSentenceOK = False
					bArticleOK = False
		return bArticleOK

	

	# 															
	def sample_actions (self, _oArticle):

		iBlocksRemaining = -1
		iTimeStep = 0
		bEnvChanged = False
		sPreviousCommand = ''

		self.lst_Actions = []
		self.lst_Feedback = []
		self.i_SelectedTerminatorWordLocation = 0

		for lstWords in _oArticle.lst_Instructions:
			self.lst_CurrentInstructionWords = list (lstWords)
			self.lst_CurrentInstructionWordIndices = []
			for x in self.lst_CurrentInstructionWords:
				self.lst_CurrentInstructionWordIndices.append (self.map_WordToIndex [x])

			self.i_CurrentInstructionLength = len (lstWords)
			self.set_UnusedWordLocations = set (range (self.i_CurrentInstructionLength))
			self.i_FirstUnusedWordLocation = 0

			if True == self.b_TestMode:
				file_InteractionLog.write ('      ' + ' '.join (lstWords) + '\n')

			self.lst_Actions.append ([])
			self.lst_Feedback.append ([])


			# iterate over the words in the current sentences ...		
			while len (self.set_UnusedWordLocations) > 0:

				# get environment state ...				
				if False == self.get_environment_state ():
					if True == self.b_TestMode:
						file_InteractionLog.write ('      --- get_env == False. No valid lines in env\n')
					if len (self.lst_Feedback) > 0:
						if len (self.lst_Feedback [-1]) > 0:
							self.lst_Feedback [-1][-1] = '-'
						elif len (self.lst_Feedback [-2]) > 0:
							self.lst_Feedback [-2][-1] = '-'
					return False

				# compute p(a|.) & sample...			
				self.sample_terminator ()

				self.set_WordsForCurrentAction = filter (lambda x: x < self.i_SelectedTerminatorWordLocation, \
														 self.set_UnusedWordLocations)
				if 0 == len (self.set_WordsForCurrentAction):
					if True == self.b_TestMode:
						file_InteractionLog.write ('      --- no words in segment\n')
					if len (self.lst_Feedback [-1]) > 0:
						self.lst_Feedback [-1][-1] = '-'
					break

				self.print_selections ()

				self.sample_action ()

				self.lst_Feedback [-1].append (' ')
				self.lst_Actions [-1].append ((self.i_SelectedTerminatorWordLocation, \
												self.i_SelectedCountWordLocation, \
												self.i_SelectedCount, \
												self.i_SelectedAction))

				# update traces & reward ...			
				self.update_traces (iTimeStep)
				iTimeStep = iTimeStep + 1

				# send perform action in environment ...
				self.print_action ()
				iBlocksRemaining = send_action (self.lst_CurrentState [self.i_SelectedAction][0])
				if None == iBlocksRemaining:
					if True == self.b_TestMode:
						file_InteractionLog.write ('      --- get_env == False during repeat\n')
					self.lst_Feedback [-1][-1] = '-'
					assert False
					return False
				if 0 == iBlocksRemaining:
					self.lst_Feedback [-1][-1] = '+'
					break

				#										
				self.update_used_words_collections ()

		if 0 != iBlocksRemaining:
			if len (self.lst_Feedback) > 0:
				if len (self.lst_Feedback [-1]) > 0:
					self.lst_Feedback [-1][-1] = '-'
				return (0 == iBlocksRemaining)
			if len (self.lst_Feedback) > 1:
				if len (self.lst_Feedback [-2]) > 0:
					self.lst_Feedback [-2][-1] = '-'
				return (0 == iBlocksRemaining)

		return (0 == iBlocksRemaining)




	# 													
	def compute_reward (self, _oArticle):
		sRewardType = self.map_ArticleIdToRewardType.get (_oArticle.i_ID, None)
		if None == sRewardType:
			return self.compute_environment_reward (_oArticle)
		elif 'pa' == sRewardType:
			return self.compute_partial_annotation_reward (_oArticle)
		elif 'fa' == sRewardType:
			return self.compute_full_annotation_reward (_oArticle)
		else:
			file_InteractionLog.write ('[ERROR]  unknown reward computation type (' + sRewardType +\
										') for article ' + str (_oArticle.i_ID) + '\n')
			file_InteractionLog.flush ()
			print '[ERROR]  unknown reward computation type (' + sRewardType +\
					') for article ' + str (_oArticle.i_ID)
			assert (False)



	# 													
	def compute_partial_annotation_reward (self, _oArticle):
		
		assert False

		lstReward = []
		return lstReward




	# 															
	def compute_environment_reward (self, _oArticle):
		
		fTotalUsedWordRatio = 1 - (1.0 * len (self.set_UnusedWordLocations) / self.i_CurrentInstructionLength)

		lstReward = []
		for lstWords, lstFeedback, lstActions \
				in zip (_oArticle.lst_Instructions, self.lst_Feedback, self.lst_Actions):

			for sFeedback, lstActionParams in zip (lstFeedback, lstActions):

				if ' ' == sFeedback:
					lstReward.append (0.1 * fTotalUsedWordRatio * self.d_PositiveRewardMultiplier)
					continue

				if '-' == sFeedback:
					lstReward.append (-1.0 * self.d_NegativeRewardMultiplier)
					continue

				if '+' == sFeedback:
					lstReward.append (1.0 * fTotalUsedWordRatio * self.d_PositiveRewardMultiplier)
			

		# compute cumulative reward ...				
		for i in xrange (len (lstReward) - 1, 0, -1):
			lstReward [i-1] += lstReward [i]

		return lstReward





	# 															
	def process_article (self, _oArticle):

		# initialize environment state ...						
		reset_environment_to_snapshot (_oArticle.s_RequiredStartingEnvState)

		# initialize learner for learning episode ...	
		self.init_for_new_episode ()
		self.initialize_traces ()
		self.lst_Feedback = []

		# get environment state ...						
		if False == self.get_environment_state ():
			return False

		# saple_actions returns False if an error		
		# occurs that precludes parameter update.		
		bGameCompleted = self.sample_actions (_oArticle)


		# compute reward based on action sequence ...	
		lstReward = self.compute_reward (_oArticle)
		iTotalReward = sum (lstReward)
		if None == lstReward:
			print '    compute_reward returned None.'
			file_InteractionLog.write ('    compute_reward returned None.\n')
			return (False, iTotalReward)
		sReward = '      reward : ' + str (map (lambda x: '%.1f'%x, lstReward))
		if True == self.b_TestMode:
			print sReward
			file_InteractionLog.write (sReward + '\n')


		# update weights ...
		if False == self.b_TestMode:
			self.compute_weight_update (lstReward)

		# we do block updates of the weights, so first have to check	
		# if a sufficient number of actions have been sampled...		
		self.i_ProcessedDocuments += 1
		if self.i_ProcessedDocuments > self.i_DocumentsPerUpdate:
			self.update_weights ()
			self.save_weights ()
			self.i_ProcessedDocuments = 0

		return (bGameCompleted, iTotalReward)



	# 															
	def get_environment_state (self):
		(lstState, bHasVisibleObject) = measure_state ()

		if False == bHasVisibleObject:
			return False

		self.set_environment_state (lstState)
		return True



	# 															
	def set_environment_state (self, _lstState):
		self.lst_CurrentState = _lstState
		self.i_CurrentStateObjectCount = len (self.lst_CurrentState)

		iColumn = 0
		iRow = 0
		for oAction in self.lst_CurrentState:
			if 1 == oAction [1][0]:
				iColumn += 1
			else:
				iRow += 1

		self.i_MaxColumns = max (iRow, iColumn)



	# 															
	def print_selections (self):
		if False == self.b_TestMode:
			return

		sLog = 't[' + str (self.i_SelectedTerminatorWordLocation) + '][' +\
				str (self.lst_CurrentInstructionWords [self.i_SelectedTerminatorWordLocation]) + ']  '

		if True == self.b_Verbose:
			print sLog
		file_InteractionLog.write (sLog + '\n')




	# 															
	def print_action (self):
		if False == self.b_TestMode:
			return
		
		sLog = str (self.lst_CurrentState [self.i_SelectedAction])
		if True == self.b_Verbose:
			print '           - ' + sLog
		file_InteractionLog.write ('           - ' + sLog + '\n')
		file_InteractionLog.flush ()








