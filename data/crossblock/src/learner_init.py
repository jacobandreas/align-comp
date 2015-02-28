

	#															
	def __init__ (self):
		self.map_ArticleIdToPartialAnnotation = {}
		self.map_ArticleIdToRewardType = {}

		self.b_Verbose = False
		self.b_TestMode = False

		self.i_FeatureCount_WT = None
		self.i_FeatureCount_WN = None
		self.i_FeatureCount_N = None
		self.i_FeatureCount_A = None

		self.d_PositiveRewardMultiplier = None
		self.d_NegativeRewardMultiplier = None
		self.d_NegativeRewardMultiplierMin = None
		self.d_NegativeRewardMultiplierMax = None

		self.d_LearningRate = None
		self.d_ExplorationRate = None
		self.b_EpsilonGreedy = None
		self.d_SamplingEpsilon = None
		self.i_MaxAlternates = None

		self.i_MaxActionRepetitions = None

		self.d_Weights_WT = []
		self.d_Weights_WN = []
		self.d_Weights_N = []
		self.d_Weights_A = []
		self.d_AverageWeights_WT = 0
		self.d_AverageWeights_WN = 0
		self.d_AverageWeights_N = 0
		self.d_AverageWeights_A = 0

		self.d_Prob_WT = []
		self.d_Prob_WN = []
		self.d_Prob_N = []
		self.d_Prob_A = []

		self.d_Features_WT = []
		self.d_Features_WN = []
		self.d_Features_N = []
		self.d_Features_A = []

		self.lst_Actions = []
		self.lst_Feedback = []

		self.d_Trace_WT = []
		self.d_Trace_WN = []
		self.d_Trace_N = []
		self.d_Trace_A = []
		self.d_WeightUpdates_WT = None
		self.d_WeightUpdates_WN = None
		self.d_WeightUpdates_N = None
		self.d_WeightUpdates_A = None

		self.map_WordToIndex = {}

		self.b_UseMaxProbabilityAction = False

		# intermediate information		
		self.i_ProcessedDocuments = 0
		self.i_DocumentsPerUpdate = None

		self.lst_CurrentInstructionWords = []
		self.i_CurrentInstructionLength = None
		self.set_UnusedWordLocations = None
		self.set_WordsForCurrentAction = None
		self.i_FirstUnusedWordLocation = None

		self.lst_CurrentState = []
		self.i_CurrentStateObjectCount = None
		self.i_MaxColumns = None

		self.i_SelectedTerminatorWordLocation = None
		self.i_SelectedCountWordLocation = None
		self.i_SelectedCount = None
		self.i_SelectedAction = None




	#																
	def init_learner (self):
		global set_HeldoutArticles
		global i_TotalArticles

		print '   initializing learner ...'
		self.load_vocabulary ()

		self.b_Verbose = ('1' == get_config ('verbose'))
		if 'article_count' == get_config ('documents_per_weight_update'):
			self.i_DocumentsPerUpdate = i_TotalArticles - len (set_HeldoutArticles)
		else:
			self.i_DocumentsPerUpdate = int (get_config ('documents_per_weight_update'))
		print '      updating weights every ' + str (self.i_DocumentsPerUpdate) + ' documents'
		self.b_UseMaxProbabilityAction = ('1' == get_config ('use_max_probability_action'))

		self.d_PositiveRewardMultiplier = float (get_config ('positive_reward_multiplier'))
		self.d_NegativeRewardMultiplier = float (get_config ('negative_reward_multiplier'))
		self.d_NegativeRewardMultiplierMin = float (get_config ('negative_reward_multiplier_min'))
		self.d_NegativeRewardMultiplierMax = float (get_config ('negative_reward_multiplier_max'))
	
		self.d_LearningRate = float (get_config ('learning_rate'))
		self.d_ExplorationRate = float (get_config ('exploration_rate'))
		self.b_EpsilonGreedy = ('epsilon-greedy' == get_config ('sampling_method'))
		self.d_SamplingEpsilon = float (get_config ('sampling_epsilon'))
		self.i_MaxAlternates = int (get_config ('max_alternate_interpretations'))
		if True == self.b_EpsilonGreedy:
			print '      using epsilog-greedy sampling with epsilon = ' + str (self.d_SamplingEpsilon)
		else:
			print '      using softmax sampling with exploration rate = ' + str (self.d_ExplorationRate)

		self.i_MaxActionRepetitions = int (get_config ('max_repetitions'))


		# +1 below is for object class name in neighbourhood
		self.i_FeatureCount_WT = self.i_VocabularySize + 20
		self.i_FeatureCount_WN = self.i_VocabularySize
		self.i_FeatureCount_N = self.i_VocabularySize * self.i_MaxActionRepetitions
		self.i_FeatureCount_A = (1 + self.i_VocabularySize) * int (get_config ('environment_feature_count'))


		self.d_Weights_WT = []
		self.d_Weights_WN = []
		self.d_Weights_N = []
		self.d_Weights_A = []

		if '0' == get_config ('random_initial_weights'):
			print '      using zero initial weights'
			self.d_Weights_WT = [0] * self.i_FeatureCount_WT
			self.d_Weights_WN = [0] * self.i_FeatureCount_WN
			self.d_Weights_N = [0] * self.i_FeatureCount_N
			self.d_Weights_A = [0] * self.i_FeatureCount_A
		else:
			print '      using random initial weights'
			self.d_Weights_WT = [0] * self.i_FeatureCount_WT
			for i in xrange (self.i_FeatureCount_WT):
				self.d_Weights_WT.append (random.uniform (-0.1, 0.1))

			for i in xrange (self.i_FeatureCount_WN):
				self.d_Weights_WN.append (random.uniform (-0.1, 0.1))

			for i in xrange (self.i_FeatureCount_N):
				self.d_Weights_N.append (random.uniform (-0.1, 0.1))

			for i in xrange (self.i_FeatureCount_A):
				self.d_Weights_A.append (random.uniform (-0.1, 0.1))




	#																
	def init_for_new_episode (self):

		self.lst_CurrentInstructionWords = []
		self.i_CurrentInstructionLength = None

		self.lst_CurrentState = []
		self.i_CurrentStateObjectCount = None

		self.i_SelectedTerminatorWordLocation = None
		self.i_SelectedCountWordLocation = None
		self.i_SelectedCount = None
		self.i_SelectedAction = None



