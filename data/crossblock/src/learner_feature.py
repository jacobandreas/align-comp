
	#																
	def distance_to_index (self, iDistance):
		if iDistance < 0:
			iDistance = iDistance + 10
			if iDistance < 0:
				iDistance = 0
		else:
			iDistance = iDistance + 9
			if iDistance > 19:
				iDistance = 19
		return iDistance



	# 																
	def feature_wt (self, _iWordLocation, _dWeights):
		fv = [0.0] * self.i_FeatureCount_WT
		dScore = 0.0

		iWordIndex = self.lst_CurrentInstructionWordIndices [_iWordLocation]
		fv [iWordIndex] = 1.0
		dScore += _dWeights [iWordIndex]
		iOffset = self.i_VocabularySize

		iDistance = _iWordLocation - self.i_FirstUnusedWordLocation

		if iDistance > 19:
			iDistance = 19
		fv [iOffset + iDistance] = 1.0
		dScore += _dWeights [iOffset + iDistance]
		iOffset = iOffset + 20

		assert (iOffset == self.i_FeatureCount_WT)
		self.d_Features_WT [_iWordLocation] = fv
		return dScore




	# 																
	def feature_a (self, _iAction, _dWeights):
		fv = [0.0] * self.i_FeatureCount_A
		dScore = 0.0

		oAction = self.lst_CurrentState [_iAction]
		lstActionFeatures = oAction [1]
		for i, af in enumerate (lstActionFeatures):
			fv [i] = 1.0 * af
			dScore += af * _dWeights [i]
		iOffset = len (lstActionFeatures)

		for i, af in enumerate (lstActionFeatures):
			for lwt in self.set_WordsForCurrentAction:
				w = self.lst_CurrentInstructionWordIndices [lwt]
				fv [iOffset + w + self.i_VocabularySize * i] = 1.0 * af
				dScore += af * _dWeights [iOffset + w + self.i_VocabularySize * i]

		self.d_Features_A [_iAction] = fv
		return dScore




	#															
	def compute_score_wt (self, _iWordLocation):
		dScore = self.feature_wt (_iWordLocation, self.d_Weights_WT)
		dScore *= self.d_ExplorationRate
		if dScore > 700:
			dScore = 700
		return dScore



	#															
	def compute_score_a (self, _iAction):
		dScore = self.feature_a (_iAction, self.d_Weights_A)
		dScore *= self.d_ExplorationRate
		if dScore > 700:
			dScore = 700
		return dScore



