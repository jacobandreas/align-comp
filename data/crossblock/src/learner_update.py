
	#															
	def update_used_words_collections (self):
		for i in xrange (self.i_SelectedTerminatorWordLocation + 1):
			self.set_UnusedWordLocations.discard (i)
		self.i_FirstUnusedWordLocation = self.i_SelectedTerminatorWordLocation + 1


	# initialize traces counts ...								
	def initialize_traces (self):
		self.d_Trace_WT = []
		self.d_Trace_WN = []
		self.d_Trace_N = []
		self.d_Trace_A = []




	# update the trace counts ...								
	def update_traces (self, _iTimeStep):
		self.update_trace_wt (_iTimeStep)
		self.update_trace_a (_iTimeStep)




	#															
	def update_trace_wt (self, _iTimeStep):
		# p(w|.)
		if len (self.d_Trace_WT) < _iTimeStep + 1:
			self.d_Trace_WT.append ([0.0] * self.i_FeatureCount_WT)
		assert (len (self.d_Trace_WT) == _iTimeStep + 1)

		swt = self.i_SelectedTerminatorWordLocation

		dSum = [0] * self.i_FeatureCount_WT
		for lwt in self.set_UnusedWordLocations:
			dProb = self.d_Prob_WT [lwt]
			if 0 == dProb:
				continue
			dFeatures_lwt = self.d_Features_WT [lwt]
			for k in xrange (self.i_FeatureCount_WT):
				if 0 != dFeatures_lwt [k]:
					dSum [k] = dSum [k] + dFeatures_lwt [k] * dProb

		
		dTrace_t = self.d_Trace_WT [_iTimeStep]
		dFeatures_swt = self.d_Features_WT [swt]
		for k in xrange (self.i_FeatureCount_WT):
			dDiff = dFeatures_swt [k] - dSum [k]
			if 0 != dDiff:
				dTrace_t [k] = dTrace_t [k] + dDiff





	#															
	def update_trace_a (self, _iTimeStep):
		# p(c|., wc)
		if len (self.d_Trace_A) < _iTimeStep + 1:
			self.d_Trace_A.append ([0.0] * self.i_FeatureCount_A)
		assert (len (self.d_Trace_A) == _iTimeStep + 1)

		swc = self.i_SelectedAction

		dSum = [0] * self.i_FeatureCount_A
		for ic in xrange (self.i_CurrentStateObjectCount):
			dFeatures_ic = self.d_Features_A [ic]
			dProb = self.d_Prob_A [ic]
			for k in xrange (self.i_FeatureCount_A):
				dSum [k] = dSum [k] + dFeatures_ic [k] * dProb

		dTrace_t = self.d_Trace_A [_iTimeStep]
		dFeatures_sic = self.d_Features_A [self.i_SelectedAction]
		for k in xrange (self.i_FeatureCount_A):
			dTrace_t [k] = dTrace_t [k] + (dFeatures_sic [k] - dSum [k])




	# update feature weights by:								
	#    \theta  = \theta  + \alpha        * R      * \Lambda	
	#    weights = weights + learning_rate * reward * trace		
	def compute_weight_update (self, _dReward):
		self.compute_weight_update_wt (_dReward)
		self.compute_weight_update_a (_dReward)




	#															
	def compute_weight_update_wt (self, _dReward):
		for n in xrange (len (self.d_Trace_WT) - 1, 0, -1):
			dTracePrev = self.d_Trace_WT [n - 1]
			dTrace = self.d_Trace_WT [n]
			for i, t in enumerate (dTrace):
				dTracePrev [i] += t
			
		dUpdate = [0.0] * len (self.d_Weights_WT)
		for timestep, r in enumerate (_dReward):
			dTrace = self.d_Trace_WT [timestep]
			for i, t in enumerate (dTrace):
				if 0 != t:
					dUpdate [i] = dUpdate [i] + t * r

		if None == self.d_WeightUpdates_WT:
			self.d_WeightUpdates_WT = [0.0] * len (dUpdate)

		for i, u in enumerate (dUpdate):
			if 0 != u:
				self.d_WeightUpdates_WT [i] = self.d_WeightUpdates_WT [i] + u * self.d_LearningRate





	#															
	def compute_weight_update_a (self, _dReward):
		# compute cumulative update to weights_wn from the 	
		# traces & rewards at each timestep ...				
		for n in xrange (len (self.d_Trace_A) - 1, 0, -1):
			dTracePrev = self.d_Trace_A [n - 1]
			dTrace = self.d_Trace_A [n]
			for i, t in enumerate (dTrace):
				dTracePrev [i] += t

		dUpdate = [0.0] * len (self.d_Weights_A)
		for timestep, r in enumerate (_dReward):
			for i, (u, t) in enumerate (zip (dUpdate, self.d_Trace_A [timestep])):
				dUpdate [i] = u + t * r

		if None == self.d_WeightUpdates_A:
			self.d_WeightUpdates_A = [0.0] * len (dUpdate)

		for i, (w, u) in enumerate (zip (self.d_WeightUpdates_A, dUpdate)):
			self.d_WeightUpdates_A [i] = w + u * self.d_LearningRate






	#															
	def update_weights (self):
		if None != self.d_WeightUpdates_WT:
			for i, (w, u) in enumerate (zip (self.d_Weights_WT, self.d_WeightUpdates_WT)):
				if 0 != u:
					self.d_Weights_WT [i] = w + u

		if None != self.d_WeightUpdates_A:
			for i, (w, u) in enumerate (zip (self.d_Weights_A, self.d_WeightUpdates_A)):
				if 0 != u:
					self.d_Weights_A [i] = w + u


		self.d_WeightUpdates_WT = None
		self.d_WeightUpdates_A = None

		self.d_AverageWeights_WT = 1.0 * sum (self.d_Weights_WT) / len (self.d_Weights_WT)
		self.d_AverageWeights_A = 1.0 * sum (self.d_Weights_A) / len (self.d_Weights_A)



