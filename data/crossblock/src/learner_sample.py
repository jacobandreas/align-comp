


	# 															
	def sample_terminator (self):
		self.compute_p_wt ()
		self.i_SelectedTerminatorWordLocation = self.sample_from_pdf (self.d_Prob_WT)


	# 
	def sample_action (self):
		self.compute_p_a ()
		self.i_SelectedAction = self.sample_from_pdf (self.d_Prob_A)



	#															
	def compute_p_wt (self):

		self.d_Features_WT = [None] * self.i_CurrentInstructionLength
		self.d_Prob_WT = [None] * self.i_CurrentInstructionLength

		iCount = 0
		dNormalization = 0.0
		for lwt in self.set_UnusedWordLocations:
			p = 0
			if lwt > self.i_FirstUnusedWordLocation:
				p = math.exp (self.compute_score_wt (lwt))
			self.d_Prob_WT [lwt] = p
			dNormalization += p
			iCount += 1

		# normalization 
		if 0 != dNormalization:
			for i, p in enumerate (self.d_Prob_WT):
				if None == p:
					continue
				self.d_Prob_WT [i] = 1.0 * p / dNormalization
		else:
			for i, p in enumerate (self.d_Prob_WT):
				if None == p:
					continue
				self.d_Prob_WT [i] = 1.0 / iCount

	


	#															
	def compute_p_a (self):

		self.d_Features_A = [None] * self.i_CurrentStateObjectCount
		self.d_Prob_A = [None] * self.i_CurrentStateObjectCount

		iCount = 0
		dNormalization = 0.0
		for a in xrange (self.i_CurrentStateObjectCount):
			p = math.exp (self.compute_score_a (a))
			self.d_Prob_A [a] = p
			dNormalization += p
			iCount += 1

		# normalization 
		if 0 != dNormalization:
			for i, p in enumerate (self.d_Prob_A):
				if None == p:
					continue
				self.d_Prob_A [i] = 1.0 * p / dNormalization
		else:
			for i, p in enumerate (self.d_Prob_A):
				if None == p:
					continue
				self.d_Prob_A [i] = 1.0 / iCount






	# 															
	def sample_from_cdf (self, _dCDF, _dPDF):
		assert (_dCDF [-1] >= _dPDF [-1])

		dUniform = random.uniform (0, _dCDF [-1])
		if dUniform == _dCDF [-1]:
			return len (_dCDF) - 1
		else:
			iSample = bisect.bisect_left (_dCDF, dUniform)

			# if we have 0's in _dPDF, the bisect above might return
			# an iSample where _dPDF [iSample] = 0!					
			# the code below is to handle this case...				
			iActualSample = iSample
			while 0 == _dPDF [iActualSample]:
				iActualSample = iActualSample + 1
				if iActualSample == len (_dPDF):
					iActualSample = None
					break
			if None == iActualSample:
				iActualSample = iSample
				while 0 == _dPDF [iActualSample]:
					iActualSample = iActualSample - 1
					if iActualSample < 0:
						iActualSample = None
						break

			if None == iActualSample:
				print '[ERROR] failed to sample from pdf.'
				file_InteractionLog.write ('[ERROR] failed to sample from pdf.\n')
				file_InteractionLog.write ('pdf : ' +  str (_dPDF) + '\n\n')
				file_InteractionLog.write ('cdf : ' + str (_dCDF) + '\n\n')
				file_InteractionLog.write ('original sample : ' + str (iSample) + '\n')

			return iActualSample





	#															
	def sample_from_pdf (self, _dPDF):

		# if we are in test mode, we always take the max probability action ...
		if True == self.b_TestMode:
			dMaxProb = max (_dPDF)

			dCDF = [0] * len (_dPDF)
			dPDF = [0] * len (_dPDF)
			if (None != _dPDF [0]) and (_dPDF [0] == dMaxProb):
				dCDF [0] = 1
				dPDF [0] = 1
			for i in xrange (1, len (_dPDF)):
				if (None == _dPDF [i]) or (_dPDF [i] < dMaxProb):
					dCDF [i] = dCDF [i-1]
				else:
					dCDF [i] = 1 + dCDF [i-1]
					dPDF [i] = 1

			iSample = self.sample_from_cdf (dCDF, dPDF)
			assert (0 != dPDF [iSample])
			assert (None != _dPDF [iSample])
			return iSample


		else:
			# If we are in training mode, we have to randomly sample.		
			# There are two ways of doing this:								
			#   1. The \epsilon greedy method								
			#   2. Sampling from a smoothed pdf								

			if True == self.b_EpsilonGreedy:
				# 1. \epsilog greedy sampling.								
				#    With probability \epsilon, we sample from a uniform	
				#    distribution, otherwise we take the max prob action.	
				dUniform = random.uniform (0, 1)

				if dUniform > self.d_SamplingEpsilon:
					# greedy sampling - find max prob, and uniformly	
					# sample from all cases equal to max prob...		

					dCDF = list (_dPDF)
					if None == dCDF [0]:
						dCDF [0] = 0
					for i in xrange (1, len (dCDF)):
						if None == dCDF [i]:
							dCDF [i] = 0
						dCDF [i] = dCDF [i] + dCDF [i-1]

					iSample = self.sample_from_cdf (dCDF, _dPDF)
					assert (None != _dPDF [iSample])
					return iSample


				else:
					# uniform sampling ...
					dCDF = [0] * len (_dPDF)
					dPDF = [0] * len (_dPDF)
					if None != _dPDF [0]:
						dCDF [0] = 1
						dPDF [0] = 1
					for i in xrange (1, len (_dPDF)):
						if None == _dPDF [i]:
							dCDF [i] = dCDF [i-1]
						else:
							dCDF [i] = 1 + dCDF [i-1]
							dPDF [i] = 1

					iSample = self.sample_from_cdf (dCDF, dPDF)
					assert (0 != dPDF [iSample])
					assert (None != _dPDF [iSample])
					return iSample


			else:
				# 2. Sampling from a smoothed pdf.  The pdf given to us	
				#    has already been smoothed, so we need to just 		
				#    sample from it...									
				dCDF = list (_dPDF)
				if None == dCDF [0]:
					dCDF [0] = 0
				for i in xrange (1, len (dCDF)):
					if None == dCDF [i]:
						dCDF [i] = 0
					dCDF [i] = dCDF [i] + dCDF [i-1]

				iSample = self.sample_from_cdf (dCDF, _dPDF)
				assert (None != _dPDF [iSample])
				return iSample



