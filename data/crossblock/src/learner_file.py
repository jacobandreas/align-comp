


	#																
	def load_vocabulary (self):
		print '      loading vocabulary ...'
		sFileName = map_Config.get ('vocabulary_file', None)
		if None == sFileName:
			print "[ERROR] 'vocabulary_file' not specified in config."
			return False

		file = open (sFileName)
		lstVocabulary = file.readlines ()
		file.close ()
		for i, word in enumerate (lstVocabulary):
			self.map_WordToIndex [word.strip ()] = i
		self.i_VocabularySize = len (self.map_WordToIndex)
		return True



	#																
	def load_weights_from_file (self, _sConfigName):
		sFileName = map_Config.get (_sConfigName, None)
		if None == sFileName:
			print "[ERROR] '" + _sConfigName + "' not specified in config."
			return None

		if True == os.access (sFileName, os.R_OK):
			# these files can get pretty big, so we need to do some fancy reading...
			iLines = 0
			iTotalLength = 0

			file = open (sFileName)
			sLastLine = ''
			sLine = file.readline ()
			while '' != sLine:
				if '' == sLine.strip ():
					sLine = file.readline ()
					continue
				iLines = iLines + 1
				iTotalLength = iTotalLength + len (sLine)
				sLastLine = sLine
				if iLines >= 10:
					break
				sLine = file.readline ()

			if '' != file.readline ():
				# we didn't get to the end of the file while reading the first few lines.
				file.seek (- iTotalLength, 2)

				sLastLine = ''
				sLine = file.readline ()
				while '' != sLine:
					if '' == sLine.strip ():
						sLine = file.readline ()
						continue
					iLines = iLines + 1
					sLastLine = sLine
					sLine = file.readline ()

			file.close ()

			if 0 == iLines:
				print "[WARNING] '" + sFileName + "' has 0 lines."
				return None

			lstWeights = []
			for sValue in sLastLine.strip ().split (','):
				lstWeights.append (float (sValue))
			return lstWeights



	#																
	def load_weights (self):
		print '      loading weights ...'

		dWeights = self.load_weights_from_file ('terminator_word_weights_file')
		if None != dWeights:
			self.d_Weights_WT = dWeights
		dWeights = self.load_weights_from_file ('action_weights_file')
		if None != dWeights:
			self.d_Weights_A = dWeights

		self.d_AverageWeights_WT = 1.0 * sum (self.d_Weights_WT) / len (self.d_Weights_WT)
		self.d_AverageWeights_A = 1.0 * sum (self.d_Weights_A) / len (self.d_Weights_A)

		return True



	#																
	def save_weights_to_file (self, _sConfigName, _dWeights):
		sFileName = map_Config.get (_sConfigName, None)
		if None == sFileName:
			print "[ERROR] '" + _sConfigName + "' not specified in config."
			return False

		file = open (sFileName, 'a')
		lst = []
		for x in _dWeights:
			lst.append (str (x))
		file.write (', '.join (lst) + '\n')
		file.flush ()
		file.close ()



	#																
	def save_weights (self):
		self.save_weights_to_file ('terminator_word_weights_file', self.d_Weights_WT)
		self.save_weights_to_file ('action_weights_file', self.d_Weights_A)

		return True



	#																
	def clear_weight_file (self, _sConfigName):
		sFileName = map_Config.get (_sConfigName, None)
		if None == sFileName:
			print "[ERROR] '" + _sConfigName + "' not specified in config."
			return False

		file = open (sFileName, 'w')
		file.write ('')
		file.flush ()
		file.close ()



	#																
	def clear_weight_files (self):
		self.clear_weight_file ('terminator_word_weights_file')
		self.clear_weight_file ('action_weights_file')

		return True


