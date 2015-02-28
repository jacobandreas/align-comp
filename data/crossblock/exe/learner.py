import sys
import os
import random
import bisect
#import socket
import time
import math
import cPickle
import re
import shutil
import copy
from config import *

try:
	import psyco
	psyco.full()
except ImportError:
	pass


map_ObjectClassToIndex = {}
o_Game = None


ARGV = [None, "puzzle.cfg", "exp=test"]


# ----------------------------------------------
class Environment:

	#											
	def __init__ (self, _iCutLength, _iGrid):
		self.i_CutLength = _iCutLength
		self.i_Grid = copy.deepcopy (_iGrid)

	#											
	def get_actions (self):
		# features:											
		# - orientation										
		# - block count										
		# - line perpendicular offset (from both directions, and from middle)
		# - line perpendicular offset (as a location index from both directions)
		# - line offset (both directions, and from middle)	
		lstActions = []


		# actions on rows ...			

		# compute minimum non zero grid	
		iMinNonZero = 1000
		iMaxNonZero = 0
		for iLine in self.i_Grid:
			iNonZeroIndices = map (lambda (i,x): i, filter (lambda (i,x): 0 != x, enumerate (iLine)))
			if 0 == len (iNonZeroIndices):
				continue
			iMinNonZero = min (iMinNonZero, min (iNonZeroIndices))
			iMaxNonZero = max (iMaxNonZero, max (iNonZeroIndices))

		iMinLine = None
		iMaxLine = 0
		for n, iLine in enumerate (self.i_Grid):
			if 0 != sum (iLine):
				if None == iMinLine:
					iMinLine = n
				iMaxLine = n

		iNonZeroGrid = []
		for iLine in self.i_Grid [iMinLine : iMaxLine + 1]:
			iNonZeroGrid.append (iLine [iMinNonZero : iMaxNonZero + 1])


		for n, iLine in enumerate (iNonZeroGrid):
			lstBlocks = map (lambda (i,x): i, filter (lambda (i,x): 0 != x, enumerate (iLine)))
			if 0 == len (lstBlocks):
				continue
			iMinOccupied = min (lstBlocks)
			iMaxOccupied = max (lstBlocks)
			nLocation = n + iMinLine

			iMaxBlocks = len (lstBlocks)
			iSingle = 0
			if iMaxBlocks == self.i_CutLength:
				iSingle = 1
			for i in xrange (iMaxBlocks - self.i_CutLength + 1):
				for j in xrange (i + self.i_CutLength - 1, iMaxBlocks):
					iBlocks = sum (iLine [lstBlocks[i] : lstBlocks[j]+1])
					iBlocksAlt = j - i + 1
					if iBlocksAlt != iBlocks:
						print iBlocks, ' != ' , iBlocksAlt
					assert (iBlocks == iBlocksAlt)
					if iBlocks != self.i_CutLength:
						continue
					sActionID = 'r:' + str(nLocation) + ':' + str(iMinNonZero + lstBlocks [i]) + ':' + str(iMinNonZero + lstBlocks [j])

					iHorizontalFromLeft = 1.0 * (i - iMinOccupied) / (iMaxOccupied - iMinOccupied + 1)
					iHorizontalFromMiddle = 1.0 * ((iMaxOccupied - iMinOccupied + 1) - (i + j)) \
											/ (2 * (iMaxOccupied - iMinOccupied + 1))
					if iHorizontalFromMiddle < 0:
						iHorizontalFromMiddle = -iHorizontalFromMiddle
					iHorizontalFromRight = 1.0 * (iMaxOccupied - j - 1) / (iMaxOccupied - iMinOccupied + 1)

					iVerticalFromTop = 1.0 * n / (iMaxLine - iMinLine + 1)
					iVerticalFromMiddle = 1.0 * (len (iNonZeroGrid)/2 - n) / (iMaxLine - iMinLine + 1)
					if iVerticalFromMiddle < 0:
						iVerticalFromMiddle = -iVerticalFromMiddle
					iVerticalFromBottom = 1.0 * (len (iNonZeroGrid) - n - 1) / (iMaxLine - iMinLine + 1)

					iFromLeft = [0] * 6
					iFromRight = [0] * 6
					iFromTop = [0] * 6
					if n < 6:
						iFromTop [n] = 1
					iFromBottom = [0] * 6
					if len (iNonZeroGrid) - n < 6:
						iFromBottom [len (iNonZeroGrid) - n] = 1

					#lstFeatures = [1, 0]
					lstFeatures = [1, 0, iSingle, \
									iHorizontalFromLeft, iHorizontalFromMiddle, iHorizontalFromRight, \
									iVerticalFromTop, iVerticalFromMiddle, iVerticalFromBottom]
					lstFeatures.extend (iFromLeft)
					lstFeatures.extend (iFromRight)
					lstFeatures.extend (iFromTop)
					lstFeatures.extend (iFromBottom)

					tplAction = [sActionID, lstFeatures]
					
					lstActions.append (tplAction)


		# actions on columns ...		
		iTransposeGrid = []
		for i in xrange (len (self.i_Grid [0])):
			iTransposeGrid.append ([0] * len (self.i_Grid))
		for i, iGrid in enumerate (self.i_Grid):
			for j, value in enumerate (iGrid):
				iTransposeGrid [j][i] = value

		# compute minimum non zero grid	
		iMinNonZero = 1000
		iMaxNonZero = 0
		for iLine in iTransposeGrid:
			iNonZeroIndices = map (lambda (i,x): i, filter (lambda (i,x): 0 != x, enumerate (iLine)))
			if 0 == len (iNonZeroIndices):
				continue
			iMinNonZero = min (iMinNonZero, min (iNonZeroIndices))
			iMaxNonZero = max (iMaxNonZero, max (iNonZeroIndices))

		iMinLine = None
		iMaxLine = 0
		for n, iLine in enumerate (iTransposeGrid):
			if 0 != sum (iLine):
				if None == iMinLine:
					iMinLine = n
				iMaxLine = n

		iNonZeroGrid = []
		for iLine in iTransposeGrid [iMinLine : iMaxLine + 1]:
			iNonZeroGrid.append (iLine [iMinNonZero : iMaxNonZero + 1])



		for n, iLine in enumerate (iNonZeroGrid):
			lstBlocks = map (lambda (i,x): i, filter (lambda (i,x): 0 != x, enumerate (iLine)))
			if 0 == len (lstBlocks):
				continue
			iMinOccupied = min (lstBlocks)
			iMaxOccupied = max (lstBlocks)
			nLocation = n + iMinLine

			iMaxBlocks = len (lstBlocks)
			iSingle = 0
			if iMaxBlocks == self.i_CutLength:
				iSingle = 1
			for i in xrange (iMaxBlocks - self.i_CutLength + 1):
				for j in xrange (i + self.i_CutLength - 1, iMaxBlocks):
					iBlocks = sum (iLine [lstBlocks[i] : lstBlocks[j]+1])
					iBlocksAlt = j - i + 1
					if iBlocksAlt != iBlocks:
						print iBlocks, ' != ' , iBlocksAlt
					assert (iBlocks == iBlocksAlt)
					if iBlocks != self.i_CutLength:
						continue
					sActionID = 'c:' + str(nLocation) + ':' + str(iMinNonZero + lstBlocks [i]) + ':' + str(iMinNonZero + lstBlocks [j])

					iVerticalFromTop = 1.0 * (i - iMinOccupied) / (iMaxOccupied - iMinOccupied + 1)
					iVerticalFromMiddle = 1.0 * ((iMaxOccupied - iMinOccupied + 1) - (i + j)) \
											/ (2 * (iMaxOccupied - iMinOccupied + 1))
					if iVerticalFromMiddle < 0:
						iVerticalFromMiddle = -iVerticalFromMiddle
					iVerticalFromBottom = 1.0 * (iMaxOccupied - j - 1) / (iMaxOccupied - iMinOccupied + 1)

					iHorizontalFromLeft = 1.0 * n / (iMaxLine - iMinLine + 1)
					iHorizontalFromMiddle = 1.0 * (len (iNonZeroGrid)/2 - n) / (iMaxLine - iMinLine + 1)
					if iHorizontalFromMiddle < 0:
						iHorizontalFromMiddle = -iHorizontalFromMiddle
					iHorizontalFromRight = 1.0 * (len (iNonZeroGrid) - n - 1) / (iMaxLine - iMinLine + 1)

					iFromLeft = [0] * 6
					if n < 6:
						iFromLeft [n] = 1
					iFromRight = [0] * 6
					if len (iNonZeroGrid) - n < 6:
						iFromRight [len (iNonZeroGrid) - n] = 1
					iFromTop = [0] * 6
					iFromBottom = [0] * 6

					#lstFeatures = [0, 1]
					lstFeatures = [0, 1, iSingle, \
									iHorizontalFromLeft, iHorizontalFromMiddle, iHorizontalFromRight, \
									iVerticalFromTop, iVerticalFromMiddle, iVerticalFromBottom]
					lstFeatures.extend (iFromLeft)
					lstFeatures.extend (iFromRight)
					lstFeatures.extend (iFromTop)
					lstFeatures.extend (iFromBottom)

					tplAction = [sActionID, lstFeatures]
					
					lstActions.append (tplAction)

		return lstActions


	#											
	def perform_action (self, _sAction):
		(sOrientation, sColumn, sStart, sEnd) = _sAction.split (':')
		iColumn = int (sColumn)
		iStart = int (sStart)
		iEnd = int (sEnd)

		if 'r' == sOrientation [0]:
			iBlocks = 0
			for i in xrange (iStart, iEnd + 1):
				iBlocks += self.i_Grid [iColumn][i]
			if iBlocks != self.i_CutLength:
				print 'row blocks : ' + str (iBlocks) + '  !=  req_cut : ' + str (self.i_CutLength)
				return None
			for i in xrange (iStart, iEnd + 1):
				self.i_Grid [iColumn][i] = 0
			return reduce (lambda x,y: x+y, map (lambda iGrid: reduce (lambda a,b: a+b, iGrid), self.i_Grid))

		if 'c' == sOrientation [0]:
			iBlocks = 0
			for i in xrange (iStart, iEnd + 1):
				iBlocks += self.i_Grid [i][iColumn]
			if iBlocks != self.i_CutLength:
				print 'column blocks : ' + str (iBlocks) + '  !=  req_cut : ' + str (self.i_CutLength)
				return None
			for i in xrange (iStart, iEnd + 1):
				self.i_Grid [i][iColumn] = 0
			return reduce (lambda x,y: x+y, map (lambda iGrid: reduce (lambda a,b: a+b, iGrid), self.i_Grid))

		assert False


	#											
	def print_state (self):
		return '\n'.join (map (lambda y: 
						''.join (map (lambda x: str(x), y)).replace ('0', '  ').replace ('1', ' #').replace ('2', ' -'), 
						self.i_Grid))



# ----------------------------------------------
class Game:

	#											
	def __init__ (self):
		self.map_IdToGameConfig = {}
		self.i_MaxWidth = None
		self.i_MaxHeight = None
		self.s_CurrentId = None
		self.o_CurrentEnv = None


	#											
	def load_game (self):
		file = open (get_config ('game_data_file'))
		lstLines = map (lambda x: x.strip (), file.readlines ())
		file.close ()

		self.i_MaxWidth = 0
		self.i_MaxHeight = 0
		sID = None
		iCutLength = None
		iGrid = []
		for sLine in lstLines:
			if '' == sLine:
				if None != sID:
					self.map_IdToGameConfig [sID] = (iCutLength, iGrid)
					if self.i_MaxHeight < len (iGrid):
						self.i_MaxHeight = len (iGrid)
					sID = None
					iCutLength = None
					iGrid = []
				continue

			if True == sLine.isdigit ():
				sID = sLine
				continue
			if ':' == sLine [0]:
				iCutLength = int (sLine [1:])
				continue

			sLine = sLine.replace ('.', '0').replace ('#', '1').replace (' ', '')
			iGrid.append (map (lambda x: int (x), sLine))
			if self.i_MaxWidth < len (sLine):
				self.i_MaxWidth = len (sLine)

		if None != sID:
			self.map_IdToGameConfig [sID] = (iCutLength, iGrid)
	


	#											
	def start_game (self, _sID):
		if None == self.map_IdToGameConfig.get (_sID, None):
			return False

		(iCutLength, iGrid) = self.map_IdToGameConfig [_sID]
		self.o_CurrentEnv = Environment (iCutLength, iGrid)
		return True


	#											
	def get_state (self):
		return self.o_CurrentEnv.get_actions ()


	#											
	def act (self, _sAction):
		return self.o_CurrentEnv.perform_action (_sAction)


	#											
	def print_game_state (self):
		return self.o_CurrentEnv.print_state ()


	#											
	def print_all_games (self):
		lstID = self.map_IdToGameConfig.keys ()
		lstID.sort ()
		for sID in lstID:
			print sID
			self.start_game (sID)
			self.print_game_state ()
			print



# ----------------------------------------------
def test_game ():
	load_config (ARGV)
	o_Game = Game ()
	o_Game.load_game ()
	#o_Game.print_all_games ()

	o_Game.start_game ('27')
	o_Game.print_game_state ()
	lstActions = o_Game.get_state ()
	print '\n'.join (map (lambda x: str(x), lstActions))

	for sAction in lstActions:
		print '--------------------'
		print sAction
		o_Game.start_game ('27')
		if None == o_Game.act (sAction[0]):
			print 'ERROR'
		o_Game.print_game_state ()


if __name__ == '__main__':
	sys.exit (test_game ())





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




map_Config = {}


#																
def expand_value (_sValue):
	global map_Config

	iStart = _sValue.find ('${')
	while -1 != iStart:
		iEnd = _sValue.find ('}', iStart)
		sKey = _sValue [iStart + 2 : iEnd]
		sConfig = map_Config.get (sKey, None)
		if None != sConfig:
			_sValue = _sValue [:iStart] + sConfig + _sValue [iEnd + 1:]
		iStart = _sValue.find ('${', iEnd)

	return _sValue
	

#																
def load_config (_lstArgs):
	global map_Config
	map_Config = {}

	sFileName = None
	mapRawConfig = {}
	for sArg in _lstArgs [1:]:
		if '=' not in sArg:
			sFileName = sArg.strip ()
			continue

		(sKey, sValue) = sArg.split ('=')
		sKey = sKey.strip ()
		sValue = sValue.strip ()
		if '${' not in sValue:
			map_Config [sKey] = sValue
		else:
			mapRawConfig [sKey] = sValue



	lstLines = []
	if None != sFileName:
		file = open (sFileName)
		lstLines = file.readlines ()
		file.close ()

	for sLine in lstLines:
		sLine = sLine.strip ()
		if '' == sLine:
			continue
		if '#' == sLine [0]:
			continue
		(sKey, sValue) = sLine.split ('=')
		sKey = sKey.strip ()
		sValue = sValue.strip ()
		if '${' not in sValue:
			map_Config [sKey] = sValue
		else:
			mapRawConfig [sKey] = sValue

	while len (mapRawConfig) > 0:
		for sKey, sValue in mapRawConfig.items ():
			sExpandedValue = expand_value (sValue)
			if '${' not in sExpandedValue:
				map_Config [sKey] = sExpandedValue
				del mapRawConfig [sKey]
			else:
				mapRawConfig [sKey] = sExpandedValue


#																
def get_config (_sConfig):
	global map_Config

	sValue = map_Config.get (_sConfig, None)
	if None == sValue:
		print "[ERROR] '" + _sConfig + "' not found in config."
		return ''
	return sValue


#																
def get_config_list (_sConfig):
	sList = get_config (_sConfig)
	if '' == sList:
		return []

	lst = []
	for x in sList.split ('|'):
		lst.append (x.strip ())
	
	return lst

map_IdToAction = {}


#																	
def measure_state ():
	global map_IdToAction

	lstState = o_Game.get_state ()

	lstColumns = []
	lstRows = []
	
	for oAction in lstState:
		(sID, lstFeatures) = oAction
		lstFeatures.extend ([1, 0, 0, 0, 0])
		if 0 == lstFeatures [2]:
			continue
		if 1 == lstFeatures [0]:
			lstRows.append (oAction)
		elif 1 == lstFeatures [1]:
			lstColumns.append (oAction)
		else:
			assert False

	if len (lstRows) > 1:
		for i in xrange (len (lstRows) - 1):
			for j in xrange (i + 1, len (lstRows)):
				lstFeatures = map (lambda (x,y): 0.5 * (x+y), zip (lstRows[i][1], lstRows[j][1]))
				lstFeatures [-5] = 0
				lstFeatures [-4] = 1
				sID = 'mr2:' + str(i) + '-' + str(j)
				lstState.append ([sID, lstFeatures])
				map_IdToAction [sID] = [lstRows[i][0], lstRows[j][0]]


	if len (lstRows) > 2:
		for i in xrange (len (lstRows) - 2):
			for j in xrange (i + 1, len (lstRows) - 1):
				for k in xrange (j + 1, len (lstRows)):
					lstFeatures = map (lambda (x,y,z): 1.0 * (x+y+z)/3, zip (lstRows[i][1], lstRows[j][1], lstRows[k][1]))
					lstFeatures [-5] = 0
					lstFeatures [-3] = 1
					sID = 'mr3:' + str(i) + '-' + str(j) + '-' + str(k)
					lstState.append ([sID, lstFeatures])
					map_IdToAction [sID] = [lstRows[i][0], lstRows[j][0], lstRows[k][0]]


	if len (lstRows) > 3:
		for i in xrange (len (lstRows) - 3):
			for j in xrange (i + 1, len (lstRows) - 2):
				for k in xrange (j + 1, len (lstRows) - 1):
					for l in xrange (k + 1, len (lstRows)):

						lstFeatures = map (lambda (w,x,y,z): 0.25 * (w+x+y+z), zip (lstRows[i][1], lstRows[j][1], lstRows[k][1], lstRows[l][1]))
						lstFeatures [-5] = 0
						lstFeatures [-2] = 1
						sID = 'mr4:' + str(i) + '-' + str(j) + '-' + str(k) + '-' + str(l)
						lstState.append ([sID, lstFeatures])
						map_IdToAction [sID] = [lstRows[i][0], lstRows[j][0], lstRows[k][0], lstRows[l][0]]

	if len (lstRows) > 4:
		for i in xrange (len (lstRows) - 4):
			for j in xrange (i + 1, len (lstRows) - 3):
				for k in xrange (j + 1, len (lstRows) - 2):
					for l in xrange (k + 1, len (lstRows) - 1):
						for m in xrange (l + 1, len (lstRows)):

							lstFeatures = map (lambda (v,w,x,y,z): 0.2 * (v+w+x+y+z), zip (lstRows[i][1], lstRows[j][1], lstRows[k][1], lstRows[l][1], lstRows[m][1]))
							lstFeatures [-5] = 0
							lstFeatures [-1] = 1
							sID = 'mr5:' + str(i) + '-' + str(j) + '-' + str(k) + '-' + str(l) + '-' + str(m)
							lstState.append ([sID, lstFeatures])
							map_IdToAction [sID] = [lstRows[i][0], lstRows[j][0], lstRows[k][0], lstRows[l][0], lstRows[m][0]]




	if len (lstColumns) > 1:
		for i in xrange (len (lstColumns) - 1):
			for j in xrange (i + 1, len (lstColumns)):

				lstFeatures = map (lambda (x,y): 0.5 * (x+y), zip (lstColumns[i][1], lstColumns[j][1]))
				lstFeatures [-5] = 0
				lstFeatures [-4] = 1
				sID = 'mc2:' + str(i) + '-' + str(j)
				lstState.append ([sID, lstFeatures])
				map_IdToAction [sID] = [lstColumns[i][0], lstColumns[j][0]]


	if len (lstColumns) > 2:
		for i in xrange (len (lstColumns) - 2):
			for j in xrange (i + 1, len (lstColumns) - 1):
				for k in xrange (j + 1, len (lstColumns)):

					lstFeatures = map (lambda (x,y,z): 1.0 * (x+y+z)/3, zip (lstColumns[i][1], lstColumns[j][1], lstColumns[k][1]))
					lstFeatures [-5] = 0
					lstFeatures [-3] = 1
					sID = 'mc3:' + str(i) + '-' + str(j) + '-' + str(k)
					lstState.append ([sID, lstFeatures])
					map_IdToAction [sID] = [lstColumns[i][0], lstColumns[j][0], lstColumns[k][0]]


	if len (lstColumns) > 3:
		for i in xrange (len (lstColumns) - 3):
			for j in xrange (i + 1, len (lstColumns) - 2):
				for k in xrange (j + 1, len (lstColumns) - 1):
					for l in xrange (k + 1, len (lstColumns)):

						lstFeatures = map (lambda (w,x,y,z): 0.25 * (w+x+y+z), zip (lstColumns[i][1], lstColumns[j][1], lstColumns[k][1], lstColumns[l][1]))
						lstFeatures [-5] = 0
						lstFeatures [-2] = 1
						sID = 'mc4:' + str(i) + '-' + str(j) + '-' + str(k) + '-' + str(l)
						lstState.append ([sID, lstFeatures])
						map_IdToAction [sID] = [lstColumns[i][0], lstColumns[j][0], lstColumns[k][0], lstColumns[l][0]]

	if len (lstColumns) > 4:
		for i in xrange (len (lstColumns) - 4):
			for j in xrange (i + 1, len (lstColumns) - 3):
				for k in xrange (j + 1, len (lstColumns) - 2):
					for l in xrange (k + 1, len (lstColumns) - 1):
						for m in xrange (l + 1, len (lstColumns)):

							lstFeatures = map (lambda (v,w,x,y,z): 0.2 * (v+w+x+y+z), zip (lstColumns[i][1], lstColumns[j][1], lstColumns[k][1], lstColumns[l][1], lstColumns[m][1]))
							lstFeatures [-5] = 0
							lstFeatures [-1] = 1
							sID = 'mc5:' + str(i) + '-' + str(j) + '-' + str(k) + '-' + str(l) + '-' + str(m)
							lstState.append ([sID, lstFeatures])
							map_IdToAction [sID] = [lstColumns[i][0], lstColumns[j][0], lstColumns[k][0], lstColumns[l][0], lstColumns[m][0]]



	return (lstState, (0 != len (lstState)))



#																	
def send_action (_sAction):
	global map_IdToAction
	lstActions = map_IdToAction.get (_sAction, None)
	if None == lstActions:
		return o_Game.act (_sAction)
	
	oReturn = None
	for sAction in lstActions:
		oReturn = o_Game.act (sAction)
	return oReturn


#																	
def reset_environment_to_snapshot (_sRequiredStartingEnvState):
	o_Game.start_game (_sRequiredStartingEnvState)







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
	load_config (ARGV)
	shutil.copyfile (ARGV [1], get_config ('output_path') + '/run.cfg')
	random.seed ()

	i_TotalArticles = load_articles ()
	load_heldout_articles ()
	learner = Learner ()
	learner.init_learner ()
	if '1' == get_config ('use_partial_annotation'):
		learner.load_partial_annotation ()

	bStartAfresh = ('restart' in ARGV)
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
