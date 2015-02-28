import sys, os, random, bisect, socket, time, math, cPickle, re, shutil, copy
from config import *

try:
	import psyco
	psyco.full()
except ImportError:
	pass


map_ObjectClassToIndex = {}
o_Game = None




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
	load_config (sys.argv)
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



