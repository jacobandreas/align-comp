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





