import sys, os



def main ():
	load_config (sys.argv)

	file = open (get_config ())
	lstLines = file.readlines ()
	file.close ()

	lst_IterationToReward = []
	iIteration = None

	for sLine in lstLines:
		if 'article' in sLine:
			iCurrent = int (sLine.split (':')[0])
			if iCurrent != iIteration:
				lst_IterationToReward.append (0)
			iIteration = iCurrent
			continue

		if 'reward : ' in sLine:
			sReward = sLine.strip ().replace ('reward : [', '').replace (']', '').strip ()
			if '' == sReward:
				continue
			lstReward = map (lambda x: float (x.replace ("'", '').strip ()), sReward.split (','))
			dReward = sum (lstReward)
			lst_IterationToReward [iIteration] += dReward
			continue


	file = open ('/tmp/reward.grp', 'w')
	file.write ('\n'.join (map (lambda (i,x): str(i) + ' ' + str(x), lst_IterationToReward)) + '\n')
	file.close ()

	sCommand = 'graph -C -T ps -g 3 '
	sCommand += '--frame-color grey /tmp/reward.grp '
	sCommand +=	'> /tmp/reward.ps'

	os.system (sCommand)




if __name__ == '__main__':
	sys.exit (main ())
