
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

