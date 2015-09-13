#!/usr/bin/python

import sys
from lxml import etree
import json
import re
from time import ctime

only_regions=["slack","total","divref0","divref1","divref2","divsrc0","divsrc1","divsrc2","divtrn0"]
macro_regions=["divref","divsrc","divtrn"]

regions=["slack","total"]+only_regions
all_transitions=[]
for r1 in macro_regions:
	for r2 in macro_regions:
		all_transitions.append(r1+"-"+r2)

params_task=['q_type','len_type','user','expUserID','game_type','usr_type','duration','id']
params_game=['game_id','slang','tlang']

def usage():
	print "Usage: "+ sys.argv[0]+" <Results file>"
	exit(0)

def parseduration(string):
	(h,m,s)=[float(x) for x in string.split(":")]
	return "%0.2f"%(h*3600+m*60+s)

def parse(filename):
	xml=etree.parse(filename)

	printtitles()
	for game in xml.findall('eyetracking-game-result'):
		atts=game.attrib

		try:
			slang=atts['source-language']
			tlang=atts['target-language']
			game_id=atts['id']
			#user_id=atts['userID']
			#game_type=atts['game_type']
		except KeyError:
			print "Found 1 instance that do not conform to new format"
			continue

		for task in game.findall('quality-checking-item'):
			atts=task.attrib
			params={x:task.attrib[x] for x in  params_task }
			params['slang']=slang
			params['game_id']=game_id
			params['tlang']=tlang
			params['duration']=parseduration(params['duration'])

			#assert (game_type ==atts['game_type'])
			#assert (user_id== atts['expUserID'])

			results=atts['result']
			

			results=json.loads(( results).replace("'","\""))
			
		

			params['score']=results["score"]
			#print params["score"]

			params['aggregate']={key: 0.0 for key in regions}
			params['chain']=[]
			unique_times={}
			gazes=[["start",0,0]]
			#print results["Eye"][0]["Region"]
			for result in results["Eyedata"]:
				if result == {}:
					continue
				try:
					#we only care about words
					if len(result["Region"].split("_"))!=2: 
						continue
					if gazes[-1][0] != result["Region"]:
						gazes.append([result["Region"],result["time"],result["time"]])
					else:
						gazes[-1][2] =result["time"]
			
				except KeyError:
					print result
					exit(0)

			fixations=[["start",0]]
			for word,frame_start,frame_end in gazes:
				duration=float(frame_end)-float(frame_start)
				if duration >=200:
					fixations.append([word.split("_")[0],duration/1000])

			fixations.append(["end",0])

			for idx, data in enumerate(fixations[:-1]):
				word=data[0]
				time=data[1]
				next_word=fixations[idx+1][0]
				params['chain'].append([word,time,next_word])

			#print params['chain']
			prettyprint(params)


def printtitles (  ):

	
#	print regions
	print( "\t".join(params_task) + "\t"+ \
	 "\t".join( params_game) + "\t" + \
	 "score\tseq\tregion\tregion2\ttime")
def prettyprint ( par ):

	
#	print regions
	for id in range(len(par['chain'])):
		chain=par['chain'][id]
		print( "\t".join([par[x] for x in params_task]) + "\t"+ \
		 "\t".join([par[x] for x in params_game]) + "\t" + \
		 "%s\t%d\t%s\t%s\t%f"%(par["score"],id,chain[0],chain[2],chain[1]) )
	 
	#exit

def main (argv=None):
	if argv is None:
	 argv = sys.argv

	if len(argv) !=2:
		usage()

	parse(sys.argv[1])
	return 0;


if __name__ == "__main__":
	main()
