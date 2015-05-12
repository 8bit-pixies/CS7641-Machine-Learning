# -*- coding: utf-8 -*-
"""
Created on Wed Mar 11 20:38:38 2015

@author: Chapman
"""

# -*- coding: utf-8 -*-
"""
Created on Wed Mar 11 20:03:39 2015

@author: Chapman
"""

import subprocess
from os import environ
env = environ.copy() 
env['CLASSPATH'] = "C:\\Users\\Chapman\\Documents\\GATECH\\ML-7641\\Assignment2\\ABAGAIL\\out\\artifacts\\ABAGAIL_jar\\ABAGAIL.jar"
env['JYTHON_HOME'] = 'C:\\jython2.5.3'
env['PATH'] = "%s;C:\\jython2.5.3;C:\\jython2.5.3\\bin" % env['PATH']

# generations = 1500

for N in [40, 60, 80, 100, 120, 140]:
    for samples in [40, 60, 80, 100, 120, 140]:
        for tokeep in [0.2, 0.5, 0.7, .9]:
            cmd = "jython knapsack_mimic.py %d %d %d" % (N, samples, int(samples*tokeep))
            print(cmd)
            subprocess.call(cmd, env=env, shell=True)

