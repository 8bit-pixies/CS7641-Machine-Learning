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

for N in [20]:
    for samples in [50,100,200]:
        for tokeep in [0.05, 0.1, 0.25, 0.5]:
            cmd = "jython fourpeaks_mimic_tuned.py %d %d %d" % (N, samples, int(samples*tokeep))
            print(cmd)
            subprocess.call(cmd, env=env, shell=True)

