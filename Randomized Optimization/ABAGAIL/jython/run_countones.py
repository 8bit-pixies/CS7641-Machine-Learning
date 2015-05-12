import subprocess
from os import environ
env = environ.copy() 
env['CLASSPATH'] = "C:\\Users\\Chapman\\Documents\\GATECH\\ML-7641\\Assignment2\\ABAGAIL\\out\\artifacts\\ABAGAIL_jar\\ABAGAIL.jar"
env['JYTHON_HOME'] = 'C:\\jython2.5.3'
env['PATH'] = "%s;C:\\jython2.5.3;C:\\jython2.5.3\\bin" % env['PATH']


    
for x in [40, 60, 80, 100, 120, 140]:
    cmd = "jython countones.py %d" % (x)
    print(cmd)
    subprocess.call(cmd, env=env, shell=True)

