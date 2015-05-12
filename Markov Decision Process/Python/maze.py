# -*- coding: utf-8 -*-
"""
Created on Tue Apr  7 22:23:59 2015

@author: Chapman
"""

import numpy
from numpy.random import random_integers as rand
import random
import re
 
def maze(size, complexity=.75, density=.75):
    # Only odd shapes
    height=size
    width=size
    shape = ((height // 2) * 2 + 1, (width // 2) * 2 + 1)
    # Adjust complexity and density relative to maze size
    complexity = int(complexity * (5 * (shape[0] + shape[1])))
    density    = int(density * (shape[0] // 2 * shape[1] // 2))
    # Build actual maze
    Z = numpy.zeros(shape, dtype=int)
    # Fill borders
    Z[0, :] = Z[-1, :] = 1
    Z[:, 0] = Z[:, -1] = 1
    # Make aisles
    for i in range(density):
        x, y = rand(0, shape[1] // 2) * 2, rand(0, shape[0] // 2) * 2
        Z[y, x] = 1
        for j in range(complexity):
            neighbours = []
            if x > 1:             neighbours.append((y, x - 2))
            if x < shape[1] - 2:  neighbours.append((y, x + 2))
            if y > 1:             neighbours.append((y - 2, x))
            if y < shape[0] - 2:  neighbours.append((y + 2, x))
            if len(neighbours):
                y_,x_ = neighbours[rand(0, len(neighbours) - 1)]
                if Z[y_, x_] == 0:
                    Z[y_, x_] = 1
                    Z[y_ + (y - y_) // 2, x_ + (x - x_) // 2] = 1
                    x, y = x_, y_
                    
                    
                    
    # remove deadends...
    # we want to iterate...from range(1,shape1)
    for y in range(shape[0]):
        for x in range(shape[1]):
            neighbours = []
            if Z[(y,x)] == 1:continue
            if x > 1 and x < shape[1] -2 and y > 1 and y < shape[0] -2: 
                # we can check all directions.
                if Z[(y,x-1)] ==1:
                    neighbours.append((y,x-1))
                if Z[(y,x+1)] ==1:
                    neighbours.append((y,x+1))
                if Z[(y-1,x)] ==1:
                    neighbours.append((y-1,x))
                if Z[(y+1,x)] ==1:
                    neighbours.append((y+1,x))
                if len(neighbours)>2:
                    for xx,yy in random.sample(neighbours, len(neighbours)-2):
                        Z[(xx,yy)] = 0 
            # otherwise we need to be a bit more careful...
            else:
                if Z[(y,x-1)] ==1:
                    if (x-1) == 0: continue
                    neighbours.append((y,x-1))
                if Z[(y,x+1)] ==1:
                    if (x+1) == (shape[1]-1): continue
                    neighbours.append((y,x+1))
                if Z[(y-1,x)] ==1:
                    if (y-1) == 0:continue
                    neighbours.append((y-1,x))
                if Z[(y+1,x)] ==1:
                    if (y+1) == (shape[0]-1): continue
                    neighbours.append((y+1,x))                
                if len(neighbours) > 0:
                    for xx,yy in random.sample(neighbours, len(neighbours)-1):
                        Z[(xx,yy)] = 0 
            # corner cases...
            #chekc....the four corners...manually?
            # top left
            Z[(1,1)] = 0
            Z[(1,2)] = 0
            Z[(2,1)] = 0
            
            # bottom left...
            Z[(shape[0]-2,1)] = 0
            Z[(shape[0]-2,2)] = 0
            Z[(shape[0]-3,1)] = 0
            
            # bottom right
            Z[(shape[0]-2,shape[1]-2)] = 0
            Z[(shape[0]-2,shape[1]-3)] = 0
            Z[(shape[0]-3,shape[1]-2)] = 0
            
            # top right
            Z[(1,shape[1]-2)] = 0
            Z[(1,shape[1]-3)] = 0
            Z[(2,shape[1]-2)] = 0

    return Z
    
# square mazes only...
#y = maze(10)
#z = y.tolist()
#z = ';\n'.join([''.join(["-%d\t" % x for x in row]) for row in z])


def write_maze(sz, fname):
    fout = open(fname +".m", 'w')
    z = maze(sz).tolist()
    z = ';\n'.join([''.join(["%d\t" % x for x in row]) for row in z])
    
    z = re.sub(r'[1]', '-1', z)
    z = re.sub(r'0', '1', z, count=1)
    z = re.sub(r'0', '1', z[::-1], count=1)[::-1]    
    
    mtfile = """
function [ G ] = %s(  )

G = [  
%s
];
    
end
""" % (fname, z)
    
    fout.write(mtfile)

for i in range(20):
    if i % 2 ==0 : continue
    write_maze(i, 'grid_%d' % i)

write_maze(50, 'grid_50')