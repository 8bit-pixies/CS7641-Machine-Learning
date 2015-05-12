import sys
import os
import time

import java.io.FileReader as FileReader
import java.io.File as File
import java.lang.String as String
import java.lang.StringBuffer as StringBuffer
import java.lang.Boolean as Boolean
import java.util.Random as Random

import dist.DiscreteDependencyTree as DiscreteDependencyTree
import dist.DiscreteUniformDistribution as DiscreteUniformDistribution
import dist.Distribution as Distribution
import opt.DiscreteChangeOneNeighbor as DiscreteChangeOneNeighbor
import opt.EvaluationFunction as EvaluationFunction
import opt.GenericHillClimbingProblem as GenericHillClimbingProblem
import opt.HillClimbingProblem as HillClimbingProblem
import opt.NeighborFunction as NeighborFunction
import opt.RandomizedHillClimbing as RandomizedHillClimbing
import opt.SimulatedAnnealing as SimulatedAnnealing
import opt.example.FourPeaksEvaluationFunction as FourPeaksEvaluationFunction
import opt.ga.CrossoverFunction as CrossoverFunction
import opt.ga.SingleCrossOver as SingleCrossOver
import opt.ga.DiscreteChangeOneMutation as DiscreteChangeOneMutation
import opt.ga.GenericGeneticAlgorithmProblem as GenericGeneticAlgorithmProblem
import opt.ga.GeneticAlgorithmProblem as GeneticAlgorithmProblem
import opt.ga.MutationFunction as MutationFunction
import opt.ga.StandardGeneticAlgorithm as StandardGeneticAlgorithm
import opt.ga.UniformCrossOver as UniformCrossOver
import opt.prob.GenericProbabilisticOptimizationProblem as GenericProbabilisticOptimizationProblem
import opt.prob.MIMIC as MIMIC
import opt.prob.ProbabilisticOptimizationProblem as ProbabilisticOptimizationProblem
import shared.FixedIterationTrainer as FixedIterationTrainer

from array import array

import opt.SimulatedAnnealingBoltzman as SimulatedAnnealingBoltzman
import opt.SimulatedAnnealingFast as SimulatedAnnealingFast

"""
Commandline parameter(s):
   none
"""


try:
    N = int(sys.argv[1])
except:
    N = 200

try:
    runs = int(sys.argv[2])
except:
    runs = 10
    
sys.stdout = open("fourpeaks%d.txt" % N, "w")
    
#N=200
T=int((N/5)*4)
fill = [2] * N
ranges = array('i', fill)

iters = 500000

ef = FourPeaksEvaluationFunction(T)
odd = DiscreteUniformDistribution(ranges)
nf = DiscreteChangeOneNeighbor(ranges)
mf = DiscreteChangeOneMutation(ranges)
cf = SingleCrossOver()
df = DiscreteDependencyTree(.1, ranges)
hcp = GenericHillClimbingProblem(ef, odd, nf)
gap = GenericGeneticAlgorithmProblem(ef, odd, mf, cf)
pop = GenericProbabilisticOptimizationProblem(ef, odd, df)

t0 = time.time()
calls = []
results = []
for _ in range(runs):
    rhc = RandomizedHillClimbing(hcp)
    fit = FixedIterationTrainer(rhc, iters)
    fitness = fit.train()
    results.append(ef.value(rhc.getOptimal()))
    calls.append(ef.getTotalCalls())    
    ef.clearCount()
print "RHC, average results , " + str(sum(results)/float(runs))
print "RHC, average feval calls , " + str(sum(calls)/float(runs))
t1 = time.time() - t0
print "RHC, average time , " + str(float(t1)/runs)


t0 = time.time()
calls = []
results = []
for _ in range(runs):
    sa = SimulatedAnnealing(1E11, .95, hcp)
    fit = FixedIterationTrainer(sa, iters)
    fitness = fit.train()
    results.append(ef.value(sa.getOptimal()))
    calls.append(ef.getTotalCalls())
    ef.clearCount()    
print "SA95, average results , " + str(sum(results)/float(runs))
print "SA95, average feval calls , " + str(sum(calls)/float(runs))
t1 = time.time() - t0
print "SA95, average time , " + str(t1/float(runs))


t0 = time.time()
calls = []
results = []
for _ in range(runs):
    sa = SimulatedAnnealing(1E11, .8, hcp)
    fit = FixedIterationTrainer(sa, iters)
    fitness = fit.train()
    results.append(ef.value(sa.getOptimal()))
    calls.append(ef.getTotalCalls())
    ef.clearCount()    
print "SA80, average results , " + str(sum(results)/float(runs))
print "SA80, average feval calls , " + str(sum(calls)/float(runs))
t1 = time.time() - t0
print "SA80, average time , " + str(t1/float(runs))

t0 = time.time()
calls = []
results = []
for _ in range(runs):
    sa = SimulatedAnnealing(1E11, .7, hcp)
    fit = FixedIterationTrainer(sa, iters)
    fitness = fit.train()
    results.append(ef.value(sa.getOptimal()))
    calls.append(ef.getTotalCalls())
    ef.clearCount()    
print "SA70, average results , " + str(sum(results)/float(runs))
print "SA70, average feval calls , " + str(sum(calls)/float(runs))
t1 = time.time() - t0
print "SA70, average time , " + str(t1/float(runs))

t0 = time.time()
calls = []
results = []
for _ in range(runs):
    sa = SimulatedAnnealingBoltzman(1E11, .7, hcp)
    fit = FixedIterationTrainer(sa, iters)
    fitness = fit.train()
    results.append(ef.value(sa.getOptimal()))
    calls.append(ef.getTotalCalls())
    ef.clearCount()    
print "SABOL, average results , " + str(sum(results)/float(runs))
print "SABOL, average feval calls , " + str(sum(calls)/float(runs))
t1 = time.time() - t0
print "SABOL, average time , " + str(t1/float(runs))




t0 = time.time()
calls = []
results = []
for _ in range(runs):
    sa = SimulatedAnnealingFast(1E11, .7, hcp)
    fit = FixedIterationTrainer(sa, iters)
    fitness = fit.train()
    results.append(ef.value(sa.getOptimal()))
    calls.append(ef.getTotalCalls())
    ef.clearCount()    
print "SAFAS, average results , " + str(sum(results)/float(runs))
print "SAFAS, average feval calls , " + str(sum(calls)/float(runs))
t1 = time.time() - t0
print "SAFAS, average time , " + str(t1/float(runs))

