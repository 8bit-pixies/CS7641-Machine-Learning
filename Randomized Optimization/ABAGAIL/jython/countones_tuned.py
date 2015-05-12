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
import opt.example.CountOnesEvaluationFunction as CountOnesEvaluationFunction
from array import array

# the size of the vector in question
try:
    N = int(sys.argv[1])
except:
    N = 100

try:
    runs = int(sys.argv[2])
except:
    runs = 20
	
N = 100
runs = 10

sys.stdout = open("onemax%d.txt" % N, "w")

"""
Commandline parameter(s):
   N : number in the test vector
   runs : number of runs to average over
"""

fill = [2] * N
ranges = array('i', fill)

ef = CountOnesEvaluationFunction()
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
    fit = FixedIterationTrainer(rhc, 200)
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
    fit = FixedIterationTrainer(sa, 200)
    fitness = fit.train()
    results.append(ef.value(sa.getOptimal()))
    calls.append(ef.getTotalCalls())
    ef.clearCount()    
print "SA, average results , " + str(sum(results)/float(runs))
print "SA, average feval calls , " + str(sum(calls)/float(runs))
t1 = time.time() - t0
print "SA, average time , " + str(t1/float(runs))

t0 = time.time()
calls = []
results = []
for _ in range(runs):
    ga_pop = N*5
    ga_keep = int(ga_pop * .75)
    ga_mut = int(ga_pop * .2)
    ga = StandardGeneticAlgorithm(ga_pop, ga_keep, ga_mut, gap)
    fit = FixedIterationTrainer(ga, 150)
    fitness = fit.train()
    results.append(ef.value(ga.getOptimal()))
    calls.append(ef.getTotalCalls())
    ef.clearCount()
print "GA, average results , " + str(sum(results)/float(runs))
print "GA, average feval calls , " + str(sum(calls)/float(runs))
t1 = time.time() - t0
print "GA, average time , " + str(t1/float(runs))

t0 = time.time()
calls = []
results = []
for _ in range(runs):
    mimic = MIMIC(N/2, N/5, pop)
    fit = FixedIterationTrainer(mimic, 100)
    fitness = fit.train()
    results.append(ef.value(mimic.getOptimal()))
    calls.append(ef.getTotalCalls())
    ef.clearCount()
print "MIMIC, average results, " + str(sum(results)/float(runs))
print "MIMIC, average feval calls , " + str(sum(calls)/float(runs))
t1 = time.time() - t0
print "MIMIC, average time , " + str(t1/float(runs))

