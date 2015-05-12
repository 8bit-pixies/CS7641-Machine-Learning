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

"""
Commandline parameter(s):
   none
"""

try:
    N = int(sys.argv[1])
except:
    N = 100

try:
    samples = int(sys.argv[2])
except:
    samples = 200

try:
    tokeep = int(sys.argv[3])
except:
    tokeep = 10
runs = 10
    
sys.stdout = open("fourpeaks_MIMIC-%d-%d-%d.txt" % (N, samples, tokeep), "w")
   
#N=200
T = N/5
fill = [2] * N
ranges = array('i', fill)



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
    mimic = MIMIC(samples, tokeep, pop)
    fit = FixedIterationTrainer(mimic, 1000)
    fitness = fit.train()
    results.append(ef.value(mimic.getOptimal()))
    calls.append(ef.getTotalCalls())
    ef.clearCount()
print "MIMIC, average results, " + str(sum(results)/float(runs))
print "MIMIC, average feval calls , " + str(sum(calls)/float(runs))
t1 = time.time() - t0
print "MIMIC, average time , " + str(t1/float(runs))

