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

import opt.ga.DoubleCrossOver as DoubleCrossOver

sf = SingleCrossOver()
uf = UniformCrossOver()
tpf = DoubleCrossOver()

try:
    N = int(sys.argv[1])
except:
    N = 100

try:
    ga_pop = int(sys.argv[2])
    #ga_pop = ga_pop*N
except:
    ga_pop = 200

try:
    co_type = int(sys.argv[3])
except:
    co_type = 1

try:
    ga_keep = int(sys.argv[4])
except:
    ga_keep = 1

try:
    ga_mut = int(sys.argv[5])
except:
    ga_mut = 1
    

if co_type == 2:
    cf = tpf
elif co_type == 3:
    cf = uf
else:
    cf = sf

sys.stdout = open("countones_ga_%d-%d-%d-%d-%d.txt" % (N, ga_pop,co_type,ga_keep,ga_mut), "w")
runs=10
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