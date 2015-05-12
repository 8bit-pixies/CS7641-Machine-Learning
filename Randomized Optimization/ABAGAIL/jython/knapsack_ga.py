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
import opt.example.KnapsackEvaluationFunction as KnapsackEvaluationFunction
from array import array

# the size of the vector in question

"""
Commandline parameter(s):
    none
"""
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

sys.stdout = open("knapsack_ga_%d-%d-%d-%d-%d.txt" % (N, ga_pop,co_type,ga_keep,ga_mut), "w")
runs=10
# Random number generator */
random = Random()
# The number of copies each
COPIES_EACH = 4
# The maximum weight for a single element
MAX_WEIGHT = 50
# The maximum volume for a single element
MAX_VOLUME = 50
# The volume of the knapsack 
KNAPSACK_VOLUME = MAX_VOLUME * N * COPIES_EACH * .4

# create copies
fill = [COPIES_EACH] * N
copies = array('i', fill)

# create weights and volumes
fill = [0] * N
weights = array('d', fill)
volumes = array('d', fill)
for i in range(0, N):
    weights[i] = random.nextDouble() * MAX_WEIGHT
    volumes[i] = random.nextDouble() * MAX_VOLUME


# create range
fill = [COPIES_EACH + 1] * N
ranges = array('i', fill)

ef = KnapsackEvaluationFunction(weights, volumes, KNAPSACK_VOLUME, copies)
odd = DiscreteUniformDistribution(ranges)
nf = DiscreteChangeOneNeighbor(ranges)
mf = DiscreteChangeOneMutation(ranges)
cf = UniformCrossOver()
df = DiscreteDependencyTree(.1, ranges)
hcp = GenericHillClimbingProblem(ef, odd, nf)
gap = GenericGeneticAlgorithmProblem(ef, odd, mf, cf)
pop = GenericProbabilisticOptimizationProblem(ef, odd, df)

# -- begin problem

t0 = time.time()
calls = []
results = []
for _ in range(runs):
    ga = StandardGeneticAlgorithm(ga_pop, ga_keep, ga_mut, gap)
    fit = FixedIterationTrainer(ga, 1000)
    fitness = fit.train()
    results.append(ef.value(ga.getOptimal()))
    calls.append(ef.getTotalCalls())
    ef.clearCount()
print "GA, average results , " + str(sum(results)/float(runs))
print "GA, average feval calls , " + str(sum(calls)/float(runs))
t1 = time.time() - t0
print "GA, average time , " + str(t1/float(runs))






