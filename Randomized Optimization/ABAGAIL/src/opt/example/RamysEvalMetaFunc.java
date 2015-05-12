package opt.example;

import dist.DiscreteUniformDistribution;
import opt.EvaluationFunction;
import opt.ga.DiscreteChangeOneMutation;
import opt.ga.GenericGeneticAlgorithmProblem;
import opt.ga.SingleCrossOver;
import opt.ga.StandardGeneticAlgorithm;
import shared.FixedIterationTrainer;
import shared.Instance;
import util.linalg.Vector;

/**
 * A function that tries to get MIMIC to optimize the params for another algo
 */
public class RamysEvalMetaFunc implements EvaluationFunction {
    /**
     * @see opt.EvaluationFunction#value(opt.OptimizationData)
     */
    private int[] problemData;
    public RamysEvalMetaFunc(int [] probVec)
    {
        this.problemData = probVec;
    }

    public double value(Instance d)
    {
        Vector data = d.getData();
        int pop = d.getDiscrete(0);
        int keep = d.getDiscrete(1);
        int mut = d.getDiscrete(2);
        int iters = d.getDiscrete(3);
        int k = (int) Math.ceil(pop* (double) keep /100.f);
        int m = Math.round(pop* mut/100.f);
        m = Math.max(m,1);
        k = Math.max(k,1);
        int i = Math.max(iters,1);
        int [] ranges = new int[4];
        ranges[0] = pop;
        ranges[1] = k;
        ranges[2] = m;
        ranges[3] = i;

        CountOnesEvaluationFunction ef = new CountOnesEvaluationFunction();
        DiscreteUniformDistribution odd = new DiscreteUniformDistribution(problemData);
        DiscreteChangeOneMutation mf = new DiscreteChangeOneMutation(problemData);
        SingleCrossOver cf = new SingleCrossOver();
        GenericGeneticAlgorithmProblem gap = new GenericGeneticAlgorithmProblem(ef, odd, mf, cf);

        StandardGeneticAlgorithm ga = new StandardGeneticAlgorithm(pop, k,m,gap );
        FixedIterationTrainer fit = new FixedIterationTrainer(ga, i);
        double val = fit.train();
        double result = 1.f/val;
        System.out.println("Metafunc outputs " + result);
        return result;
    }

//    public double value(Instance d) {
//        Vector data = d.getData();
//        double val = 0;
//        for (int i = 0; i < data.size(); i++) {
//            if (data.get(i) == 1) {
//                val++;
//            }
//        }
//        return val;
//    }
}