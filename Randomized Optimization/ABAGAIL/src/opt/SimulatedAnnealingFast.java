package opt;

import dist.Distribution;
import shared.Instance;

/**
 * A simulated annealing hill climbing algorithm
 * @author Andrew Guillory gtg008g@mail.gatech.edu
 * @version 1.0
 */
public class SimulatedAnnealingFast extends OptimizationAlgorithm {
    
    /**
     * The current optimiation data
     */
    private Instance cur;
    
    /**
     * The current optimization value
     */
    private double curVal;
    
    /**
     * The current temperature
     */
    private double t;
    
    /**
     * The cooling parameter
     */
    private double cooling;

    /**
     * Keeps track of what iteration we're on* 
     */
    private double iter=0.0001;

    /**
     * Initial Termperature
     */
    private double initTemp;
    
    /**
     * Make a new simulated annealing hill climbing
     * @param t the starting temperature
     * @param cooling the cooling exponent
     * @param hcp the problem to solve
     */
    public SimulatedAnnealingFast(double t, double cooling, HillClimbingProblem hcp) {
        super(hcp);
        this.t = t;
        this.initTemp = t;
        this.cooling = cooling;
        this.cur = hcp.random();
        this.curVal = hcp.value(cur);
    }

    /**
     * @see shared.Trainer#train()
     */
    public double train() {
        HillClimbingProblem p = (HillClimbingProblem) getOptimizationProblem();
        Instance neigh = p.neighbor(cur);
        double neighVal = p.value(neigh);
        if (neighVal > curVal || Distribution.random.nextDouble() < 
                Math.exp((neighVal - curVal) / t)) {
            curVal = neighVal;
            cur = neigh;
        }
        iter = iter+1;
        t = initTemp / iter;
        return curVal;
    }

    /**
     * @see OptimizationAlgorithm#getOptimal()
     */
    public Instance getOptimal() {
        return cur;
    }

}