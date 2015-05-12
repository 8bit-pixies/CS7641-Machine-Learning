package opt.ga;

import dist.Distribution;
import shared.Instance;


/**
 * A single point cross over function
 * @author Andrew Guillory gtg008g@mail.gatech.edu
 * @version 1.0
 */
public class DoubleCrossOver implements CrossoverFunction {

    /**
     * @see opt.CrossOverFunction#mate(opt.OptimizationData, opt.OptimizationData)
     */
    public Instance mate(Instance a, Instance b) {
        double[] newData = new double[a.size()];
        int point1 = Distribution.random.nextInt(newData.length + 1);
        int point2 = Distribution.random.nextInt(newData.length + 1);

        for (int i = 0; i < newData.length; i++) {
            if (i >= Math.min(point1, point2)) {
                newData[i] = a.getContinuous(i);
            } else if (i <= Math.max(point1, point2)) {
                newData[i] = b.getContinuous(i);
            } else {
                newData[i] = a.getContinuous(i);
            }
        }
        return new Instance(newData);
    }

}