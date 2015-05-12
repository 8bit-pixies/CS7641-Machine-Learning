package opt.ga;

import shared.Instance;
import dist.Distribution;



/**
 * Created by Chapman on 11/03/2015.
 */
public class TwoPointCrossOver implements CrossoverFunction {

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
