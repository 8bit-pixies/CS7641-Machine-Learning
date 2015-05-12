package opt.example;

import opt.EvaluationFunction;
import shared.Instance;
import util.linalg.Vector;

/**
 * Created by Chapman on 7/02/2015.
 */
public class RosenbrockEvaluationFunction implements EvaluationFunction {
    private double scaleVal;
    private double centreVal;

    public RosenbrockEvaluationFunction(double centre, double scale) {
        scaleVal = scale;
        centreVal = centre;
    }

    public double value(Instance d) {
        Vector data = d.getData();
        double value = 0.0;
        for (int i = 0; i < (data.size()-1); i++) {
            double xi = (data.get(i) - centreVal)/scaleVal;
            double xi1 = (data.get(i+1) - centreVal)/scaleVal;
            value += 100*(Math.pow(xi1-Math.pow(xi,2),2)) + Math.pow(xi-1, 2);
        }

        return -value; /*convert the function to a maximization problem*/
    }
}
