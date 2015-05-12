package opt.example;

import opt.EvaluationFunction;
import shared.Instance;
import util.linalg.Vector;

/**
 * Created by Chapman on 7/02/2015.
 */
public class ViennetEvaluationFunction implements EvaluationFunction{
    private double scaleVal;
    private double centreVal;

    public ViennetEvaluationFunction(double centre, double scale) {
        scaleVal = scale;
        centreVal = centre;
    }

    public double value(Instance d) {
        Vector data = d.getData();
        /*only 2 params*/
        double x = (data.get(0) - centreVal)/scaleVal;
        double y = (data.get(1) - centreVal)/scaleVal;

        double f1 = 05*(Math.pow(x,2) + Math.pow(y,2)) + Math.sin(Math.pow(x,2) + Math.pow(y,2));
        double f2 = (Math.pow(3*x-2*y+4, 2)/8) + (Math.pow(x-y+1,2)/27) + 15;
        double f3 = (1/(Math.pow(x,2)+Math.pow(y,2)+1))-(1.1*Math.exp(-(Math.pow(x,2) + Math.pow(y,2))));

        double val = Math.min(f3, Math.min(f1, f2));

        return -val; /*convert the function to a maximization problem*/
    }
}
