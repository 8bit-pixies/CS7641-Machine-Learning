package opt.example;

import opt.EvaluationFunction;
import shared.Instance;
import util.linalg.Vector;

/**
 * Created by Chapman on 7/02/2015.
 */
public class RastriginEvaluationFunction implements EvaluationFunction {

    private double scaleVal;
    private double centreVal;
    private double lastVal = -999999999.0;
    private int totalCalls;

    public RastriginEvaluationFunction(double centre, double scale){
        /*shiftVal = shift;*/
        scaleVal = scale;
        centreVal = centre;
    }

    public double value(Instance d) {
        Vector data = d.getData();
        double value = 0.0;
        for (int i = 0; i < data.size(); i++) {
            double xi = (data.get(i) - centreVal)/scaleVal;
            value += 10 * Math.cos(2*Math.PI*xi) - xi*xi ;
        }
        value += -10*data.size();
        if (this.lastVal != value) {
            this.lastVal = value;
            this.totalCalls++;
        }
        return value; /*convert the function to a maximization problem*/
    }
    public int getTotalCalls() { return totalCalls; }
    public void clearCount() {
        totalCalls = 0;
        lastVal = -999999999.0;
    }
}
