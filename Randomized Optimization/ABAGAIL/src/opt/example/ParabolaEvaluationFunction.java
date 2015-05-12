package opt.example;

/*y = -(x-a)^2 + b*/

import opt.EvaluationFunction;
import shared.Instance;
import util.linalg.Vector;

public class ParabolaEvaluationFunction implements EvaluationFunction{
    private double xVal;
    private double yVal;
    public ParabolaEvaluationFunction(double a, double b) {
        xVal = a;
        yVal = b;
    }

    public double value(Instance d) {
        Vector data = d.getData();
        double value = 0;
        for (int i = 0; i < data.size(); i++) {
            value += -Math.pow(data.get(i)-xVal, 2) + yVal;
        }
        return value;
    }
}