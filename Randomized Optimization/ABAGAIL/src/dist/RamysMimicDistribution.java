package dist;

import shared.DataSet;
import shared.Instance;

import java.util.Vector;

/**
 * A distribution of all of the permutations
 * of a set size.
 * @author Andrew Guillory gtg008g@mail.gatech.edu
 * @version 1.0
 */
public class RamysMimicDistribution extends AbstractDistribution {
    /**
     * The ranges of the data
     */
    //private int[] n;
    private Vector<Integer> n;
    private Vector<Integer> pops = new Vector<Integer>() ;
    private Vector<Integer> keeps= new Vector<Integer>() ;
    private Vector<Integer> muts = new Vector<Integer>() ;
    private Vector<Integer> its  = new Vector<Integer>() ;

    /**
     * The probabilites
     */
    private Vector<Double>  p = new Vector<Double>();
    private  int  popBegin   ;
    private  int  popEnd     ;
    private  int  keepBegin  ;
    private  int  keepEnd    ;
    private  int  mutBegin   ;
    private  int  mutEnd     ;
    private  int  itersBegin ;
    private  int  itersEnd   ;
    /**
     * Make a new discrete permutation distribution
     * @param n the size of the data
     */
    public RamysMimicDistribution(Vector<Integer> n) {
        this.n = n;
        popBegin =   n.get(0);
        popEnd =     n.get(1);
        keepBegin =  n.get(2);
        keepEnd =    n.get(3);
        mutBegin =   n.get(4);
        mutEnd =     n.get(5);
        itersBegin = n.get(6);
        itersEnd =   n.get(7);
        System.out.println("Ramy's MIMIC Distribution at your service!");
        for(int i=popBegin; i<= popEnd; i++)
        {
            int idx = i - popBegin;
            pops.addElement(i);
        }

        for(int i=keepBegin; i<= keepEnd; i++)
        {
            int idx = i - keepBegin;
            pops.addElement(i);
        }
        for(int i=mutBegin; i<= mutEnd; i++)
        {
            int idx = i - mutBegin;
            muts.addElement(i);
        }
        for(int i=itersBegin; i<= itersEnd; i++)
        {
            int idx = i - itersBegin;
            its.addElement(i);
        }

        double val = 1.f / pops.size();
        p.addElement(val);
        val = 1.f / keeps.size();
        p.addElement(val);
        val = 1.f / muts.size();
        p.addElement(val);
        val = 1.f / its.size();
        p.addElement(val);

// compute uniform probs for each range.
//        for (int i = 1; i < n.length; i++) {
//            p *= n[i];
//        }
//        p = 1 / p;
    }

    /**
     * @see Distribution#probabilityOf(shared.Instance)
     */
    public double p(Instance i)
    {
        double result = 0;
        int d = 0;
        double p0 = 0;
        if ( ( (d =i.getDiscrete(0)) > popBegin) && (d < popEnd) )
          p0=p.get(0);
        double p1 = 0;
        if ( ( (d =i.getDiscrete(1)) > popBegin) && (d < popEnd) )
          p1=p.get(1);
        double p2 = 0;
        if ( ( (d =i.getDiscrete(2)) > popBegin) && (d < popEnd) )
          p2=p.get(2);
        double p3 = 0;
        if ( ( (d =i.getDiscrete(3)) > popBegin) && (d < popEnd) )
          p3=p.get(3);

        return result = p0*p1*p2*p3;
    }

    /**
     * @see Distribution#generateRandom(shared.Instance)
     */
    public Instance sample(Instance ignored) {
        int len = pops.size() + keeps.size() + muts.size() + its.size();
        double [] d = new double[len];
        int count = 0;
        for(int i =0; i < pops.size(); i++)
        {
            d[count++] = random.nextInt(pops.get(i));
        }
        for(int i =0; i < keeps.size(); i++)
        {
            d[count++] = random.nextInt(keeps.get(i));
        }
        for(int i =0; i < muts.size(); i++)
        {
            d[count++] = random.nextInt(muts.get(i));
        }
        for(int i =0; i < its.size(); i++)
        {
            d[count++] = random.nextInt(its.get(i));
        }
        return new Instance(d);


//        double[] d  = new double[n.length];
//        for (int i = 0; i < d.length; i++) {
//            d[i] = random.nextInt(n[i]);
//        }
//        return new Instance(d);
    }

    /**
     * @see Distribution#generateMostLikely(shared.Instance)
     */
    public Instance mode(Instance ignored) {
        return sample(ignored);
    }

    /**
     * @see Distribution#estimate(shared.DataSet)
     */
    public void estimate(DataSet observations) {
        return;
    }
}
