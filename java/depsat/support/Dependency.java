package depsat.support;

import java.util.Arrays;

import org.sat4j.core.VecInt;
import org.sat4j.minisat.SolverFactory;
import org.sat4j.specs.ContradictionException;
import org.sat4j.specs.ISolver;
import org.sat4j.specs.TimeoutException;

public class Dependency
{

    public static void main( String[] args )
        throws ContradictionException, TimeoutException
    {
        new Dependency().f();
    }

    public void f() throws ContradictionException, TimeoutException
    {
        ISolver solver = SolverFactory.newDefault();
        solver.newVar( 3 );
        solver.setExpectedNumberOfClauses( 12 );

        solver.addClause( new VecInt( new int[] {  1,  -2,  3 } ) );
        solver.addAtLeast( new VecInt( new int[] {  1,   2,  3 } ), 1 );
        solver.addAtMost(  new VecInt( new int[] {  1,   2,  3 } ), 1 );
        //solver.addClause( new VecInt( new int[] { -1,  2, -3 } ) );
        //solver.addClause( new VecInt( new int[] { -1, -2,  3 } ) );

        while( solver.isSatisfiable() ) {
            int [] model = solver.model();
            System.out.println( Arrays.toString( model ) );
            int [] neg = model.clone();
            for( int i = 0; i < neg.length; ++i ) {
                neg[i] = -neg[i];
            }
            solver.addBlockingClause( new VecInt( neg ) );
        }
    }

}
