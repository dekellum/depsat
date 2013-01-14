package depsat.support;

import java.math.BigInteger;
import java.util.Arrays;

import org.sat4j.core.Vec;
import org.sat4j.core.VecInt;
import org.sat4j.minisat.SolverFactory;
import org.sat4j.pb.IPBSolver;
import org.sat4j.pb.ObjectiveFunction;
import org.sat4j.specs.ContradictionException;
import org.sat4j.specs.ISolver;
import org.sat4j.specs.TimeoutException;

public class Dependency
{

    public static void main( String[] args )
        throws ContradictionException, TimeoutException
    {
        new Dependency().psuedo_solve();
    }

    public void ssolv() throws ContradictionException, TimeoutException
    {
        ISolver solver = SolverFactory.newDefault();
        solver.newVar( 3 );
        solver.setExpectedNumberOfClauses( 12 );

        solver.addClause( new VecInt( new int[] {  1,  -2,  3 } ) );
        solver.addExactly( new VecInt( new int[] {  1,   2,  3 } ), 1 );
        //solver.addAtMost(  new VecInt( new int[] {  1,   2,  3 } ), 1 );
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

    public void psuedo_solve() throws ContradictionException, TimeoutException
    {
        IPBSolver solver = org.sat4j.pb.SolverFactory.newDefault();
        solver.newVar( 3 );
        solver.setExpectedNumberOfClauses( 12 );

        solver.addClause( new VecInt( new int[] {  1,  -2,  3 } ) );

        // lower coef (1 vs 8) means favor var 1 in this case?
        solver.addExactly( new VecInt( new int[] {  1,  2,  3 } ),
                           new VecInt( new int[] {  1,  1,  8 } ),
                           1 );

        solver.addExactly( new VecInt( new int[] {  1,  2,  3 } ), 1 );

        // Lowest coeff wins between 1 and 2
        //Vec<BigInteger> coeffs = new Vec<BigInteger>( 3 );
        //coeffs.push( BigInteger.valueOf( 1 ) );
        //coeffs.push( BigInteger.valueOf( 0 ) );
        //coeffs.push( BigInteger.valueOf( 3 ) );

        //solver.setObjectiveFunction( new ObjectiveFunction(
        //    new VecInt( new int[] {  1,  2,  3 } ),
        //    coeffs ) );

        //solver.addAtMost( new VecInt( new int[] {  1,   2,  3 } ), 1 );
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
