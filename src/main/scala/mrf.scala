package mrfinference

class Mrf(val factors: Vector[Factor], val domain: Set[Int]) {
  
  /** Checks if the variables in X are independent of the variables in Y given the variables in Z.
      Note that this only checks independence based on the scopes on the features, i.e., it checks
      if X is independent of Y given Z in every set of factors with equivalent scopes.
    *
    * @param X the set of variables being checked for independence with Y. Overlap with Z is removed.
    * @param Y the set of variables being checked for independence with X. Overlap with Z is removed.
    * @param Z the set of variables being conditioned on.
    * @return `true` if X is independent of Y given Z in every set of factors with equivalent
              scopes.
    */
  
  val variables: Set[Int] = this.factors.map(_.scope.toSet).reduce(_ | _)
  def checkIndependence(X: Set[Int], Y: Set[Int], Z: Set[Int]): Boolean = {
    // This algorithm works by recursively iterating over the factors. If the current factor has
    // both elements of X and Y then the algorithm returns `false`. If not but an element of X
    // appears in the factor, then it adds all the elements not in Z into X and continues to the
    // next factor, and similar for Y, continuing until all factors have been checked. See
    // [1][Section 4.3] for further details.
    @scala.annotation.tailrec
    def recurCheckIndependence(X:Set[Int], Y:Set[Int], Z:Set[Int], factors: Vector[Factor]): Boolean = {
      if (factors.isEmpty) true else {
        val currScope = factors.head.scope.toSet -- Z
        val hasVarInX = (X & currScope).size >= 1
        val hasVarInY = (Y & currScope).size >= 1
        if (hasVarInX && hasVarInY) {
          false
        } else if (hasVarInX) {
          recurCheckIndependence(X | currScope, Y, Z, factors.tail)
        } else if (hasVarInY) {
          recurCheckIndependence(X, Y | currScope, Z, factors.tail)
        } else {
          recurCheckIndependence(X, Y, Z, factors.tail)
        }
      }
    }
    recurCheckIndependence(X -- Z, Y -- Z, Z, this.factors)
  }
  
  /** Uses variable elimination to find the marginals of an input variable. Returned as a Map from
      the domain.
    *
    * @param x the variable to get the marginals of
    */
  def marginalize(x: Int): Map[Int, Double] = {
    // This algorithm works by choosing a variable to be eliminated, creating a product factor of
    // all factors that contain the chosen variable, and them summing the variable out. It then
    // continues recursively. See [1][Section 9.2] for futher details.
    @scala.annotation.tailrec
    def recurMarginalize(x:Int, currFactors: Vector[Factor], varsToElim: Vector[Int]): Factor = {
      // If all variables have been eliminated then the product of the remaining factors is what we
      // need.
      if (varsToElim.isEmpty) currFactors.reduce(Factor.productFactor(_,_)) else {
        val elimVar = varsToElim.head
        val factorsToMultiply = currFactors.filter(_.scope contains elimVar)
        val productFactor = factorsToMultiply.reduce(Factor.productFactor(_,_))
        val elimFactor = Factor.eliminatedVariableFactor(productFactor, elimVar, this.domain)
        recurMarginalize(x, elimFactor +: currFactors.filterNot(_.scope contains elimVar), varsToElim.tail)
      }
    }
    val finalFactor = recurMarginalize(x, this.factors, this.variables.filter(_ != x).toVector)
    // Just need to normailize to get the marginals
    val partitionFunction = this.domain.foldLeft(0.0){(acc, curr) => 
      acc + finalFactor.feature(Vector(curr))
    }
    Map[Int, Double]() ++ this.domain.map(x => (x, finalFactor.feature(Vector(x)) / partitionFunction))
  }
}

object MrfUtils {
  val factorSizeAfterEliminationz: (Int, Vector[Factor]) => Int = (x, factors) => {
    factors.map(f => f.scope.toSet).reduce { (acc, next) =>
      if(next contains x) (acc | next) else acc
    }.size-1
  }
}
/** Sources
  *
  * [1] Koller, D., and Friedman, N., Probabilistic Graphical Models: Principles and Techniques,
        MIT Press, 2009.
  */