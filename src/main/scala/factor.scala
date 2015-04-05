package mrfinference

/** Factory for [[mrfinference.Factor]] instances. */
object Factor {
  
  /** Creates a factor of an arbitrary number of variables.
    * 
    * @param scope the Int ids of the variables the feature function takes as an input in the order
                   of how they should be applied.
    * @param feature a function of the variables identified in scope with input order corresponding
                     to the order in the scope vector.
    */
  def apply(scope: Vector[Int], feature: Vector[Int] => Double): Factor = {
    new Factor(scope, feature)
  }

  /** Creates a 1-variable factor.
    *
    * @param scope the Int id of the variable.
    * @param feature the function of the variable identified in scope.
    */
  def apply(scope: Int, feature: Int => Double) = {
    val vectorScope = Vector(scope)
    val vectorFeature = (args: Vector[Int]) => feature(args(0))
    new Factor(vectorScope, vectorFeature)
  }
  
  /** Creates a 2-variable factor.
    *
    * @param scope the Int ids of the variables
    * @param feature a function of the variables identified in scope with input order corresponding
                     to the order in the scope tuple. 
    */
  def apply(scope: (Int, Int), feature: (Int, Int) => Double) = {
    val vectorScope = Vector(scope._1, scope._2)
    val vectorFeature = (args: Vector[Int]) => feature(args(0), args(1))
    new Factor(vectorScope, vectorFeature)
  }
  
  /** Creates a 3-variable factor.
    *
    * @param scope the Int ids of the variables
    * @param feature a function of the variables identified in scope with input order corresponding
                     to the order in the scope tuple. 
    */
  def apply(scope: (Int, Int, Int), feature: (Int, Int, Int) => Double) = {
    val vectorScope = Vector(scope._1, scope._2, scope._3)
    val vectorFeature = (args: Vector[Int]) => feature(args(0), args(1), args(2))
    new Factor(vectorScope, vectorFeature)
  }
  
  /** Creates a 4-variable factor.
    *
    * @param scope the Int ids of the variables
    * @param feature a function of the variables identified in scope with input order corresponding
                     to the order in the scope tuple. 
    */
  def apply(scope: (Int, Int, Int, Int), feature: (Int, Int, Int, Int) => Double) = {
    val vectorScope = Vector(scope._1, scope._2, scope._3, scope._4)
    val vectorFeature = (args: Vector[Int]) => feature(args(0), args(1), args(2), args(3))
    new Factor(vectorScope, vectorFeature)
  }
  /** Returns a new factor with scope equal to the union and feature equal to the product
    */
  def productFactor(factor1: Factor, factor2: Factor): Factor = {
    // The new scope vector will be the scope of factor1 with a vector containing the variables
    // unique to factor2 appended at the end
    val commonVariables = factor1.scope.toSet & factor2.scope.toSet
    // To evaluate factor2 properly we need to keep track of the indicies of its scope in the new
    // scope
    var factor2ScopeIdxMut = new Array[Int](factor2.scope.length)
    val shift = factor1.scope.length
    var j = 0
    for(i <- 0 to factor2.scope.length-1) {
      if(commonVariables contains factor2.scope(i)) {
        factor2ScopeIdxMut(i) = factor1.scope.indexOf(factor2.scope(i)) // common with factor1
      } else {
        factor2ScopeIdxMut(i) = shift + j // unique to factor2
        j += 1
      }
    }
    val factor2ScopeIdx = factor2ScopeIdxMut.toVector
    val newScope = factor1.scope ++ factor2.scope.filterNot(commonVariables contains _)
    val newFeature = (args: Vector[Int]) => {
      val factor1Args = args take factor1.scope.length
      val factor2Args = factor2ScopeIdx.map(args(_))
      factor1.feature(factor1Args) * factor2.feature(factor2Args)
    }
    new Factor(newScope, newFeature)
  }
  
  /** Returns a new factor by summing over the variable to be eliminated
    *
    * @param factor the factor to have a variable eliminated
    * @param eliminatedVariable the variable that will be summed over
    * @param domain the domain of values the eliminated variable takes
    */
  def eliminatedVariableFactor(factor: Factor, eliminatedVariable: Int, domain: Set[Int]) = {
    val elimVarPos = factor.scope.indexOf(eliminatedVariable)
    val (scopeHead, scopeTail) = factor.scope.splitAt(elimVarPos)
    val newScope = scopeHead ++ (scopeTail.tail)
    val newFeature = (args: Vector[Int]) => {
      val (argsHead, argsTail) = args.splitAt(elimVarPos)
      val enlargedArgs = argsHead ++ (domain.head +: argsTail)
      domain.foldLeft(0.0){(acc, curr) =>
        acc + factor.feature(enlargedArgs.updated(elimVarPos, curr))
      }
    }
    new Factor(newScope, newFeature)
  }
}

class Factor(val scope: Vector[Int], val feature: Vector[Int] => Double) {}