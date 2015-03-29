package mrfinference

/** Factory for [[mrfinference.Factor]] instances. */
object Factor{
  
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
    val vectorFeature = (vals: Vector[Int]) => feature(vals(0))
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
    val vectorFeature = (vals: Vector[Int]) => feature(vals(0), vals(1))
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
    val vectorFeature = (vals: Vector[Int]) => feature(vals(0), vals(1), vals(2))
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
    val vectorFeature = (vals: Vector[Int]) => feature(vals(0), vals(1), vals(2), vals(3))
    new Factor(vectorScope, vectorFeature)
  }
}

class Factor(val scope: Vector[Int], val feature: Vector[Int] => Double) {}