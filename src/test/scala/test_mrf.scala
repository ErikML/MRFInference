package mrfinference.tests

import org.scalatest._
import mrfinference.{Factor, Mrf}

class MrfSpec extends FlatSpec with Matchers{
  behavior of "The independent, uniform, and binary Mrf"
  
  val oneVarUniformFeature: Int=>Double = x => 1
  val iUBFactor1 = Factor(1, oneVarUniformFeature)
  val iUBFactor2 = Factor(2, oneVarUniformFeature)
  val iUBFactor3 = Factor(3, oneVarUniformFeature)
  val iUBFactor4 = Factor(4, oneVarUniformFeature)
  val iUBFactors = Vector(iUBFactor1, iUBFactor2, iUBFactor3, iUBFactor4)
  val binaryDomain = Set(0,1)
  
  val iUBMrf = new Mrf(iUBFactors, binaryDomain)
  
  it should "have the right variable ids" in {
    assert(iUBMrf.variables === Set(1,2,3,4))
  }
  
  it should "identify independence for all independence queries" in {
    assert(iUBMrf.checkIndependence(Set(1,2), Set(3,4), Set()) === true)
    assert(iUBMrf.checkIndependence(Set(1,3), Set(2), Set(4)) === true)
    assert(iUBMrf.checkIndependence(Set(4), Set(3,2), Set(1)) === true)
  }
  
  it should "return marginalization queries accurate within 10^-5" in {
    val iUBp1 = iUBMrf.marginalize(1)
    iUBp1(0) should equal (0.50000 +- 0.00001)
    iUBp1(1) should equal (0.50000 +- 0.00001)
  }
  
  behavior of "The 4-cycle, attractive, binary Mrf"
  
  val attractiveFeature: (Int, Int) => Double = (x,y) => if (x == y) 2 else 1
  val fCFactor1 = Factor((1,2), attractiveFeature)
  val fCFactor2 = Factor((2,3), attractiveFeature)
  val fCFactor3 = Factor((3,4), attractiveFeature)
  val fCFactor4 = Factor((4,1), attractiveFeature)
  val fCFactors = Vector(fCFactor1, fCFactor2, fCFactor3, fCFactor4)
  
  val fCMrf = new Mrf(fCFactors, binaryDomain)
  
  it should "have the right variable ids" in {
    assert(fCMrf.variables === Set(1,2,3,4))
  }
  
  it should "identify independence for opposing corners when conditioned on the rest" in {
    assert(fCMrf.checkIndependence(Set(1), Set(3), Set(2,4)) === true)
    assert(fCMrf.checkIndependence(Set(2), Set(4), Set(1,3)) === true)
  }
  
  it should "identify dependencies for opposing corners without conditioning on the rest" in {
    assert(fCMrf.checkIndependence(Set(1), Set(3), Set()) === false)
    assert(fCMrf.checkIndependence(Set(2), Set(4), Set(3)) === false)
    assert(fCMrf.checkIndependence(Set(4), Set(2), Set(3)) === false)
    assert(fCMrf.checkIndependence(Set(3), Set(1), Set(2)) === false)
  }
  
  it should "identify dependencies for neighbors regardless of conditioning" in {
    assert(fCMrf.checkIndependence(Set(1), Set(2), Set(3,4)) === false)
    assert(fCMrf.checkIndependence(Set(2), Set(3), Set()) === false)
    assert(fCMrf.checkIndependence(Set(1), Set(4), Set(3)) === false)
    assert(fCMrf.checkIndependence(Set(4), Set(3,2), Set(1)) === false)
    assert(fCMrf.checkIndependence(Set(1,3), Set(2,3), Set()) === false)
  }
  
  it should "return marginalization queries accurate within 10^-5" in {
    val fCMrfp1 = fCMrf.marginalize(1)
    fCMrfp1(0) should equal (0.50000 +- 0.00001)
    fCMrfp1(1) should equal (0.50000 +- 0.00001)
  }
  
  behavior of "The length-5-line, biased, attractive, binary Mrf"
  val biasedAttractiveFeature: (Int, Int) => Double = (x,y) => {
    val domain = Set(0,1)
    if(!(domain contains x) || !(domain contains x)) 0.0
    else if(x == y && x == 0) 2.0
    else if(x == y && x == 1) 3
    else 1.0
  }
  
  val lFLBABFactor1 = Factor((1,2), biasedAttractiveFeature)
  val lFLBABFactor2 = Factor((2,3), biasedAttractiveFeature)
  val lFLBABFactor3 = Factor((3,4), biasedAttractiveFeature)
  val lFLBABFactor4 = Factor((4,5), biasedAttractiveFeature)
  val lFLBABFactors = Vector(lFLBABFactor1, lFLBABFactor2, lFLBABFactor3, lFLBABFactor4)
  
  val lFLBABMrf = new Mrf(lFLBABFactors, binaryDomain)
  
  it should "have the right variable ids" in {
    assert(lFLBABMrf.variables === Set(1,2,3,4,5))
  }
  
  it should "identify independencies when conditioned on a separating element" in {
    assert(lFLBABMrf.checkIndependence(Set(4,5), Set(1,2), Set(3)) == true)
    assert(lFLBABMrf.checkIndependence(Set(5), Set(2), Set(4)) == true)
    assert(lFLBABMrf.checkIndependence(Set(1), Set(4), Set(2,3,5)) == true)
    assert(lFLBABMrf.checkIndependence(Set(1,2,3), Set(5), Set(4)) == true)
  }
  
  it should "identify dependencies without conditioning on a separating element" in {
    assert(lFLBABMrf.checkIndependence(Set(2,3), Set(5), Set(1)) == false)
    assert(lFLBABMrf.checkIndependence(Set(3,4,5), Set(2), Set()) == false)
    assert(lFLBABMrf.checkIndependence(Set(2), Set(3), Set(1,5)) == false)
    assert(lFLBABMrf.checkIndependence(Set(4), Set(5,1), Set(2,3)) == false)
  }
  it should "return marginalization queries accurate within 10^-5" in {
    val lFLBABMrfp1 = lFLBABMrf.marginalize(2)
    lFLBABMrfp1(0) should equal (0.32307 +- 0.00001)
    lFLBABMrfp1(1) should equal (0.67692 +- 0.00001)
  }
  
}