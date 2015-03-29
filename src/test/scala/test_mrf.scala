package mrfinference.tests

import org.scalatest.FlatSpec
import mrfinference.{Factor, Mrf}

class MrfSpec extends FlatSpec {
  
  behavior of "The independent, uniform, and binary Mrf"
  
  val oneVarUniformFeature: Int=>Double = x => 1
  val iUBFactor1 = Factor(1, oneVarUniformFeature)
  val iUBFactor2 = Factor(2, oneVarUniformFeature)
  val iUBFactor3 = Factor(3, oneVarUniformFeature)
  val iUBFactor4 = Factor(4, oneVarUniformFeature)
  val iUBFactors = Vector(iUBFactor1, iUBFactor2, iUBFactor3, iUBFactor4)
  val binaryDomain = Set(0,1)
  
  val iUBMrf = new Mrf(iUBFactors, binaryDomain)
  
  it should "identify independence for all independence queries" in {
    assert(iUBMrf.checkIndependence(Set(1,2), Set(3,4), Set()) === true)
    assert(iUBMrf.checkIndependence(Set(1,3), Set(2), Set(4)) === true)
    assert(iUBMrf.checkIndependence(Set(4), Set(3,2), Set(1)) === true)
  }
  
  behavior of "The 4-cycle, attractive, binary Mrf"
  
  val attractiveFeature: (Int, Int) => Double = (x,y) => if (x == y) 2 else 1
  val fCFactor1 = Factor((1,2), attractiveFeature)
  val fCFactor2 = Factor((2,3), attractiveFeature)
  val fCFactor3 = Factor((3,4), attractiveFeature)
  val fCFactor4 = Factor((4,1), attractiveFeature)
  val fCFactors = Vector(fCFactor1, fCFactor2, fCFactor3, fCFactor4)
  
  val fCMrf = new Mrf(fCFactors, binaryDomain)
  
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
  
  behavior of "The length-5-line, attractive, binary Mrf"
  
  val lFLFactor1 = Factor((1,2), attractiveFeature)
  val lFLFactor2 = Factor((2,3), attractiveFeature)
  val lFLFactor3 = Factor((3,4), attractiveFeature)
  val lFLFactor4 = Factor((4,5), attractiveFeature)
  val lFLFactors = Vector(lFLFactor1, lFLFactor2, lFLFactor3, lFLFactor4)
  
  val lFLMrf = new Mrf(lFLFactors, binaryDomain)
  
  it should "identify independencies when conditioned on a separating element" in {
    assert(lFLMrf.checkIndependence(Set(4,5), Set(1,2), Set(3)) == true)
    assert(lFLMrf.checkIndependence(Set(5), Set(2), Set(4)) == true)
    assert(lFLMrf.checkIndependence(Set(1), Set(4), Set(2,3,5)) == true)
    assert(lFLMrf.checkIndependence(Set(1,2,3), Set(5), Set(4)) == true)
  }
  
  it should "identify dependencies without conditioning on a separating element" in {
    assert(lFLMrf.checkIndependence(Set(2,3), Set(5), Set(1)) == false)
    assert(lFLMrf.checkIndependence(Set(3,4,5), Set(2), Set()) == false)
    assert(lFLMrf.checkIndependence(Set(2), Set(3), Set(1,5)) == false)
    assert(lFLMrf.checkIndependence(Set(4), Set(5,1), Set(2,3)) == false)
  }
}