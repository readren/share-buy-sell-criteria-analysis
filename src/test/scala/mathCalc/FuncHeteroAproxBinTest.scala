package mathCalc

import org.scalatest.WordSpec
import mathCalc.funcHeteroAproxBin._
import org.scalactic.Tolerance

/**
 * Created by Gustavo on 09/12/2014.
 */
class FuncHeteroAproxBinTest extends WordSpec with Tolerance {

	"una función" when {
		
		val f = Func.fill(CuboMd(List(Intervalo(3,5)))) (x => List(x.reduce(_ + _)))
		"creada usando fill" should {
			"dar lo esperado 1" in {
				assert(f(List(0)) === List(0))
			}
			"dar lo esperado 2" in {
				assert(f(List(3)) === List(3))
			}
			"dar lo esperado 3" in {
				assert(f(List(3.5)) === List(3.5))
			}
			"dar lo esperado 4" in {
				assert(f(List(6)) === List(6))
			}
			"lanzar lo esperado" in {
				intercept[IllegalArgumentException] {
					val h = f(List(3, 4))
				}
			}
		}
	}

	"una función bidimensional " when {
		val f = Func.fill(CuboMd(List(Intervalo(3,5),Intervalo(10,12))))(x => List(x.reduce(_ + _)))
		"creada usando fill" should {
			"dar lo esperado 1" in {
				assert(f(List(0,0)) === List(0))
			}
			"dar lo esperado 2" in {
				assert(f(List(2,11)) === List(13))
			}
			"dar lo esperado 3" in {
				assert(f(List(4,11)) === List(15))
			}
			"dar lo esperado 4" in {
				assert(f(List(4,15)) === List(19))
			}
			"dar lo esperado 5" in {
				assert(f(List(7,15)) === List(22))
			}
			"lanzar lo esperado" in {
				intercept[IllegalArgumentException] {
					val h = f(List(3))
				}
			}
		}
	}

	"debe hacer bien la interpolación " when {
		val f = Func.fill(CuboMd(List(Intervalo(0,1),Intervalo(0,1))))(x => {
			val m = Map(List(0d,0d)-> -1d,List(0d,1d)->2d,List(1d,1d)->0d,List(1d,0d)->1d)
			List(m.withDefaultValue(0d).apply(x))
		});
		"creada usando fill" should {
			"dar lo esperado 1" in {
				assert(0.5+-0.0001 === f(List(.5,.5))(0))
			}
			"dar lo esperado 2" in {
				assert(-0.16+-0.0001 === f(List(.2,.2))(0))
			}
			"dar lo esperado 3" in {
				assert(0.44+-0.0001 === f(List(0.7,0.2))(0))
			}
		}
	}
	
	"al funcionar la partición" when {
		val f = Func.fill(CuboMd(List(Intervalo(3,5),Intervalo(10,12))))(x => List(x.reduce(_ + _)))
	}
	
}
