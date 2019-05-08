package mathCalc

import org.scalatest.WordSpec

/**
 * Created by Gustavo on 07/12/2014.
 */
class GrillaMultidimensionalTest extends WordSpec {

  "una grilla" when {
    val grilla1 = GrillaMultidimensional.fill(List(1))(índice => índice.sum)
    "creada usando fill(List(1))(índice=>índice.sum)" should {
      "ser igual a " in {
        assert( grilla1 ===
            Rama(1, IndexedSeq(Hoja(0))))
      }
    }
    val grilla2 = Rama(2, IndexedSeq(
            Rama(1, IndexedSeq(Hoja(0), Hoja(1), Hoja(2))),
            Rama(1, IndexedSeq(Hoja(1), Hoja(2), Hoja(3)))))
    "creada usando fill(List(2,3))(índice=>índice.sum)" should {
      "ser igual a " in {
        assert(GrillaMultidimensional.fill(List(2, 3))(índice => índice.sum) === grilla2)
      }
    }
    
    "consultada " should {
       "dar el resultado esperado 1" in {
         assert(grilla2(List(1,1))===Hoja(2) )
       }
       "dar el resultado esperado 2" in {
         assert(grilla2(List(1))===Rama(1,IndexedSeq(Hoja(1), Hoja(2), Hoja(3) )))
       } 
       "lanzar IllegalArgumentException" in {
          intercept[IllegalArgumentException] {
             grilla2(List(1,2,3))
          }
       }         
    }
    "updateada" should {
      "dar el resultado esperado 1" in {
        assert(grilla2.updated(List(1,1),Hoja(7))(List(1,1))===Hoja(7))
      }
    }
  }

}
