
import mathCalc.funcHeteroAproxBin._

val f = Func.fill(CuboMd(List(Intervalo(3,5),Intervalo(10,12))))(x=>List(x.reduce(_ + _)))
val g = f.partir(1,11,ValsEnVertices())