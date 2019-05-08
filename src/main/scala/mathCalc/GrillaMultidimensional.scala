package mathCalc

/**
 * Created by Gustavo on 07/12/2014.
 */

object GrillaMultidimensional {
	def fill[T](capacidad: List[Int])(f: List[Int] => T) = {
		def loop(índice: Seq[Int], capacidad: List[Int]): GrillaMultidimensional[T] = capacidad match {
			case Nil => Hoja(f(índice.toList))
			case head :: tail =>
				val elems = for (i <- 0 until head) yield loop(índice :+ i, tail)
				Rama(capacidad.size, elems)
		}
		loop(Seq(), capacidad)
	}

	implicit def toHoja[X](x: X) = Hoja(x)
}

trait GrillaMultidimensional[T] {
	val dimensión: Int
	def apply(coordenada: List[Int]): GrillaMultidimensional[T]
	def updated(coordenada: List[Int], valor: GrillaMultidimensional[T]): GrillaMultidimensional[T];

	def get(cordenada: List[Int]): T = {
		apply(cordenada) match {
			case Hoja(x) => x
			case _ => throw new IllegalArgumentException("la coordenada tiene menor dimension que la grilla, y debería ser igual")
		}
	}
	def updated(cordenada: List[Int], valor: T): GrillaMultidimensional[T] =
		updated(cordenada, Hoja(valor))
}

case class Hoja[T](valor: T) extends GrillaMultidimensional[T] {
	override val dimensión = 0
	override def apply(cordenada: List[Int]) = {
		if (cordenada.isEmpty) this
		else throw new IllegalArgumentException("celda tiene mas elementos que la dimension de la grilla")
	}
	override def updated(cordenada: List[Int], valor: GrillaMultidimensional[T]) = valor match {
		case v: Hoja[T] => v
		case _ => throw new IllegalArgumentException("la dimensión del nuevo valor es mayor a la dimensión del viejo valor")
	}
}

case class Rama[T](dimensión: Int, siguiente: IndexedSeq[GrillaMultidimensional[T]]) extends GrillaMultidimensional[T] {

	override def apply(índice: List[Int]) = índice match {
		case Nil => this
		case head :: tail => siguiente(head).apply(tail)
	}

	override def updated(cordenada: List[Int], valor: GrillaMultidimensional[T]) = cordenada match {
		case Nil => valor
		case head :: tail =>
			val viejoValorElem = siguiente(head)
			val nuevoValorElem = viejoValorElem.updated(tail, valor)
			if (viejoValorElem.dimensión != nuevoValorElem.dimensión) throw new IllegalArgumentException("la dimensión del nuevo valor es distinta a la dimensión del elemento a modificar")
			val nuevoValorSeq = siguiente.updated(head, nuevoValorElem)
			Rama(dimensión, nuevoValorSeq)
	}
}
