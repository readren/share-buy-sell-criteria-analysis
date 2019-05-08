package mathCalc

import scala.collection.immutable.Queue

package object funcHeteroAproxBin {

	case class Intervalo(inf: Núm, sup: Núm) {
		def partido(puntoCorte: Núm) = (Intervalo(inf, puntoCorte), Intervalo(puntoCorte, sup))
	}

	/**Cubo multidimensional resultante del producto cartesiano de intervalos */
	case class CuboMd(intervalos: List[Intervalo]) {
		/** Da los dos cubos multidimensionales resultantes de partir a este cubo en dos.
		  * @param índiceEjePartido índice del eje perpendicular al plano multidimensional de corte
		  * @param puntoCorte coordenada del punto donde el plano corta al eje.
		  */
		def partido(índiceEjePartido: Int, puntoCorte: Núm) = {
			def loop(indiceEjePartido: Int, intervalos: List[Intervalo]): (List[Intervalo], List[Intervalo]) = {
				if (indiceEjePartido == 0) {
					val (a, b) = intervalos.head.partido(puntoCorte)
					(a :: intervalos.tail, b :: intervalos.tail)
				} else {
					val (a, b) = loop(indiceEjePartido - 1, intervalos.tail)
					(intervalos.head :: a, intervalos.head :: b)
				}
			}
			val (a, b) = loop(índiceEjePartido, intervalos)
			(CuboMd(a), CuboMd(b))
		}
	}

	/**Asocia un valor a cada uno de los vertices de un cubo multidimensional. */
	trait ValsEnVertices[T] {
		/**Dimensión del [[CuboMd]] multidimensional del cual esta instancia conoce el valor en sus vertices */
		val dimension: Int
		/** Da los dos [[ValsEnVertices]] resultantes de dividir el [[CuboMd]] multidimensional en dos con un plano multidimensional perpendicular al eje señalado.
		  * Los valores asociados a los nuevos vertices en el plano de corte son recibos por parámetro.
		  * @param índiceEjePartido índice del eje perpendicular al plano multidimensional de corte
		  * @param valsEnVerticesPlanoCorte valores asociados a los nuevos vertices en el plano de corte.
		  */
		def partido(índiceEjePartido: Int, valsEnVerticesPlanoCorte: ValsEnVertices[T]): (ValsEnVertices[T], ValsEnVertices[T])
	}
	case class Rama[T](dimension: Int, valEnVérticeInf: ValsEnVertices[T], valEnVérticeSup: ValsEnVertices[T]) extends ValsEnVertices[T] {
		override def partido(índiceEjePartido: Int, valsEnVerticesPlanoCorte: ValsEnVertices[T]) = {
			assert(dimension == valsEnVerticesPlanoCorte.dimension + 1)
			if (índiceEjePartido == 0)
				(Rama(dimension, valEnVérticeInf, valsEnVerticesPlanoCorte), Rama(dimension, valsEnVerticesPlanoCorte, valEnVérticeSup))
			else valsEnVerticesPlanoCorte match {
				case Rama(_, valsEnVerticesPlanoCorteInf, valsEnVerticesPlanoCorteSup) =>
					val inf = valEnVérticeInf.partido(índiceEjePartido - 1, valsEnVerticesPlanoCorteInf)
					val sup = valEnVérticeSup.partido(índiceEjePartido - 1, valsEnVerticesPlanoCorteSup)
					(Rama(dimension, inf._1, sup._1), Rama(dimension, inf._2, sup._2))
				case _ =>
					throw new IllegalStateException
			}
		}
	}
	case class Hoja[T](valor: T) extends ValsEnVertices[T] {
		val dimension = 0
		override def partido(índiceEjePartido: Int, valsEnVerticesPlanoCorte: ValsEnVertices[T]) =
			throw new IllegalStateException
	}
	object ValsEnVertices {
		def apply[T](vals: T*): ValsEnVertices[T] = {
			def tejido(vals: Seq[ValsEnVertices[T]]): ValsEnVertices[T] =
				if (vals.size == 1) vals(0)
				else {
					val t = for (par <- vals.sliding(2, 2)) yield {
						if (par.size == 1) throw new IllegalArgumentException("La cantidad de valores recibidos debe ser potencia de dos")
						else Rama(par(0).dimension + 1, par(0), par(1))
					}
					tejido(t.toSeq)
				}
			tejido(vals.map(Hoja(_)))
		}
	}

	trait Func {
		val dimension: Int
		def apply(varsIndep: Vec): Vec
	}

	/** Función que resulta de empalmar dos funciones en el plano multidimensional (de empalme) que corta perpendicularmente a un eje.
	  * @param índiceEjePartido indica cuál es el eje cortado por el plano multidimensional de empalme.
	  * @param puntoCorte es la coordenada del punto en que el plano multidimensional de empalme corta al eje
	  * @param funcInf es la función que aplicara del lado inferior del plano de empalme
	  * @param funcSup es la función que aplicará del lado superior del plano de empalme
	  */
	case class FuncEmpalme(índiceEjePartido: Int, puntoCorte: Núm, funcInf: Func, funcSup: Func) extends Func {
		assert(funcInf.dimension == funcSup.dimension)
		assert(0 <= índiceEjePartido && índiceEjePartido < funcInf.dimension)

		override val dimension = funcInf.dimension
		override def apply(varsIndep: Vec): Vec = {
			if (varsIndep(índiceEjePartido) < puntoCorte) funcInf.apply(varsIndep)
			else funcSup.apply(varsIndep)
		}
	}

	/**Función que da la interpolación/extrapolación lineal de las muestras tomadas en los vertices de un cubo multidimensional */
	case class FuncLineal(cubo: CuboMd, valsEnVertices: ValsEnVertices[Vec]) extends Func {
		assert(cubo.intervalos.size == valsEnVertices.dimension)

		override val dimension = valsEnVertices.dimension
		override def apply(varsIndep: Vec): Vec = {
			if (varsIndep.size != valsEnVertices.dimension) throw new IllegalArgumentException("la dimensión del vector")
			/** Da un término por cada vértice del cubo multidimensional recibido. La dimensión del cubo debe coincidir con la cantidad de variables independientes.
			  * Si hay una sola variable independiente la dimensión del cubo multidimensional será 1, tendrá  dos vértices (los dos extremos de un segmento) y, por ende, dará dos términos;
			  * si hay dos variables independientes la dimensión será 2, tendrá cuatro vértices (las esquinas de un cuadrado) y, por ende, dará 4 términos;
			  * si hay tres variables independientes la dimensión será 3, tendrá ocho vertices (los vertices de un cubo) y, por ende, dará 8 términos.
			  * En general, la cantidad de vértices (y términos) es pow(2,dimensión).
			  * El término correspondiente a un vértice V es el valor de la muestra en V multiplicado por N ponderadores, donde N es la dimensión del cubo multidimensional (y debe coincider con la cantidad de variables independientes).
			  * Sea P el punto determinado por las variables independientes de entrada; el ponderador del vértice V para el eje 'e' es: (IeSup-Pe)/(IeSup-IeInf), donde Pe es la variable independiente asociada al eje 'e'; y IeInf y IeSup son los extremos del intervalo asociado al eje 'e'.
			  */
			def armarTérminos(varsIndep: Vec, intervalos: List[Intervalo], valEnVértice: ValsEnVertices[Vec]): List[Vec] = valEnVértice match {
				case Hoja(v) =>
					assert(varsIndep.isEmpty && intervalos.isEmpty)
					List(v)
				case Rama(_, valsEnVerticesInf, valsEnVerticesSup) =>
					val términosSigInf = armarTérminos(varsIndep.tail, intervalos.tail, valsEnVerticesInf)
					val términosSigSup = armarTérminos(varsIndep.tail, intervalos.tail, valsEnVerticesSup)
					val términosInf = for (f <- términosSigInf)
						yield multiplicaVector((intervalos.head.sup - varsIndep.head) / (intervalos.head.sup - intervalos.head.inf), f)
					val términosSup = for (f <- términosSigSup)
						yield multiplicaVector((varsIndep.head - intervalos.head.inf) / (intervalos.head.sup - intervalos.head.inf), f)
					términosInf ++ términosSup
			}
			armarTérminos(varsIndep, cubo.intervalos, valsEnVertices).reduce(sumaVectores)
		}

		def partir(índiceEjePartido: Int, puntoCorte: Núm, valsEnVerticesPlanoCorte: ValsEnVertices[Vec]): FuncEmpalme = {
			assert(valsEnVertices.dimension == 1 + valsEnVerticesPlanoCorte.dimension)
			val (cuboInf, cuboSup) = cubo.partido(índiceEjePartido, puntoCorte)
			val (valsInf, valsSup) = valsEnVertices.partido(índiceEjePartido, valsEnVerticesPlanoCorte)
			FuncEmpalme(índiceEjePartido, puntoCorte, FuncLineal(cuboInf, valsInf), FuncLineal(cuboSup, valsSup))
		}
	}

	object Func {
		def fill(cubo: CuboMd)(f: Vec => Vec): FuncLineal = {
			def loop(intervalos: List[Intervalo], vec: Queue[Núm]): ValsEnVertices[Vec] = intervalos match {
				case Nil =>
					Hoja(f(vec.toList))
				case h :: t =>
					Rama(intervalos.size, loop(t, vec :+ h.inf), loop(t, vec :+ h.sup))
			}
			FuncLineal(cubo, loop(cubo.intervalos, Queue()))
		}

	}

}

package funcHeteroAproxBin {

}




