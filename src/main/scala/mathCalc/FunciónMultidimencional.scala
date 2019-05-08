package mathCalc

object FunciónMultidimensional {

	/** Número digitalizado según una {@link ParticiónEje} */
	case class NúmDigitalizado(índiceInf: Int, índiceSup: Int, fracción: Núm)

	/** Eje particionado. */
	case class ParticiónEje(partición: IndexedSeq[Núm]) {
		val índiceMax = partición.size - 1

		/** Mapea el valor recibido hacia el índice de la partición que contiene al punto, el índice de la siguiente partición, y la distancia relativa entre el comienzo de la partición y el valor recibido respecto del ancho de la particion. */
		def digitalizar(v: Núm): NúmDigitalizado = {
			val índiceSup = partición.indexWhere(_ > v)
			if (índiceSup == -1) NúmDigitalizado(índiceMax - 1, índiceMax, 1)
			else if (índiceSup == 0) NúmDigitalizado(0, 1, 0)
			else {
				val índiceInf = índiceSup - 1
				val valAnterior = partición(índiceInf)
				val valSiguiente = partición(índiceSup)
				val fracción = (v - valAnterior) / (valSiguiente - valAnterior)
				NúmDigitalizado(índiceInf, índiceSup, fracción)
			}
		}
	}


	case class IntervaloParticionado(inicio: Núm, fin: Núm, divisiones: Int) {
		if( inicio >= fin) throw new IllegalArgumentException("inicio debe ser menor a fin")
	}

	def fill(intervalo: List[IntervaloParticionado])(f: Vec => Vec) = {
		val capacidad = intervalo.map(_.divisiones + 1)
		val ejes = for (ip <- intervalo) yield ParticiónEje(ip.inicio to ip.fin by (ip.fin - ip.inicio) / ip.divisiones)
		def h(coordenadas: List[Int]): Vec =
			for ((cordenada, eje) <- coordenadas.zip(ejes)) yield eje.partición(cordenada)
		val grilla = GrillaMultidimensional.fill(capacidad)(x => f(h(x)))
		new FunciónMultidimensional(grilla, ejes)
	}

}

/** Función multidimencional respaldada por una GrillaMultidimensional. El mapeo entre vector y coordenadas de la grilla lo determina particiónEjes */
case class FunciónMultidimensional(grilla: GrillaMultidimensional[Vec], particionesEjes: List[FunciónMultidimensional.ParticiónEje]) {
	if( grilla.dimensión != particionesEjes.size) throw new IllegalArgumentException("la cantidad de ejes debe ser igual a la dimensión de la grilla")
	import mathCalc.FunciónMultidimensional._

import scala.collection.immutable.Queue

	def apply(varsIndependientes: Vec): Vec = {
		if( varsIndependientes.size != grilla.dimensión) throw new IllegalArgumentException("la dimensión del vector recibido difiere de la dimensión de la grilla")
		val varsIndepDig: List[NúmDigitalizado] = for {(vi, eje) <- varsIndependientes.zip(particionesEjes)} yield eje.digitalizar(vi)

		/** Da un término por cada vértice del cubo multidimensional mas pequeño tal que sus vértices esten mapeados en la grilla de respaldo, y contenga al punto P; donde P es el vector de entrada formado por las variables independientes (cada variable es una componente del vector).
		  * Si la dimensión de entrada es 1 el cubo multidimensional (un segmento) tiene dos vértices (los dos extremos) y, por ende, dará dos términos;
		  * si la dimensión es 2, el cubo multidimensional (un cuadrado) tiene 4 vértices y, por ende, dará 4 términos;
		  * si es 3, el cubo (un cubo) tiene 8 vértices y, por ende, dará 8 términos.
		  * En general, la cantidad de vérticis (y términos) es pow(2,dimensión).
		  * El término correspondiente a un vértice V es el valor de la grilla de respaldo en el vértice V multiplicado por N ponderadores, donde N es la dimensión del espacio de entrada (cantidad de variables independientes).
		  * Sea P el punto determinado por las variables independientes de entrada; el ponderador del vértice V para el eje E es: 1 - (|(P-V).E| / AE), donde AE es el ancho del cubo en el eje E  */
		def armarTérminos(coordenadas: Queue[Int], varsIndepDig: List[NúmDigitalizado]): List[Vec] =
			varsIndepDig match {
				case Nil => List(grilla.get(coordenadas.toList))
				case varIndepDig :: siguientes =>
					val coordenadasInf = coordenadas :+ varIndepDig.índiceInf
					val coordenadasSup = coordenadas :+ varIndepDig.índiceSup
					val factoresSiguientesInf = armarTérminos(coordenadasInf, siguientes)
					val factoresInf = for (factorSiguiente <- factoresSiguientesInf) yield multiplicaVector(1 - varIndepDig.fracción, factorSiguiente)
					val factoresSiguientesSup = armarTérminos(coordenadasSup, siguientes)
					val factoresSup = for (factorSiguiente <- factoresSiguientesSup) yield multiplicaVector(varIndepDig.fracción, factorSiguiente)
					factoresInf ++ factoresSup
			}
		val términos = armarTérminos(Queue(), varsIndepDig)
		términos.reduce(sumaVectores)
	}

}