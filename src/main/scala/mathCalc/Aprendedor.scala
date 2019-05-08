package mathCalc

/**
 * Created by Gustavo on 07/12/2014.
 */

object Aprendedor {
	type Num = Float

	case class Var(min: Num, max: Num, estimado: Num, cantPuntos: Int)



}

/** Propósito: hallar la funcion (buyTs,sellTs)=f(m1,m2) tal que usada en el simulador de operador con datos historicos logre la mayor cantidad de dinero
 *  Empezar probando las funciones que resultan de unir los puntos de una grilla de 2x2, y recordar los maximos aislados.
 *  Luego agregar los puntos intermedios a la grilla quedando de 3x3, mejorar la aproximación cerca de los máximos hallados en el paso anterior, y recordar los nuevos máximos aislados
 *  Luego agregar los puntos intermedios a la grilla quedando de 5x5, mejorar la aproximación cerca de los máximos hallados en el paso anterior, y recordar los nuevos máximos aislados
 *  Asi susecivamente 
 */

import mathCalc.Aprendedor._

class Aprendedor(varsEntrada: IndexedSeq[Var], varsSalida: IndexedSeq[Var]) {
  
//	val grillaEntrada: Grilla = {
//		var grilla: Grilla = grillaSalidaInicial
//		val iterador = varsEntrada.reverseIterator
//		while (iterador.hasNext) {
//			val h = IndexedSeq.fill(iterador.next.cantPuntos)(grilla)
//			grilla = mathCalc.Rama(h)
//		}
//		grilla
//	}
//
//	val grillaSalidaInicial: Grilla = ???
//
//	def f(entradas: IndexedSeq[T]): IndexedSeq[T] = {
//
//	}

}
