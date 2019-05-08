package stockTrading

import java.util.Date

/**
 * Created by Gustavo on 03/12/2014.
 */

object Simulador {
	type Dinero = Double
	case class Jornada(fecha: Date, precioApertura: Dinero, precioMáximo: Dinero, precioMínimo: Dinero, precioCierre: Dinero, volumen: Long, precioCierreAjustado: Dinero) {
		override def toString = s"(open=$precioApertura, max=$precioMáximo, min=$precioMínimo, close=$precioCierre)"
	}
}


abstract class Simulador {
	import stockTrading.Simulador._

	/** Calcula el monto final si si aplicaran los parámetros recibidos */
	def simular(operador:Operador, dineroInicial:Dinero, jornadas: IndexedSeq[Jornada]): Dinero = {
		val estadoFinal = jornadas.foldLeft(operador.inicializar(dineroInicial))(operador.operar)
		operador.valorTenencia(estadoFinal,jornadas.last.precioCierre)
	}


	protected type Estado

	protected trait Operador {
		def inicializar(dineroInicial:Dinero):Estado
		def operar(estado:Estado, jornada:Jornada):Estado
		def valorTenencia(estado:Estado, precioFinal:Dinero):Dinero
	}

}
