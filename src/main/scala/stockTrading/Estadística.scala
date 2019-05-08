package stockTrading

import stockTrading.Simulador.{Dinero, Jornada}

/**
 * Created by Gustavo on 06/12/2014.
 */
class Estadística(jornadas: IndexedSeq[Jornada]) {


	def promedio(días: Int): IndexedSeq[Dinero] = {
		val díasMenosUno = días
		var acumulador: Dinero = Double.NaN
		for (jornada <- jornadas) yield {
			val t = acumulador
			acumulador = if (acumulador.isNaN) jornada.precioCierre
			else (acumulador * díasMenosUno + jornada.precioCierre) / días
			t
		}
	}
}
