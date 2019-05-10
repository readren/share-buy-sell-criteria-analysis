package stockTrading

/**
  * Created by Gustavo on 27/11/2014.
  */


object Analizador {
	type Factor = Double
	type PorMil = Int
	type Dinero = Simulador.Dinero
	type Resultado = IndexedSeq[IndexedSeq[Dinero]]
	type Jornada = Simulador.Jornada
	type Pormiltaje = Int

	val comisiónFija: Dinero = 7d
	val dineroInicial: Dinero = 10000d
	val startingBuyTs = 57
	val startingSellTs = 1
	val inerciaPromediador = 20;


	def simularOperadorPasivo(jornadas: IndexedSeq[Jornada]): Dinero = {
		val s = new SimuladorOperadorNocturno(0, Double.MaxValue, inerciaPromediador)
		s.simular(s.operador, dineroInicial, jornadas)
	}

	def analizarOperadorNocturno(jornadas: IndexedSeq[Jornada]): Resultado = {
		buyTsIterator map (buyTs => {
			sellTsIterator map (sellTs => {
				val s = new SimuladorOperadorNocturno(buyTs / 1000d, sellTs / 1000d, inerciaPromediador)
				s.simular(s.operador, dineroInicial, jornadas)
			}
				) toIndexedSeq
		}
			) toIndexedSeq
	}

	def buyTsIterator: Iterator[PorMil] = Iterator.range(startingBuyTs, 190)

	def sellTsIterator: Iterator[PorMil] = Iterator.range(startingSellTs, 110)

	def buyTs(outerIndex: Int): PorMil = startingBuyTs + outerIndex

	def sellTs(innerIndex: Int): PorMil = startingSellTs + innerIndex

	def redondear(dinero: Double): String = {
		"%4.2f" format dinero
	}


	///////////////////


	def analizarOperadorPuchos(jornadas: IndexedSeq[Jornada], partes: Int): Resultado = {
		val montoCompraPucho = (dineroInicial - 0.01 - comisiónFija * partes) / partes;
		distanciaPormiltualEntreComprasIterator map (distanciaPormiltualEntreCompras => {
			pormiltajeGananciaVentaIterator map (pormiltajeGananciaVenta => {
				val simulador = new SimuladorPuchos(distanciaPormiltualEntreCompras, pormiltajeGananciaVenta, montoCompraPucho); // el -0.01 esta para evitar que el error de representación en Double provoque que se use una parte menos.
				simulador.simular(simulador.operador, dineroInicial, jornadas)
			}
				) toIndexedSeq
		}
			) toIndexedSeq
	}

	def analizarOperadorPuchosV2(jornadas: IndexedSeq[Jornada], partes: Int): Resultado = {
		val montoCompraPucho = (dineroInicial - 0.01 - comisiónFija * partes) / partes;

		distanciaPormiltualEntreComprasIterator map (distanciaPormiltualEntreCompras => {
			pormiltajeGananciaVentaIterator map (pormiltajeGananciaVenta => {
				val simulador = new SimuladorPuchosV2(distanciaPormiltualEntreCompras, pormiltajeGananciaVenta, montoCompraPucho, 72, 24); // el -0.01 esta para evitar que el error de representación en Double provoque que se use una parte menos.
				simulador.simular(simulador.operador, dineroInicial, jornadas)
			}
				) toIndexedSeq
		}
			) toIndexedSeq
	}

	def distanciaPormiltualEntreComprasIterator: Iterator[Int] = Iterator.range(10, 650); // Y
	def pormiltajeGananciaVentaIterator: Iterator[Int] = Iterator.range(10, 1500); // X
}

