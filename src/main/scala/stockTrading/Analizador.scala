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

	val comisiónFija: Dinero = 10d
	val dineroInicial: Dinero = 1000d
	val startingBuyTs = 5
	val startingSellTs = 5


	def simularOperadorPasivo(jornadas: IndexedSeq[Jornada]): Dinero = {
		val s = new SimuladorOperadorNocturno(0, Double.MaxValue)
		s.simular(s.operador,dineroInicial,jornadas)
	}

	def analizarOperadorNocturno(jornadas: IndexedSeq[Jornada]): Resultado = {
		buyTsIterator map (buyTs => {
			sellTsIterator map (sellTs => {
				val s = new SimuladorOperadorNocturno(buyTs / 1000d, sellTs / 1000d)
				s.simular(s.operador,dineroInicial,jornadas)
			}) toIndexedSeq
		}) toIndexedSeq
	}

	def buyTsIterator = Iterator.range(startingBuyTs, 150)

	def sellTsIterator = Iterator.range(startingSellTs, 250)

	def buyTs(outerIndex: Int) = startingBuyTs + outerIndex

	def sellTs(innerIndex: Int) = startingSellTs + innerIndex




	class SimuladorOperadorNocturno(buyTs: Factor, sellTs: Factor) extends Simulador {
		type Estado = EstadoOn

		trait EstadoOn {
			val dinero: Dinero

			def siguiente(jornada: Jornada): EstadoOn

			def dineroFinal(precioFinal: Dinero): Dinero
		}

		
		case class EsperandoComprar(dinero: Dinero, precioMínimo: Dinero) extends EstadoOn {
			val precioUmbral = precioMínimo * (1 + buyTs)

			override def siguiente(jornada: Jornada) = {
				if (jornada.precioApertura >= precioUmbral)
					comprar(jornada.precioApertura)
				else if (jornada.precioMáximo >= precioUmbral)
					comprar(precioUmbral) // asumiendo que la suba es gradual
				else if (jornada.precioMínimo < precioMínimo)
					EsperandoComprar(dinero, jornada.precioMínimo)
				else
					this
			}

			def comprar(precioCompra: Dinero): EstadoOn = {
				val cantComprada: Long = math.round((dinero - comisiónFija) / precioCompra)
				if (cantComprada > 0) {
					val dineroRestante = dinero - comisiónFija - cantComprada * precioCompra
					EsperandoVender(dineroRestante, cantComprada, precioCompra)
				} else
					this
			}

			override def dineroFinal(precioFinal: Dinero) = dinero

			override def toString = "EsperandoComprar(dinero=" + redondear(dinero) + ", umbral=" + redondear(precioUmbral) + ")"
		}

		case class EsperandoVender(dinero: Dinero, cantTítulos: Long, precioMáximo: Dinero) extends EstadoOn {
			val precioUmbral = precioMáximo * (1 - sellTs)

			override def siguiente(jornada: Jornada) = {
				if (jornada.precioApertura <= precioUmbral)
					vender(jornada.precioApertura)
				else if (jornada.precioMínimo <= precioUmbral)
					vender(precioUmbral)
				else if (jornada.precioMáximo > precioMáximo)
					EsperandoVender(dinero, cantTítulos, jornada.precioMáximo)
				else
					this
			}

			def vender(precioVenta: Dinero) = EsperandoComprar(dinero + cantTítulos * precioVenta - comisiónFija, precioVenta)

			override def dineroFinal(precioFinal: Dinero): Dinero = dinero + cantTítulos * precioFinal

			override def toString = "EsperandoVender(dinero=" + redondear(dinero) + ", títulos=" + cantTítulos + ", umbral=" + redondear(precioUmbral) + ")"
		}


		val operador = new Operador {

			override def inicializar(dineroInicial: Dinero): EstadoOn =
				EsperandoComprar(dineroInicial, Double.MaxValue)

			override def operar(estado: EstadoOn, jornada: Jornada): EstadoOn =
				estado.siguiente(jornada)

			override def valorTenencia(estado: EstadoOn, precioCierre:Dinero): Dinero =
				estado.dineroFinal(precioCierre)
		}


	}

	def redondear(dinero: Double): String = {
		"%4.2f" format dinero
	}

}

