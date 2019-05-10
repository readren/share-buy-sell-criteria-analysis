package stockTrading

import stockTrading.Analizador.{Dinero, Factor, Jornada, redondear}


class SimuladorOperadorNocturno(buyTs: Factor, sellTs: Factor, inerciaPromediador: Int) extends Simulador {
	type Estado = EstadoOn

	def promediar(promedio: Dinero, precioCierre: Dinero): Dinero = (promedio * inerciaPromediador + precioCierre) / (inerciaPromediador + 1)

	trait EstadoOn {
		val dinero: Dinero;

		def siguiente(jornada: Jornada): EstadoOn

		def dineroFinal(precioFinal: Dinero): Dinero
	}


	case class Inicial(dinero: Dinero) extends EstadoOn {
		override def siguiente(jornada: Jornada): EstadoOn =
			EsperandoComprar(dinero, jornada.precioCierre, jornada.precioMínimo);

		override def dineroFinal(precioFinal: Dinero): Dinero =
			dinero;
	}

	case class EsperandoComprar(dinero: Dinero, promedioPrecioCierre: Dinero, precioMínimo: Dinero) extends EstadoOn {
		val precioUmbral: Factor = precioMínimo * (1 + buyTs)

		override def siguiente(jornada: Jornada): EstadoOn = {
			val nuevoPromedio = promediar(promedioPrecioCierre, jornada.precioCierre);
			if (jornada.precioApertura >= precioUmbral)
				comprar(jornada.precioApertura, nuevoPromedio)
			else if (jornada.precioMáximo >= precioUmbral)
				comprar(precioUmbral, nuevoPromedio) // asumiendo que la suba es gradual
			else if (jornada.precioMínimo < precioMínimo)
				EsperandoComprar(dinero, nuevoPromedio, jornada.precioMínimo)
			else
				this
		}

		private def comprar(precioCompra: Dinero, promedio: Dinero): EstadoOn = {
			val cantComprada: Long = math.round(math.floor((dinero - Analizador.comisiónFija) / precioCompra))
			if (cantComprada > 0) {
				val dineroRestante = dinero - Analizador.comisiónFija - cantComprada * precioCompra
				EsperandoVender(dineroRestante, promedio, cantComprada, precioCompra)
			} else
				this
		}

		override def dineroFinal(precioFinal: Dinero): Dinero = dinero

		override def toString: String = "EsperandoComprar(dinero=" + redondear(dinero) + ", umbral=" + redondear(precioUmbral) + ")"
	}

	case class EsperandoVender(dinero: Dinero, promedioPrecioCierre: Dinero, cantTítulos: Long, precioMáximo: Dinero) extends EstadoOn {
		val precioUmbral: Factor = precioMáximo * (1 - sellTs)

		override def siguiente(jornada: Jornada): EstadoOn = {
			val nuevoPromedio = promediar(promedioPrecioCierre, jornada.precioCierre);
			if (jornada.precioApertura <= precioUmbral)
				vender(jornada.precioApertura, nuevoPromedio)
			else if (jornada.precioMínimo <= precioUmbral)
				vender(precioUmbral, nuevoPromedio)
			else if (jornada.precioMáximo > precioMáximo)
				EsperandoVender(dinero, nuevoPromedio, cantTítulos, jornada.precioMáximo)
			else
				this
		}

		private def vender(precioVenta: Dinero, promedio: Dinero) =
			EsperandoComprar(dinero + cantTítulos * precioVenta - Analizador.comisiónFija, promedio, precioVenta)

		override def dineroFinal(precioFinal: Dinero): Dinero = dinero + cantTítulos * precioFinal

		override def toString: String = "EsperandoVender(dinero=" + redondear(dinero) + ", títulos=" + cantTítulos + ", umbral=" + redondear(precioUmbral) + ")"
	}


	val operador: Operador = new Operador {

		override def inicializar(dineroInicial: Dinero): EstadoOn =
			Inicial(dineroInicial)

		override def operar(estado: EstadoOn, jornada: Jornada): EstadoOn =
			estado.siguiente(jornada)

		override def valorTenencia(estado: EstadoOn, precioCierre: Dinero): Dinero =
			estado.dineroFinal(precioCierre)
	}


}
