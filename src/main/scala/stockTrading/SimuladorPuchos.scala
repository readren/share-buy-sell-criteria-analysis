package stockTrading

import stockTrading.Analizador.{Dinero, Factor, Jornada, Pormiltaje}


object SimuladorPuchos {

	private val INERCIA_PROMEDIADOR: Double = 20;

	abstract class EstadoSP {
		def siguiente(jornada: Jornada): EstadoSP;
		def dineroFinal(precioFinal: Dinero): Dinero;
	};


}


class SimuladorPuchos(distanciaPormiltualEntreCompras: Pormiltaje, pormiltajeGananciaVenta: Pormiltaje, montoCompraPucho: Dinero) extends Simulador {
	import SimuladorPuchos._

	private val factorUmbralCompraPucho: Factor = (1000D - distanciaPormiltualEntreCompras) / 1000D;
	private val factorUmbralVentaPucho: Factor = (1000D + pormiltajeGananciaVenta) / 1000D;
	private val costoNuevoPucho: Dinero = montoCompraPucho + Analizador.comisiónFija;

	override type Estado = EstadoSP;

	case class Inicial(dinero: Dinero) extends EstadoSP {
		override def siguiente(jornada: Jornada): EstadoSP =
			SinPuchos(dinero, 0, jornada.precioCierre);

		override def dineroFinal(precioFinal: Dinero): Dinero =
			this.dinero;
	}

	case class SinPuchos(dinero: Dinero, cantResignaciones: Int, promedioMóvilCierres: Dinero) extends EstadoSP {
		def siguiente(jornada: Jornada): EstadoSP = {
			val umbralCompraNuevoPucho = promedioMóvilCierres * factorUmbralCompraPucho;
			if (jornada.precioMínimo <= umbralCompraNuevoPucho && dinero >= Analizador.comisiónFija * 100) {
				val precioCompraNuevoPucho = if(jornada.precioApertura <= umbralCompraNuevoPucho) jornada.precioApertura else umbralCompraNuevoPucho
				val (nuevoDinero, nuevoPucho) = armaPucho(this.dinero, precioCompraNuevoPucho)
				ConPuchos(
					nuevoDinero,
					cantResignaciones,
					nuevoPucho :: Nil
				)
			} else {
				SinPuchos(
					this.dinero,
					this.cantResignaciones,
					(promedioMóvilCierres * INERCIA_PROMEDIADOR + jornada.precioCierre) / (INERCIA_PROMEDIADOR + 1)
				)
			}
		}

		def dineroFinal(precioFinal: Dinero): Dinero = {
			dinero
		};
	}

	case class ConPuchos(dinero: Dinero, cantResignaciones: Int, puchos: List[Pucho]) extends EstadoSP {
		val minPrecioComprado: Dinero = puchos.head.precioComprado

		override def siguiente(jornada: Jornada): Estado = {
			val puchoMasBajo = this.puchos.head;
			val puchosRestantes = this.puchos.tail;
			if (jornada.precioMáximo >= puchoMasBajo.precioVenta) {
				val precioVendido = if(jornada.precioApertura>puchoMasBajo.precioVenta) jornada.precioApertura else puchoMasBajo.precioVenta;
				val nuevoDinero = this.dinero + puchoMasBajo.cantTítulos * precioVendido - Analizador.comisiónFija;
				if (puchosRestantes.isEmpty)
					SinPuchos(nuevoDinero, cantResignaciones, jornada.precioCierre);
				else {
					ConPuchos(nuevoDinero, cantResignaciones, puchosRestantes).siguiente(jornada);
				}
			} else {
				val precioCompraNuevoPucho = minPrecioComprado * factorUmbralCompraPucho;
				if (jornada.precioMínimo <= precioCompraNuevoPucho) {
					if (this.dinero < costoNuevoPucho) {
						this.puchoMasAltoResignado(precioCompraNuevoPucho)
							.siguiente(jornada.copy(precioMáximo = 0)); // se usa recursión para considerar el caso que haya que resignar más de un Pucho. El preció máximo de la jornada es anulado para no comprar dentro de la recursión
					} else {
						val (nuevoDinero, nuevoPucho) = armaPucho(this.dinero, precioCompraNuevoPucho);
						ConPuchos(nuevoDinero, this.cantResignaciones, nuevoPucho :: this.puchos)
					}
				} else {
					this
				}
			}
		}

		private def puchoMasAltoResignado(nuevoPrecioComprado: Dinero): ConPuchos = {
			val nuevoPrecioVenta = nuevoPrecioComprado * factorUmbralVentaPucho;
			val puchoMasAlto :: puchosRestantesDeMayorAMenor = this.puchos.reverse;
			val puchoResignado = puchoMasAlto.copy(precioComprado = nuevoPrecioComprado, precioVenta = nuevoPrecioVenta)
			val puchosRestantesDeMenorAMayor = puchosRestantesDeMayorAMenor.reverse;
			assert(puchosRestantesDeMenorAMayor.isEmpty || puchoResignado.precioComprado < puchosRestantesDeMenorAMayor.head.precioComprado)
			ConPuchos(
				this.dinero,
				this.cantResignaciones + 1,
				puchoResignado :: puchosRestantesDeMenorAMayor
			)
		}

		override def dineroFinal(precioFinal: Dinero): Dinero = {
			puchos.foldLeft(this.dinero) { (acum, p) =>
				acum + p.cantTítulos * precioFinal - Analizador.comisiónFija
			}
		}
	}

	case class Pucho(cantTítulos: Long, precioComprado: Double, precioVenta: Double)



	private def armaPucho(dinero: Dinero, precioCompraNuevoPucho: Dinero): (Dinero, Pucho) = {
		val montoCompra = math.min(dinero - Analizador.comisiónFija, montoCompraPucho);
		val cantTítulosNuevoPucho = math.round(math.floor(montoCompra / precioCompraNuevoPucho));
		val nuevoPucho = Pucho(
			cantTítulosNuevoPucho,
			precioCompraNuevoPucho,
			precioCompraNuevoPucho * factorUmbralVentaPucho
		);
		val nuevoDinero = dinero - Analizador.comisiónFija - cantTítulosNuevoPucho * precioCompraNuevoPucho;
		(nuevoDinero, nuevoPucho)
	}



	val operador: Operador = new Operador {

		override def inicializar(dineroInicial: Dinero): Estado =
			Inicial(dineroInicial)

		override def operar(estado: Estado, jornada: Jornada): Estado =
			estado.siguiente(jornada)

		override def valorTenencia(estado: Estado, precioCierre: Dinero): Dinero =
			estado.dineroFinal(precioCierre)
	}
}
