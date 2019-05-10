package stockTrading

import stockTrading.Analizador.{Dinero, Factor, Jornada, Pormiltaje}

object SimuladorPuchosV2 {

	private val INERCIA_PROMEDIADOR: Double = 20;

	abstract class EstadoSP {
		def siguiente(jornada: Jornada): EstadoSP;
		def dineroFinal(precioFinal: Dinero): Dinero;
	};

	case class Pucho(cantTítulos: Long, precioComprado: Dinero, precioActivaciónVenta: Dinero);
}

class SimuladorPuchosV2(
	distanciaEntreCompras: Pormiltaje,
	gananciaVenta: Pormiltaje,
	montoCompraPucho: Dinero,
	buyTrailingStop: Pormiltaje,
	sellTrailingStop: Pormiltaje
) extends Simulador {
	import SimuladorPuchosV2._


	private val factorUmbralCompraPucho: Factor = (1000 - distanciaEntreCompras) / 1000d;
	private val factorUmbralVentaPucho: Factor = (1000 + gananciaVenta) / 1000d;
	private val factorUmbralBuyTrailingStop: Factor = (1000 + buyTrailingStop) / 1000d;
	private val factorUmbralSellTrailingStop: Factor = (1000 - sellTrailingStop) / 1000d;
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
				ConPuchosPasivo(
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

	case class ConPuchosPasivo(dinero: Dinero, cantResignaciones: Int, puchos: List[Pucho]) extends EstadoSP {

		override def siguiente(jornada: Jornada): Estado = {

			val puchoMasBajo = this.puchos.head;
			if (jornada.precioApertura >= puchoMasBajo.precioActivaciónVenta)
				ConPuchosActivo(dinero, cantResignaciones, puchos, jornada.precioApertura).siguiente(jornada);
			else if (jornada.precioMáximo >= puchoMasBajo.precioActivaciónVenta)
				ConPuchosActivo(dinero, cantResignaciones, puchos, jornada.precioMáximo)
			else {
				val precioCompraNuevoPucho = puchoMasBajo.precioComprado * factorUmbralCompraPucho;
				if (jornada.precioMínimo <= precioCompraNuevoPucho) {
					if (this.dinero < costoNuevoPucho) {
						this.puchoMasAltoResignado(precioCompraNuevoPucho)
							.siguiente(jornada.copy(precioMáximo = 0)); // se usa recursión para considerar el caso que haya que resignar más de un Pucho. El preció máximo de la jornada es anulado para no comprar dentro de la recursión
					} else {
						val (nuevoDinero, nuevoPucho) = armaPucho(this.dinero, precioCompraNuevoPucho);
						ConPuchosPasivo(nuevoDinero, this.cantResignaciones, nuevoPucho :: this.puchos)
					}
				} else {
					this
				}
			}
		}

		private def puchoMasAltoResignado(nuevoPrecioComprado: Dinero): ConPuchosPasivo = {
			val nuevoPrecioVenta = nuevoPrecioComprado * factorUmbralVentaPucho;
			val puchoMasAlto :: puchosRestantesDeMayorAMenor = this.puchos.reverse;
			val puchoResignado = puchoMasAlto.copy(precioComprado = nuevoPrecioComprado, precioActivaciónVenta = nuevoPrecioVenta)
			val puchosRestantesDeMenorAMayor = puchosRestantesDeMayorAMenor.reverse;
			assert(puchosRestantesDeMenorAMayor.isEmpty || puchoResignado.precioComprado < puchosRestantesDeMenorAMayor.head.precioComprado)
			ConPuchosPasivo(
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

	case class ConPuchosActivo(dinero: Dinero, cantResignaciones: Int, puchos: List[Pucho], precioMáxAlcanzado: Dinero) extends EstadoSP {
		val umbralVenta: Factor = precioMáxAlcanzado * factorUmbralSellTrailingStop

		override def siguiente(jornada: Jornada): Estado = {

			if (jornada.precioApertura <= umbralVenta) {
				this.vender(jornada.precioApertura, jornada);
			} else if (jornada.precioMínimo <= umbralVenta) {
				this.vender(umbralVenta, jornada);
			} else {
				this.copy(precioMáxAlcanzado = jornada.precioMáximo)
			};
		}

		private def vender(precioVenta: Dinero, jornada: Jornada): EstadoSP = {
			val puchoMasBajo :: puchosRestantes = this.puchos;
			val nuevoDinero = this.dinero + puchoMasBajo.cantTítulos * precioVenta - Analizador.comisiónFija;

			puchosRestantes match {
				case Nil =>
					SinPuchos(nuevoDinero, cantResignaciones, jornada.precioCierre);

				case nuevoPuchoMasBajo :: _ =>
					if(precioVenta > nuevoPuchoMasBajo.precioActivaciónVenta)
						ConPuchosActivo(nuevoDinero, cantResignaciones, puchosRestantes, jornada.precioMáximo).vender(precioVenta, jornada);
					else
						ConPuchosPasivo(nuevoDinero, cantResignaciones, puchosRestantes)
			}
		}

		override def dineroFinal(precioFinal: Dinero): Dinero = {
			puchos.foldLeft(this.dinero) { (acum, p) =>
				acum + p.cantTítulos * precioFinal - Analizador.comisiónFija
			}
		}
	}

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
