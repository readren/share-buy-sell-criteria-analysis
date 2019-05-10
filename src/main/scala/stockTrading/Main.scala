package stockTrading


import java.util.{Calendar, Date}

import Simulador.Jornada

import scala.collection.immutable.PagedSeq
import scala.io.Source
import scala.util.parsing.combinator.Parsers
import scala.util.parsing.input.{PagedSeqReader, Reader}

class JornadasParsers extends Parsers {
	override type Elem = Char
	type Jornada = Simulador.Jornada

	private def letras: Parser[String] = rep1(acceptIf(c => c.isLetter || c.isSpaceChar)(c => s"se buscaban letras y se encontró '$c'")) ^^ (_.mkString)

	private def palabrasEntreComillas: Parser[String] = '"' ~ letras ~ '"' ^^ { case _ ~ x ~ _ => x }

	private def encabezados: Parser[List[String]] = repsep(palabrasEntreComillas, ',')

	private def dígitos: Parser[String] = acceptIf(_.isDigit)(c => s"se buscaban dígitos pero se encontró '$c'").+ ^^ (_.mkString)

	private def numEntero: Parser[Int] = dígitos ^^ (_.toInt)

	private def numEnteroEntreComillas: Parser[Double] = '"' ~ numEntero ~ '"' ^^ { case _ ~ x ~ _ => x }

	private def numReal: Parser[Double] = dígitos ~ '.' ~ dígitos ^^ { case pe ~ _ ~ pd => (pe + '.' + pd).toDouble }

	private def numRealEntreComillas: Parser[Double] = '"' ~ numReal ~ '"' ^^ { case _ ~ x ~ _ => x }

	private def numRealSep: Parser[Double] = ',' ~> numRealEntreComillas

	private def fecha: Parser[Date] = numEntero ~ '/' ~ numEntero ~ '/' ~ numEntero ^^ { case año ~ _ ~ mes ~ _ ~ día =>
		val calendar = Calendar.getInstance()
		calendar.set(año, mes, día)
		calendar.getTime
	}

	private def fechaEntreComillas: Parser[Date] = '"' ~ fecha ~ '"' ^^ { case _ ~ x ~ _ => x }

	private def jornada: Parser[Jornada] = fechaEntreComillas ~ numRealSep ~ numRealSep ~ numRealSep ~ numRealSep ~ numRealSep ^^ { case date ~ close ~ volume ~ open ~ high ~ low => Jornada(date, open, high, low, close, volume.toLong, close) }

	private def jornadas: Parser[List[Jornada]] = phrase(repsep(jornada, '\n'))

	private def tabla: Parser[List[Jornada]] = encabezados ~ '\n' ~> jornadas

	def aplicar(reader: Reader[Char]): Either[Any, List[Jornada]] = tabla.apply(reader) match {
		case Success(r, _) => Right(r)
		case f: Failure => Left(f)
		case e: Error => Left(e)
	}

}

/**
  * Created by Gustavo on 25/11/2014.
  */
object Main extends App {

	{
		//		val source = Source.fromFile("C:/Users/Gustavo/Downloads/historical prices ibb.csv")
		val source = Source.fromFile("C:\\Users\\gusta\\Downloads\\HistoricalQuotes-BRZU-desde2017.csv")

		val reader = new PagedSeqReader(PagedSeq.fromSource(source))
		val myParsers = new JornadasParsers
		myParsers.aplicar(reader) match {
			case Left(mensaje) => println(mensaje)
			case Right(jornadas) =>
				var precioCierreSiguiente = Double.NaN
				var precioCierreAjustadoSiguiente = Double.NaN
				var factorSplit = 1L
				val jornadasSinSplits = jornadas.map(jornada => {
					if (!precioCierreSiguiente.isNaN
						&& jornada.precioCierre * precioCierreAjustadoSiguiente / (jornada.precioCierreAjustado * precioCierreSiguiente) > 1.9)
						factorSplit *= math.round((jornada.precioCierre / precioCierreSiguiente) / (jornada.precioCierreAjustado / precioCierreAjustadoSiguiente))
					precioCierreSiguiente = jornada.precioCierre
					precioCierreAjustadoSiguiente = jornada.precioCierreAjustado
					new Jornada(jornada.fecha, jornada.precioApertura / factorSplit, jornada.precioMáximo / factorSplit, jornada.precioMínimo / factorSplit, jornada.precioCierre / factorSplit, jornada.volumen, jornada.precioCierreAjustado)
				}
				)

				val jornadasSinSplitsRevertida = jornadasSinSplits.reverse.toIndexedSeq
				if (false) {
					println("Parámetros: dineroInicial=" + Analizador.dineroInicial + ", comisiónFija=" + Analizador.comisiónFija)
					val dineroFinalSinTrucos = Analizador.simularOperadorPasivo(jornadasSinSplitsRevertida)
					println("DineroFinal sin trucos=" + dineroFinalSinTrucos)
					val análisisNocturno = Analizador.analizarOperadorNocturno(jornadasSinSplitsRevertida)
					Presentador.mostrarPerformanceRelativa(análisisNocturno, () => Analizador.sellTsIterator, () => Analizador.buyTsIterator)
					//					Presentador.mostrarParámetrosEnFunciónDePerformance(análisisNocturno)
				}
				if (false) {
					println("------- Nocturno - Progreso de un caso ---------")
					val s = new SimuladorOperadorNocturno(0.072, 0.023, Analizador.inerciaPromediador)
					val operador = s.operador
					val estadoFinal = jornadasSinSplitsRevertida.foldLeft(operador.inicializar(Analizador.dineroInicial)) { (estado, jornada) =>
						println(f"$jornada%60s \t\t$estado")
						operador.operar(estado, jornada);
					}
					val tenenciasValorizadas = operador.valorTenencia(estadoFinal, jornadasSinSplits.head.precioCierre);
					println(s"tenencias valorizadas: $tenenciasValorizadas");
				}


				if (false) {
					println("------- Puchos V1 - Comparación parámetros ---------")
					val análisisPuchos = Analizador.analizarOperadorPuchos(jornadasSinSplitsRevertida, 4);
					Presentador.mostrarPerformanceRelativa(análisisPuchos, () => Analizador.pormiltajeGananciaVentaIterator, () => Analizador.distanciaPormiltualEntreComprasIterator)
				}
				if (false) {
					println("------- Puchos V1 - Progreso de un caso ---------")
					val partes = 2;
					val montoCompraPucho = (Analizador.dineroInicial - 0.01 - Analizador.comisiónFija * partes) / partes;
					val s = new SimuladorPuchos(397, 190, montoCompraPucho)
					val operador = s.operador

					val estadoFinal = jornadasSinSplitsRevertida.foldLeft(operador.inicializar(Analizador.dineroInicial)) { (estado, jornada) =>
						val nuevoEstado = operador.operar(estado, jornada);
						println(f"$jornada%60s \t\t$nuevoEstado")
						nuevoEstado
					}
					val tenenciasValorizadas = operador.valorTenencia(estadoFinal, jornadasSinSplits.head.precioCierre);
					println(s"tenencias valorizadas: $tenenciasValorizadas");
				}

				if (false) {
					println("------- Puchos V2 - Comparación parámetros ---------")
					val análisisPuchos = Analizador.analizarOperadorPuchosV2(jornadasSinSplitsRevertida, 1);
					Presentador.mostrarPerformanceRelativa(análisisPuchos, () => Analizador.pormiltajeGananciaVentaIterator, () => Analizador.distanciaPormiltualEntreComprasIterator)
				}
				if (true) {
					println("------- Puchos V2 - Progreso de un caso ---------")
					val partes = 1;
					val montoCompraPucho = (Analizador.dineroInicial - 0.01 - Analizador.comisiónFija * partes) / partes;
//					val s = new SimuladorPuchosV2(388, 818, montoCompraPucho, 72, 24)
					val s = new SimuladorPuchosV2(368, 1499, montoCompraPucho, 72, 24)
					val operador = s.operador

					val estadoFinal = jornadasSinSplitsRevertida.foldLeft(operador.inicializar(Analizador.dineroInicial)) { (estado, jornada) =>
						println(f"$jornada%60s \t\t$estado")
						operador.operar(estado, jornada);
					}
					val tenenciasValorizadas = operador.valorTenencia(estadoFinal, jornadasSinSplits.head.precioCierre);
					println(s"tenencias valorizadas: $tenenciasValorizadas");
				}

		}
	}

}

