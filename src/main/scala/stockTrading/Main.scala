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

	private def encabezados: Parser[List[String]] = repsep(letras, ',')

	private def dígitos: Parser[String] = acceptIf(_.isDigit)(c => s"se buscaban dígitos pero se encontró '$c'").+ ^^ (_.mkString)

	private def numEntero: Parser[Int] = dígitos ^^ (_.toInt)

	private def numReal: Parser[Double] = dígitos ~ '.' ~ dígitos ^^ { case pe ~ _ ~ pd => (pe + '.' + pd).toDouble}

	private def numRealSep: Parser[Double] = ',' ~> numReal

	private def fecha: Parser[Date] = numEntero ~ '-' ~ numEntero ~ '-' ~ numEntero ^^ { case año ~ _ ~ mes ~ _ ~ día =>
		val calendar = Calendar.getInstance()
		calendar.set(año, mes, día)
		calendar.getTime
	}

	private def jornada: Parser[Jornada] = fecha ~ numRealSep ~ numRealSep ~ numRealSep ~ numRealSep ~ (',' ~> numEntero) ~ numRealSep ^^ { case date ~ open ~ high ~ low ~ close ~ volume ~ adjClose => Jornada(date, open, high, low, close, volume, adjClose)}

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
		val source = Source.fromFile("C:/Users/Gustavo/Desktop/sharesAnalisis/historical prices IBB.csv")

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
				})

				val jornadasSinSplitsRevertida = jornadasSinSplits.reverse.toIndexedSeq
				println("Parámetros: dineroInicial="+Analizador.dineroInicial+", comisiónFija="+Analizador.comisiónFija)
				val dineroFinalSinTrucos = Analizador.simularOperadorPasivo(jornadasSinSplitsRevertida)
				println("DineroFinal sin trucos="+dineroFinalSinTrucos)
				val análisis = Analizador.analizarOperadorNocturno(jornadasSinSplitsRevertida)
				Presentador.mostrarPerformanceRelativaEnFunciónDe_BuyTs_Y_SellTs(análisis)
				Presentador.mostrarParámetrosEnFunciónDePerformance(análisis)
		}
	}

}

