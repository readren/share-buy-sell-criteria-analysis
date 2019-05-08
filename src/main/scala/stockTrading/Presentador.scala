package stockTrading

/**
 * Created by Gustavo on 30/11/2014.
 */
object Presentador {

	def mostrarPerformanceRelativaEnFunciónDe_BuyTs_Y_SellTs(análisis: Analizador.Resultado): Unit = {
		var menor = Double.MaxValue
		var mayor = Double.MinValue

		val cuadro = (for ((fila, outerIndex) <- análisis.iterator.zipWithIndex) yield {
			val buyTs = Analizador.buyTs(outerIndex)
			for ((dineroFinal, innerIndex) <- fila.iterator.zipWithIndex) yield {
				val sellTs = Analizador.sellTs(innerIndex)
				if (buyTs <= sellTs) {
					if (dineroFinal < menor) menor = dineroFinal
					if (dineroFinal > mayor) mayor = dineroFinal
					dineroFinal
				} else
					dineroFinal
			}

		} toIndexedSeq) toIndexedSeq

		println(s"mayor=$mayor, menor=$menor")
		print("      ")
		for (sellTs <- Analizador.sellTsIterator) print(sellTs / 100)
		println()
		print("      ")
		for (sellTs <- Analizador.sellTsIterator) print((sellTs / 10) % 10)
		println()
		print("      ")
		for (sellTs <- Analizador.sellTsIterator) print(sellTs % 10)
		println()
		println()
		for ((fila, outerIndex) <- cuadro.iterator.zipWithIndex) {
			print("%3d ~ " format Analizador.buyTs(outerIndex))
			for ((dineroFinal, innerIndex) <- fila.iterator.zipWithIndex) {
				//				if (Analizador.buyTs(outerIndex) <= Analizador.sellTs(innerIndex))
				print(rango(dineroFinal, menor, mayor))
				//				else
				//					print(' ')
			}
			println()
		};

	}

	def mostrarParámetrosEnFunciónDePerformance(análisis: Analizador.Resultado): Unit = {

		val h = for ((fila, outerIndex) <- análisis.iterator.zipWithIndex; (dineroFinal, innerIndex) <- fila.iterator.zipWithIndex) yield {
			(Analizador.buyTs(outerIndex), Analizador.sellTs(innerIndex), dineroFinal)
		}
		val x = h.toIndexedSeq.sortWith(_._3 > _._3)
		println(s"x=$x")

	}

	private def rango(x: Double, menor: Double, mayor: Double): Char = {
		if (x == mayor) 'M'
		else {
			val r = math.floor(10d * (x - menor) / (mayor - menor)).toInt
			if (r < 0) '-'
			else if (r >= 10) '+'
			else ('0' + r).toChar
		}
	}
}
