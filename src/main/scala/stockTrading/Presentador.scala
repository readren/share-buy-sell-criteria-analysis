package stockTrading

/**
  * Created by Gustavo on 30/11/2014.
  */
object Presentador {

	def mostrarPerformanceRelativa(análisis: Analizador.Resultado, ctorIteradorÍndiceX: () => Iterator[Int], ctorIteradorÍndiceY: () => Iterator[Int]): Unit = {
		var menor = Double.MaxValue
		var mayor = Double.MinValue

		for (fila <- análisis) {
			for (dineroFinal <- fila) {
				if (dineroFinal < menor) menor = dineroFinal
				if (dineroFinal > mayor) mayor = dineroFinal
			}
		}

		println(s"mayor=$mayor, menor=$menor")
		print("      ")
		for (sellTs <- ctorIteradorÍndiceX()) print((sellTs / 1000) % 10)
		println()
		print("      ")
		for (sellTs <- ctorIteradorÍndiceX()) print((sellTs / 100) % 10)
		println()
		print("      ")
		for (sellTs <- ctorIteradorÍndiceX()) print((sellTs / 10) % 10)
		println()
		print("      ")
		for (sellTs <- ctorIteradorÍndiceX()) print(sellTs % 10)
		println()
		println()
		val iteradorÍndiceY = ctorIteradorÍndiceY();
		for (fila <- análisis) {
			print("%3d ~ " format iteradorÍndiceY.next)
			for (dineroFinal <- fila) {
				print(rango(dineroFinal, menor, mayor))
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
