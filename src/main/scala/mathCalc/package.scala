package object mathCalc {
	type Núm = Double
	type Vec = List[Núm]

	def sumaVectores(a: Vec, b: Vec): Vec =
		for ((ai, bi) <- a.zip(b)) yield ai + bi

	def multiplicaVector(factor: Núm, v: Vec): Vec =
		for (vi <- v) yield vi * factor
	
}