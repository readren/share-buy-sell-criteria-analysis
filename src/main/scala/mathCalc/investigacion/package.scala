package mathCalc

package object investigacion {
	trait Investigador {
		/** La situación contextual del actor en un instante dado se puede dividir en dos partes: la parte descripta por las variables que dependen de las acciones del actor (variables alterables) y la parte descripta por las variables que no dependen de las acciones del actor (variables inalterables). Las inalterables las dividiremos en fuente y derivadas*/
		case class Contexto(estadoTestigo: EstadoTestigo, estimuloFuente: EstimuloFuente, estimuloDerivado: EstimuloDerivado)
		type EstimuloFuente
		type EstimuloDerivado
		type EstadoAyuda
		type EstadoTestigo
		type EstadoActor
		type Accion
		/**función que da el valor de las variables estimulantes fuente en función del tiempo. El valor de estas variables en todo instante es independientes de las acciones presentes, pasadas (y futuras) del actor. Esta función esta establecida antes de la realización del experimento. */
		type FuncEstimulo = Traversable[EstimuloFuente]
		/** función que da el valor de las variables estimulantes derivadas de ayuda (por experto). Igual que las fuente solo que su valor se obtienen a partir de y solo de el valor presente y pasado de las variables estimulantes fuente.
		  * Nota implementacion: El resultado de esta función conviene memorizarlo para evitar tener que evaluar esta función en cada repetición del experimento con mismo estimulo fuente.
		  */
		type FuncAyuda = (EstadoAyuda, EstimuloFuente) => (EstimuloDerivado, EstadoAyuda)
		/**función que da las acciones del actor a partir del estado anterior del actor y la situacion contextual actual (incluye variables estimulantes e inalterables). */
		type FuncActor = (EstadoActor, Contexto) => (Accion, EstadoActor)
		/**función que da el estado del contexto (solo interesan las variables alterables) a partir del estado anterior del contexto y las acciones que realizo el actor en respuesta al estado anterior del contexto. El estado final del contexto es lo que se busca dirigir. Por ejemplo, maximizar una de sus variables.*/
		type FuncConsecuencia = (Accion, Contexto) => EstadoTestigo

		trait ExperimentoPreparado {
			val funcEstimulo: FuncEstimulo
			val funcAyuda: FuncAyuda
			val estadoAyudaInicial: EstadoAyuda
			val funcConsecuencia: FuncConsecuencia
			val estadoTestigoInicial: EstadoTestigo

			def realizar(funcActor: FuncActor, estadoActorInicial: EstadoActor): EstadoTestigo = {
				var estadoAyudaPrevio = estadoAyudaInicial
				var estadoTestigo = estadoTestigoInicial
				var estadoActorPrevio = estadoActorInicial
				for (estimuloFuente <- funcEstimulo) {
					val (estimuloDerivado, estadoAyuda) = funcAyuda(estadoAyudaPrevio, estimuloFuente)
					val contexto = Contexto(estadoTestigo, estimuloFuente, estimuloDerivado)
					val (accion, estadoActor) = funcActor(estadoActorPrevio, contexto)
					val estadoTestigoSig = funcConsecuencia(accion, contexto)
					estadoAyudaPrevio = estadoAyuda
					estadoTestigo = estadoTestigoSig;
					estadoActorPrevio = estadoActor;
				}
				estadoTestigo
			}
		}

		/** Busca funcion actor mediante gradient boosting trees*/
		def buscarFuncActorQueMaximize(pauta: EstadoTestigo => Núm, experimento: ExperimentoPreparado):FuncActor

	}

}