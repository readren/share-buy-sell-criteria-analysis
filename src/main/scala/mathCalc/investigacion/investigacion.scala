package mathCalc.investigacion

import mathCalc._

/** @author Gustavo
  */
class InvestigadorNovato extends Investigador {
	override type EstimuloFuente = Int
	override type EstadoActor = Vec
	override type EstimuloDerivado = Float
	override type EstadoAyuda = Float
	override type EstadoTestigo = Tenencia
	override type Accion = AccionSimple

	case class Tenencia(cant: Int, dinero: Int)

	trait AccionSimple {
		def operar(t0: Tenencia, precio: Int): Tenencia
	}
	case object Comprar extends AccionSimple {
		override def operar(t0: Tenencia, precio: Int) = {
			val cantComprable: Int = t0.dinero / precio
			Tenencia(t0.cant + cantComprable, t0.dinero - cantComprable * precio)
		}
	}
	case object Vender extends AccionSimple {
		override def operar(t0: Tenencia, precio: Int) = Tenencia(0, t0.dinero + t0.cant * precio)
	}

	type Peso = Double
	trait Hipo
	case class HipoPonderada(hipo: Hipo, peso: Peso)

	case class FuncActorNovato(hipos: List[HipoPonderada]) extends FuncActor {
		override def apply(estadoActor: EstadoActor, contexto: Contexto): (Accion, EstadoActor) = {
			???
		}
	}

	/** Busca funcion actor mediante gradient boosting trees */
	override def buscarFuncActorQueMaximize(pauta: (EstadoTestigo) => Núm, experimento: ExperimentoPreparado): FuncActor = {
		
		???
	}

}
	
