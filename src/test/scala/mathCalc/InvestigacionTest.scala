package mathCalc

import org.scalatest.WordSpec
import mathCalc.investigacion._
import org.scalactic.Tolerance
import mathCalc.investigacion.InvestigadorNovato

/** @author Gustavo
  */
class InvestigacionTest extends WordSpec with Tolerance {
	"el novato" when {
		val novato = new InvestigadorNovato {
			val experimento = new ExperimentoPreparado {
				override val funcEstimulo: FuncEstimulo = Seq(5, 7, 3, 7, 1, 3, 9, 8, 7, 5)
				override val estadoAyudaInicial: EstadoAyuda = funcEstimulo.head
				override val funcConsecuencia: FuncConsecuencia = (accion, contexto) => accion.operar(contexto.estadoTestigo, contexto.estimuloFuente)
				override val funcAyuda: FuncAyuda = (e, s) => { val x = (e * 2 + s) / 3; (x, x) }
				override val estadoTestigoInicial: EstadoTestigo = Tenencia(0, 100)
			}

		}
		"intenta hallar una funcion actor" should {
			val funcActor = novato.buscarFuncActorQueMaximize(_.dinero, novato.experimento)
			assert(funcActor != null)
		}
	}
}