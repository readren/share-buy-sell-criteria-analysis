import mathCalc.funcHeteroAproxBin._
object worksheet {

val v1 = ValsEnVertices(1,2,3,4)                  //> v1  : mathCalc.funcHeteroAproxBin.ValsEnVertices[Int] = Rama(2,Rama(1,Hoja(1)
                                                  //| ,Hoja(2)),Rama(1,Hoja(3),Hoja(4)))
val v2 = ValsEnVertices(List(4d),List(12d))       //> v2  : mathCalc.funcHeteroAproxBin.ValsEnVertices[List[Double]] = Rama(1,Hoja
                                                  //| (List(4.0)),Hoja(List(12.0)))

val f = Func.fill(CuboMd(List(Intervalo(0,8),Intervalo(0,8))))(x=>List(x.reduce(_ + _)))
                                                  //> f  : mathCalc.funcHeteroAproxBin.FuncLineal = FuncLineal(CuboMd(List(Interva
                                                  //| lo(0.0,8.0), Intervalo(0.0,8.0))),Rama(2,Rama(1,Hoja(List(0.0)),Hoja(List(8.
                                                  //| 0))),Rama(1,Hoja(List(8.0)),Hoja(List(16.0)))))
FuncLineal(
	CuboMd(List(Intervalo(0.0,8.0), Intervalo(0.0,8.0))),
	Rama(2,
		Rama(1,Hoja(List(0.0)),Hoja(List(8.0))),
		Rama(1,Hoja(List(8.0)),Hoja(List(16.0)))
	)
)                                                 //> res0: mathCalc.funcHeteroAproxBin.FuncLineal = FuncLineal(CuboMd(List(Interv
                                                  //| alo(0.0,8.0), Intervalo(0.0,8.0))),Rama(2,Rama(1,Hoja(List(0.0)),Hoja(List(8
                                                  //| .0))),Rama(1,Hoja(List(8.0)),Hoja(List(16.0)))))


val g = f.partir(0,4,ValsEnVertices(List(4d),List(0d)))
                                                  //> g  : mathCalc.funcHeteroAproxBin.FuncEmpalme = FuncEmpalme(0,4.0,FuncLineal(
                                                  //| CuboMd(List(Intervalo(0.0,4.0), Intervalo(0.0,8.0))),Rama(2,Rama(1,Hoja(List
                                                  //| (0.0)),Hoja(List(8.0))),Rama(1,Hoja(List(4.0)),Hoja(List(0.0))))),FuncLineal
                                                  //| (CuboMd(List(Intervalo(4.0,8.0), Intervalo(0.0,8.0))),Rama(2,Rama(1,Hoja(Lis
                                                  //| t(4.0)),Hoja(List(0.0))),Rama(1,Hoja(List(8.0)),Hoja(List(16.0))))))
                                                  
FuncEmpalme(0,4.0,
	FuncLineal(
		CuboMd(List(Intervalo(0.0,4.0), Intervalo(0.0,8.0))),
		Rama(2,
			Rama(1,Hoja(List(0.0)),Hoja(List(8.0))),
			Rama(1,Hoja(List(4.0)),Hoja(List(0.0)))
		)
	),
	FuncLineal(
		CuboMd(List(Intervalo(4.0,8.0), Intervalo(0.0,8.0))),
		Rama(2,
			Rama(1,Hoja(List(4.0)),Hoja(List(0.0))),
			Rama(1,Hoja(List(8.0)),Hoja(List(16.0)))
		)
	)
)                                                 //> res1: mathCalc.funcHeteroAproxBin.FuncEmpalme = FuncEmpalme(0,4.0,FuncLineal
                                                  //| (CuboMd(List(Intervalo(0.0,4.0), Intervalo(0.0,8.0))),Rama(2,Rama(1,Hoja(Lis
                                                  //| t(0.0)),Hoja(List(8.0))),Rama(1,Hoja(List(4.0)),Hoja(List(0.0))))),FuncLinea
                                                  //| l(CuboMd(List(Intervalo(4.0,8.0), Intervalo(0.0,8.0))),Rama(2,Rama(1,Hoja(Li
                                                  //| st(4.0)),Hoja(List(0.0))),Rama(1,Hoja(List(8.0)),Hoja(List(16.0))))))



f(List(0,0))                                      //> res2: mathCalc.Vec = List(0.0)
g(List(0,0))                                      //> res3: mathCalc.Vec = List(0.0)
f(List(2,2))                                      //> res4: mathCalc.Vec = List(4.0)
g(List(2,2))                                      //> res5: mathCalc.Vec = List(2.5)
f(List(3,1))                                      //> res6: mathCalc.Vec = List(4.0)
g(List(3,1))                                      //> res7: mathCalc.Vec = List(2.875)
f(List(4,7))                                      //> res8: mathCalc.Vec = List(11.0)
g(List(4,7))                                      //> res9: mathCalc.Vec = List(0.5)
f(List(5,2))                                      //> res10: mathCalc.Vec = List(7.0)
g(List(5,2))                                      //> res11: mathCalc.Vec = List(4.75)
f(List(6,0))                                      //> res12: mathCalc.Vec = List(6.0)
g(List(6,0))                                      //> res13: mathCalc.Vec = List(6.0)



val l = List(1,2,3,4,5,6,7,8,9)                   //> l  : List[Int] = List(1, 2, 3, 4, 5, 6, 7, 8, 9)
val i = l.iterator.sliding(2,2)                   //> i  : Iterator[Int]#GroupedIterator[Int] = non-empty iterator
i.foreach(println)                                //> List(1, 2)
                                                  //| List(3, 4)
                                                  //| List(5, 6)
                                                  //| List(7, 8)
                                                  //| List(9)
}