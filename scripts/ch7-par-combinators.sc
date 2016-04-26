import java.util.concurrent.{ExecutorService, Future, Executors}
import learning.ch7.Blocking.Par

val es = Executors.newFixedThreadPool(2)

val pInt = Par.unit(3)
Par.run(es)(Par.map(pInt)(println))
Thread.sleep(1000)

// From a list of Par choices, choose one Par!
def choiceN [A] (pN: Par[Int])(pChoices: List[Par[A]]): Par[A] = {
  Par.flatMap(pN)(pChoices(_))
}

val pInts = List(10,11,12,13,14).map(Par.unit(_))
Par.run(es)(Par.map(Par.sequence(pInts))(println))
Thread.sleep(1000)

val pChosen = choiceN(pInt)(pInts)
Par.run(es)(Par.map(pChosen)(println))
Thread.sleep(1000)

// Switch between two Pars
def choice [A] (pCond: Par[Boolean])(pYes: Par[A], pNo: Par[A]): Par[A] = {
  val pIndex: Par[Int] = Par.map(pCond)(_ match {
    case true => 0
    case false => 1
  })
  choiceN(pIndex)(List(pYes, pNo))
}

val pYes = choice(Par.unit(true))(Par.unit("yes"), Par.unit("no"))
Par.run(es)(Par.map(pYes)(println))
Thread.sleep(1000)

// Map to Pars
def choiceMap [K,V] (pKey: Par[K]) (pChoices: Map[K, Par[V]]): Par[V] = {
  Par.flatMap(pKey)(pChoices(_))
}

// Just choose already
def choice [K,V] (pKey: Par[K]) (choose: K => Par[V]): Par[V] = {
  Par.flatMap(pKey)(choose(_))
}