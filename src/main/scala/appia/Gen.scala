package appia

trait Gen { self =>

  type Context

  def from: Set[Node { type Context = self.Context }]

  def to: Set[Node { type Context = self.Context }]

  def run(c: Context)

  def checkedRun(c: Context) {
    assert(from.forall(_.done(c)))
    if (to.exists(!_.done(c))) {
      run(c)
      assert(to.forall(_.done(c)))
    }
  }

}

object Gen extends com.typesafe.scalalogging.slf4j.StrictLogging {

  private type N[C] = Node { type Context = C }
  private type G[C] = Gen { type Context = C }

  def compose[C](gs: Set[G[C]]): G[C] = new Composition[C](gs)

  def composePar[C](gs: Set[G[C]]): G[C] = new ThreadedComposition[C](gs)

  private class Composition[C](gs: Set[G[C]]) extends Gen {
    val linearized = toposort(dag(gs))
    override type Context = C
    override val to = |*(gs.map(_.to))
    override val from = |*(gs.map(_.from)) &~ to
    override def run(c: C) = {
      for (g <- linearized) {
        logger.debug(s"started ${g.getClass.getSimpleName}")
        g.checkedRun(c)
        logger.debug(s"finished ${g.getClass.getSimpleName}")
      }
    }
  }

  private class ThreadedComposition[C](gs: Set[G[C]]) extends Composition[C](gs) { self =>
    val ended = new Node {
      override type Context = C
      override def done(c: C) = true
    }
    val end = new Gen {
      override type Context = C
      override val from = self.to
      override val to = Set[N[C]](ended)
      override def run(c: C) = {}
    }
    override def run(c: C) = {
      import java.util.concurrent._
      val nodes = from | to
      val latches = (nodes + ended).map { key => (key, new CountDownLatch(1)) }.toMap
      val tasks = for (g <- (linearized :+ end)) yield new Runnable {
        override def run() {
          for (dependency <- g.from) latches(dependency).await()
          logger.debug(s"started ${g.getClass.getSimpleName}")
          g.checkedRun(c)
          for (produced <- g.to) latches(produced).countDown()
          logger.debug(s"finished ${g.getClass.getSimpleName}")
        }
      }
      // even though we have this as a precondition
      // we still want to check this, otherwise deadlock
      assert(from.forall(_.done(c)))
      // we can be sure that done nodes are a superset of from
      for (node <- nodes if node.done(c)) latches(node).countDown()
      val pool = Executors.newCachedThreadPool
      for (task <- tasks) pool.execute(task)
      pool.shutdown()
      latches(ended).await()
    }
  }

  private type DAG[C] = Map[G[C], Set[G[C]]]

  private def dag[C](gs: Set[G[C]]): DAG[C] = {
    val generatedBy = scala.collection.mutable.Map.empty[N[C], G[C]]
    for (g <- gs; node <- g.to) {
      // TODO: is this too restrictive?
      assert(!generatedBy.contains(node))
      generatedBy(node) = g
    }
    val dag = for (g <- gs) yield {
      val dependencies = g.from.map { dependency =>
        assert(generatedBy.contains(dependency))
        generatedBy(dependency)
      }
      (g, dependencies)
    }
    dag.toMap
  }

  private def toposort[C](dependencies: DAG[C]): Seq[G[C]] = {
    import scala.collection.mutable._
    val sorted = LinkedHashSet.empty[G[C]]
    def add(g: G[C]) {
      if (sorted.contains(g)) return
      for (d <- dependencies(g)) add(d)
      sorted.add(g)
    }
    for (g <- dependencies.keys) {
      try {
        add(g)
      } catch {
        // ghetto
        case e: StackOverflowError =>
          sys.error(s"gen ${g.getClass.getSimpleName} has cyclic dependency")
      }
    }
    sorted.toSeq
  }

  private def |*[T](ss: Set[Set[T]]) = ss.fold(Set.empty[T])(_ | _)

  // temporarily unused, will potentially use for evented composition

  //  private def roots(dependencies: DAG): Set[Gen] = {
  //    dependencies.filter(_._2.isEmpty).keySet
  //  }
  //
  //  private def invert[A, B](m: Map[A, Set[B]]): Map[B, Set[A]] = {
  //    import scala.collection.mutable._
  //    val inverted = Map.empty[B, Set[A]]
  //    for ((a, bs) <- m) {
  //      for (b <- bs) {
  //        inverted.getOrElseUpdate(b, Set.empty[A]).add(a)
  //      }
  //    }
  //    inverted.map { case (b, as) => (b, as.toSet) }.toMap
  //  }

}