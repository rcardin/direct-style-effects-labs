import jdk.internal.vm.{Continuation, ContinuationScope}

// Shared scope for communication (same as before)
class EffectScope extends ContinuationScope("EffectScope"):
  var op: Any = null      // The yielded effect operation
  var value: Any = null   // The value to resume the continuation with

// Effect traits are now a clean API. No implementation details.
trait Log:
  def info(msg: String): Unit

trait State[S]:
  def get(): S
  def put(s: S): Unit


object Runner:
  def run[A](initialState: Int)(program: Log ?=> State[Int] ?=> A): A = {
    val scope = new EffectScope()

    // --- Handler Implementations ---
    // The handlers are now inner classes with access to the runner's state and scope.
    class LogHandler extends Log:
      def info(msg: String): Unit = {
        scope.op = LogHandler.Info(msg)
        Continuation.`yield`(scope)
      }
      def handle(op: Any): Unit = op match {
        case LogHandler.Info(msg) => println(s"INFO: $msg")
      }
    object LogHandler:
      case class Info(msg: String)

    class StateHandler extends State[Int]:
      private var currentState: Int = initialState
      def get(): Int = {
        scope.op = StateHandler.Get
        Continuation.`yield`(scope)
        scope.value.asInstanceOf[Int]
      }
      def put(s: Int): Unit = {
        scope.op = StateHandler.Put(s)
        Continuation.`yield`(scope)
      }
      def handle(op: Any): Any = op match {
        case StateHandler.Get    => currentState
        case StateHandler.Put(s) => currentState = s.asInstanceOf[Int]
      }
    object StateHandler:
      case object Get
      case class Put(state: Any)

    // --- Execution ---
    val logHandler = new LogHandler()
    val stateHandler = new StateHandler()
    var result: A = null.asInstanceOf[A]

    val continuation = new Continuation(scope, () => {
      // The user's program is called here, with the handlers provided implicitly.
      result = program(using logHandler)(using stateHandler)
    })

    // The trampoline loop
    while (!continuation.isDone) {
      continuation.run()
      if (!continuation.isDone) {
        scope.value = scope.op match {
          case op: LogHandler.Info => logHandler.handle(op)
          case op                => stateHandler.handle(op)
        }
      }
    }
    result
  }

object MainApp:
  // The function signature now perfectly tracks the effects it uses.
  def myProgram: Log ?=> State[Int] ?=> String = {
    val L = summon[Log]
    val S = summon[State[Int]]
    L.info("Program starting")
    val initial = S.get()
    L.info(s"Initial state is $initial")

    S.put(initial + 100)
    val finalState = S.get()
    L.info("Program finished")

    s"Final state is $finalState"
  }

  def main(args: Array[String]): Unit = {
    // The runner creates and provides all handlers.
    // The composition is handled automatically.
    val result = Runner.run(initialState = 42)(myProgram)
    println(s"\nFinal Result: $result")
  }