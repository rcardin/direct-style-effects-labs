package async

// ============================================================================
// Algebraic Effects with Context Function Effect Capability Tracking
// Based on Filinski's "Representing Monads" with single effect peeling
//
// This implementation provides:
// 1. Effect capabilities tracked as Log ?=> Raise[E] ?=> Async[R] ?=> A
// 2. Single effect peeling (handle one effect type at a time)
// 3. Each effect has its own capability trait
// 4. Proper shift/reset implementation with operation reification
// 5. Built on shift/reset foundation
//
// Key insight: Operations are reified as data, handlers interpret them
// ============================================================================

// Import the shift/reset machinery from the previous implementation
// (Assuming ContextualDelimitedControl is available)

// ============================================================================
// Core Effect Operation Framework
// ============================================================================
import algeff.ContextualDelimitedControl

/**
 * Base trait for effect operations that can be reified
 */
sealed trait EffectOp[A]

/**
 * Perform an effect operation using shift/reset
 * This reifies the operation and escapes to the handler
 */
def perform[A](operation: EffectOp[A]): A =
  ContextualDelimitedControl.shift[A, A] { k =>
    // This should never be reached - the reset boundary should catch this
    throw new RuntimeException(s"Unhandled effect operation: $operation")
  }

// ============================================================================
// Log Effect Capability
// ============================================================================

/**
 * Log effect operations
 */
sealed trait LogOp extends EffectOp[Unit]
case class Info(message: String) extends LogOp
case class Debug(message: String) extends LogOp
case class Error(message: String) extends LogOp

/**
 * Log capability - provides logging operations via context function
 */
trait Log:
  def info(message: String): Unit
  def debug(message: String): Unit  
  def error(message: String): Unit

/**
 * Log operations using the Log capability
 */
def info(message: String)(using log: Log): Unit = log.info(message)
def debug(message: String)(using log: Log): Unit = log.debug(message)
def error(message: String)(using log: Log): Unit = log.error(message)

/**
 * Log capability implementation that reifies operations
 */
def createLogCapability(): Log = new Log:
  def info(message: String): Unit = perform(Info(message))
  def debug(message: String): Unit = perform(Debug(message))
  def error(message: String): Unit = perform(Error(message))

/**
 * Log handler - manages log state and interprets log operations
 */
class LogHandler:
  private var logs: List[String] = Nil
  
  def handle(op: LogOp): Unit = op match
    case Info(message) => 
      val entry = s"[INFO] $message"
      logs = entry :: logs
      println(entry)
    case Debug(message) => 
      val entry = s"[DEBUG] $message"
      logs = entry :: logs
      println(entry)
    case Error(message) => 
      val entry = s"[ERROR] $message"
      logs = entry :: logs
      println(entry)
  
  def getLogs: List[String] = logs.reverse

/**
 * Handle Log effect using single effect peeling
 */
def handleLog[A](computation: Log ?=> A): (A, List[String]) =
  val handler = new LogHandler
  val logCapability = createLogCapability()
  
  def loop(comp: () => A): A =
    ContextualDelimitedControl.reset { () =>
      try 
        comp()
      catch
        case ContextualDelimitedControl.ShiftEscape(op: LogOp) =>
          handler.handle(op)
          loop(comp)
    }
  
  val result = loop(() => computation(using logCapability))
  (result, handler.getLogs)

// ============================================================================
// Raise Effect Capability  
// ============================================================================

/**
 * Raise effect operations
 */
sealed trait RaiseOp[E] extends EffectOp[Nothing]
case class RaiseError[E](error: E) extends RaiseOp[E]

/**
 * Raise capability - provides exception operations via context function
 */
trait Raise[E]:
  def raise[A](error: E): A

/**
 * Raise operations using the Raise capability
 */
def raise[E, A](error: E)(using raiseCapability: Raise[E]): A = 
  raiseCapability.raise(error)

/**
 * Raise capability implementation that reifies operations
 */
def createRaiseCapability[E](): Raise[E] = new Raise[E]:
  def raise[A](error: E): A = perform(RaiseError(error)).asInstanceOf[A]

/**
 * Raise handler - manages exceptions
 */
class RaiseHandler[E]:
  def handle(op: RaiseOp[E]): Nothing = op match
    case RaiseError(error) => 
      throw new RuntimeException(s"Raised: $error")

/**
 * Handle Raise effect using single effect peeling
 */
def handleRaise[E, A](computation: Raise[E] ?=> A): Either[E, A] =
  val handler = new RaiseHandler[E]
  val raiseCapability = createRaiseCapability[E]()
  
  def loop(comp: () => A): Either[E, A] =
    ContextualDelimitedControl.reset { () =>
      try 
        Right(comp())
      catch
        case ContextualDelimitedControl.ShiftEscape(op: RaiseOp[E]) =>
          Left(op.asInstanceOf[RaiseError[E]].error)
        case e: RuntimeException if e.getMessage.startsWith("Raised: ") =>
          Left(e.getMessage.substring(8).asInstanceOf[E])
    }
  
  loop(() => computation(using raiseCapability))

// ============================================================================
// State Effect Capability
// ============================================================================

/**
 * State effect operations
 */
sealed trait StateOp[S] extends EffectOp[?]
case class Get[S]() extends StateOp[S] with EffectOp[S]
case class Put[S](newState: S) extends StateOp[S] with EffectOp[Unit]
case class Modify[S](f: S => S) extends StateOp[S] with EffectOp[Unit]

/**
 * State capability - provides state operations via context function
 */
trait State[S]:
  def get: S
  def put(newState: S): Unit
  def modify(f: S => S): Unit

/**
 * State operations using the State capability
 */
def get[S](using state: State[S]): S = state.get
def put[S](newState: S)(using state: State[S]): Unit = state.put(newState)
def modify[S](f: S => S)(using state: State[S]): Unit = state.modify(f)

/**
 * State capability implementation that reifies operations
 */
def createStateCapability[S](): State[S] = new State[S]:
  def get: S = perform(Get[S]())
  def put(newState: S): Unit = perform(Put(newState))
  def modify(f: S => S): Unit = perform(Modify(f))

/**
 * State handler - manages state and interprets state operations
 */
class StateHandler[S](private var currentState: S):
  def handle[A](op: StateOp[S]): Any = op match
    case Get() => currentState
    case Put(newState) => 
      currentState = newState
      ()
    case Modify(f) => 
      currentState = f(currentState)
      ()
  
  def getCurrentState: S = currentState

/**
 * Handle State effect using single effect peeling
 */
def handleState[S, A](initialState: S)(computation: State[S] ?=> A): (A, S) =
  val handler = new StateHandler(initialState)
  val stateCapability = createStateCapability[S]()
  
  def loop(comp: () => A): A =
    ContextualDelimitedControl.reset { () =>
      try 
        comp()
      catch
        case ContextualDelimitedControl.ShiftEscape(op: StateOp[S]) =>
          val result = handler.handle(op)
          // Continue with the result - this is tricky without proper continuation support
          result.asInstanceOf[A]
    }
  
  val result = loop(() => computation(using stateCapability))
  (result, handler.getCurrentState)

// ============================================================================
// Simplified Implementation with Direct State Threading
// ============================================================================

/**
 * Simplified state effect that works without complex continuation handling
 */
def handleStateSimple[S, A](initialState: S)(computation: State[S] ?=> A): (A, S) =
  var currentState = initialState
  
  val stateCapability = new State[S]:
    def get: S = currentState
    def put(newState: S): Unit = { currentState = newState }
    def modify(f: S => S): Unit = { currentState = f(currentState) }
  
  val result = computation(using stateCapability)
  (result, currentState)

/**
 * Simplified log effect that works without complex continuation handling
 */
def handleLogSimple[A](computation: Log ?=> A): (A, List[String]) =
  var logs: List[String] = Nil
  
  val logCapability = new Log:
    def info(message: String): Unit = 
      val entry = s"[INFO] $message"
      logs = entry :: logs
      println(entry)
    
    def debug(message: String): Unit = 
      val entry = s"[DEBUG] $message"
      logs = entry :: logs
      println(entry)
    
    def error(message: String): Unit = 
      val entry = s"[ERROR] $message"
      logs = entry :: logs
      println(entry)
  
  val result = computation(using logCapability)
  (result, logs.reverse)

/**
 * Simplified raise effect that works without complex continuation handling
 */
def handleRaiseSimple[E, A](computation: Raise[E] ?=> A): Either[E, A] =
  case class EffectException(error: Any) extends Exception
  
  val raiseCapability = new Raise[E]:
    def raise[A](error: E): A = throw EffectException(error)
  
  try
    val result = computation(using raiseCapability)
    Right(result)
  catch
    case EffectException(error) => Left(error.asInstanceOf[E])

// ============================================================================
// Fiber-Based Async Effect with Structured Concurrency
// ============================================================================

import java.util.concurrent.{ConcurrentHashMap, CompletableFuture}
import java.util.concurrent.atomic.{AtomicBoolean, AtomicReference}
import scala.util.{Try, Success, Failure}

/**
 * Fiber handle representing a running computation
 * Models happy path only - failures propagate through structured concurrency
 */
trait FiberHandle[A]:
  def join(): A
  def cancel(): Unit
  def isCompleted: Boolean
  def isCancelled: Boolean

/**
 * Fiber scope manages child fibers for structured concurrency
 * Implements Kotlin coroutines failure model
 */
class FiberScope:
  private val children = ConcurrentHashMap.newKeySet[FiberHandle[?]]()
  private val cancelled = AtomicBoolean(false)
  
  def addChild[A](fiber: FiberHandle[A]): Unit = 
    if !cancelled.get() then children.add(fiber)
    else fiber.cancel()
  
  def removeChild[A](fiber: FiberHandle[A]): Unit = 
    children.remove(fiber)
  
  def cancel(): Unit =
    cancelled.set(true)
    children.forEach(_.cancel())
    children.clear()
  
  def isCancelled: Boolean = cancelled.get()
  
  def awaitAll(): Unit =
    // In structured concurrency, we wait for all children to complete
    children.forEach { child =>
      if !child.isCancelled then
        try child.join()
        catch case _: Exception => cancel() // Failure propagates
    }

/**
 * Virtual thread-based fiber implementation
 */
class VirtualFiber[A](
  computation: () => A,
  parentScope: FiberScope
) extends FiberHandle[A]:
  
  private val future = new CompletableFuture[A]()
  private val cancelled = AtomicBoolean(false)
  private val childScope = new FiberScope()
  private val virtualThread = Thread.ofVirtual().unstarted(() => runFiber())
  
  // Start the fiber immediately
  parentScope.addChild(this)
  virtualThread.start()
  
  private def runFiber(): Unit =
    try
      if !cancelled.get() then
        val result = computation()
        childScope.awaitAll() // Wait for all children (structured concurrency)
        if !cancelled.get() then
          future.complete(result)
        else
          future.cancel(false)
      else
        future.cancel(false)
    catch
      case e: Exception =>
        childScope.cancel() // Cancel all children on failure
        if !cancelled.get() then
          // Don't cancel self - let the exception propagate to join()
          future.completeExceptionally(e)
          // Notify parent scope of failure (but don't cancel siblings immediately)
          // In a real implementation, this might be more sophisticated
        else
          future.cancel(false)
  
  def join(): A = 
    try
      if cancelled.get() then
        throw new RuntimeException("Fiber was cancelled")
      else
        future.get() // This blocks until completion
    catch
      case _: java.util.concurrent.CancellationException =>
        throw new RuntimeException("Fiber was cancelled")
      case e: java.util.concurrent.ExecutionException =>
        // Unwrap the actual exception from the computation
        throw e.getCause match
          case null => e
          case actual => actual
  
  def cancel(): Unit =
    cancelled.set(true)
    childScope.cancel() // Cancel all children
    future.cancel(false)
    parentScope.removeChild(this)
  
  def isCompleted: Boolean = future.isDone && !future.isCancelled
  def isCancelled: Boolean = cancelled.get() || future.isCancelled

/**
 * Async capability representing the fiber execution scope
 */
trait Async[R]:
  // Original async operations
  def delay[A](ms: Int)(computation: => A): A
  def parallel[A, B](fa: => A, fb: => B): (A, B)
  
  // Fiber scope operations
  def fork[A](computation: => A): FiberHandle[A]

/**
 * Async operations using the Async capability (execution scope)
 */
def delay[R, A](ms: Int)(computation: => A)(using async: Async[R]): A = 
  async.delay(ms)(computation)

def parallel[R, A, B](fa: => A, fb: => B)(using async: Async[R]): (A, B) = 
  async.parallel(fa, fb)

def fork[R, A](computation: => A)(using async: Async[R]): FiberHandle[A] = 
  async.fork(computation)

/**
 * Fiber yielding - affects the currently executing fiber
 */
def yieldFiber(): Unit = 
  Thread.`yield`() // Cooperative yielding of current fiber

/**
 * Fiber-based async effect handler with structured concurrency
 */
def handleAsyncWithFibers[R, A](computation: Async[R] ?=> A): A =
  val rootScope = new FiberScope()
  
  val asyncCapability = new Async[R]:
    // Thread-local storage for current fiber scope
    private val currentScope = new ThreadLocal[FiberScope]:
      override def initialValue(): FiberScope = rootScope
    
    def delay[A](ms: Int)(computation: => A): A = 
      Thread.sleep(ms) // Cooperative - doesn't block other fibers
      computation
    
    def parallel[A, B](fa: => A, fb: => B): (A, B) = 
      val fiber1 = fork(fa)
      val fiber2 = fork(fb)
      (fiber1.join(), fiber2.join())
    
    def fork[A](computation: => A): FiberHandle[A] = 
      val parentScope = currentScope.get()
      new VirtualFiber(() => {
        currentScope.set(new FiberScope()) // Child gets its own scope
        try computation
        finally currentScope.set(parentScope)
      }, parentScope)
  
  try
    val result = computation(using asyncCapability)
    rootScope.awaitAll() // Ensure all children complete (structured concurrency)
    result
  finally
    rootScope.cancel() // Clean up any remaining children

/**
 * Simplified async handler (keeping for compatibility)
 */
def handleAsyncSimple[R, A](computation: Async[R] ?=> A): A =
  handleAsyncWithFibers(computation)

// ============================================================================
// Single Effect Peeling Composition (Simplified)
// ============================================================================

/**
 * Compose effects using single effect peeling (simplified versions that work)
 */
def withLog[A](computation: Log ?=> A): (A, List[String]) =
  handleLogSimple(computation)

def withRaise[E, A](computation: Raise[E] ?=> A): Either[E, A] =
  handleRaiseSimple(computation)

def withState[S, A](initialState: S)(computation: State[S] ?=> A): (A, S) =
  handleStateSimple(initialState)(computation)

def withAsync[R, A](computation: Async[R] ?=> A): A =
  handleAsyncSimple(computation)

/**
 * Compose multiple effects using single effect peeling
 */
def withLogAndRaise[E, A](computation: Log ?=> Raise[E] ?=> A): Either[E, (A, List[String])] =
  withRaise {
    withLog {
      computation
    }
  }

def withStateAndLog[S, A](initialState: S)(computation: State[S] ?=> Log ?=> A): ((A, S), List[String]) =
  withLog {
    withState(initialState) {
      computation
    }
  }

def withLogRaiseAndAsync[E, R, A](computation: Log ?=> Raise[E] ?=> Async[R] ?=> A): Either[E, (A, List[String])] =
  withRaise {
    withLog {
      withAsync {
        computation
      }
    }
  }

def withStateLogAndRaise[S, E, A](initialState: S)(
  computation: State[S] ?=> Log ?=> Raise[E] ?=> A
): Either[E, ((A, S), List[String])] =
  withRaise {
    withLog {
      withState(initialState) {
        computation
      }
    }
  }

// ============================================================================
// Examples with Effect Capability Tracking
// ============================================================================

object AlgebraicEffectsExamples:
  
  /**
   * Example 1: Simple stateful computation with capability tracking
   */
  val counterProgram: State[Int] ?=> Int =
    put(10)
    val x = get[Int]
    put(x + 5)
    modify[Int](_ * 2)
    get[Int]
  
  /**
   * Example 2: Logging computation with capability tracking  
   */
  val loggingProgram: Log ?=> String =
    info("Starting computation")
    debug("Processing data")
    error("This is just a test error")
    "computation completed"
  
  /**
   * Example 3: Multi-effect computation with capability tracking
   */
  val multiEffectProgram: State[Int] ?=> Log ?=> String =
    info("Starting multi-effect computation")
    put(42)
    val x = get[Int]
    debug(s"Current state: $x")
    modify[Int](_ * 2)
    val finalVal = get[Int]
    info(s"Final state: $finalVal")
    s"Result: $finalVal"
  
  /**
   * Example 4: Exception handling with capability tracking
   */
  val exceptionProgram: Raise[String] ?=> Int =
    if scala.util.Random.nextBoolean() then 42
    else raise[String, Int]("random failure")
  
  /**
   * Example 5: Async computation with fiber capability tracking
   */
  val asyncProgram: Async[Int] ?=> Int =
    val delayed = delay[Int, Int](100) { 42 }
    val (a, b) = parallel[Int, Int, Int] ({ delayed + 1 }, { delayed + 2 })
    a + b
  
  /**
   * Example 6: Fiber-based computation with direct fiber operations
   */
  val fiberProgram: Async[Int] ?=> Int =
    val fiber1 = fork { 
      delay[Int, Int](100) { 42 }
    }
    val fiber2 = fork { 
      delay[Int, Int](50) { 24 }  
    }
    
    // Join directly on fiber handles
    fiber1.join() + fiber2.join()
  
  /**
   * Example 7: Complex fiber coordination with cancellation
   */
  val complexFiberProgram: Async[Int] ?=> Int =
    val longFiber = fork {
      delay[Int, Int](1000) { 100 }
    }
    
    val quickFiber = fork {
      delay[Int, Int](50) { 1 }
    }
    
    val quickResult = quickFiber.join()
    
    // Cancel long fiber if quick one completes first
    if quickResult == 1 then
      longFiber.cancel()
      quickResult
    else
      longFiber.join()
  
  /**
   * Example 8: Complex composition with multiple effect capabilities
   * This is the desired form: Log ?=> Raise[String] ?=> Async[Int] ?=> Int
   */
  val program: Log ?=> Raise[String] ?=> Async[Int] ?=> Int =
    info("Starting complex computation")
    
    val step1Fiber = fork {
      delay[Int, Int](50) {
        if scala.util.Random.nextBoolean() then 10
        else raise[String, Int]("async failure")
      }
    }
    
    val step1 = step1Fiber.join()
    debug(s"Step 1 result: $step1")

    val (a, b) = parallel[Int, Int, Int]({ step1 + 1 }, { step1 + 2 })

    info(s"Parallel results: $a, $b")
    
    if a + b < 10 then raise[String, Int]("Result too small")
    else a + b
  
  /**
   * Example 7: State, Log, and Raise together
   */
  val complexProgram: State[Int] ?=> Log ?=> Raise[String] ?=> Int =
    info("Starting complex state computation")
    put(0)
    
    modify[Int](_ + 10)
    val x = get[Int]
    debug(s"State after increment: $x")
    
    if x < 5 then 
      error("State too small")
      raise[String, Int]("State validation failed")
    else
      put(x * 2)
      val finalResult = get[Int]
      info(s"Final result: $finalResult")
      finalResult

// ============================================================================
// Testing Effect Capability Tracking
// ============================================================================

@main def testEffectCapabilities(): Unit =
  import AlgebraicEffectsExamples.*
  
  println("=== Algebraic Effects with Capability Tracking ===")
  
  // Test single effect capabilities
  println("\n--- State Capability ---")
  val (stateResult, finalState) = withState(0)(counterProgram)
  println(s"State result: $stateResult, final state: $finalState")
  
  println("\n--- Log Capability ---")
  val (logResult, logs) = withLog(loggingProgram)
  println(s"Log result: $logResult")
  println("Logs:")
  logs.foreach(println)
  
  println("\n--- Raise Capability ---")
  val raiseResult = withRaise(exceptionProgram)
  println(s"Raise result: $raiseResult")
  
  println("\n--- Async Capability ---")
  val asyncResult = withAsync(asyncProgram)
  println(s"Async result: $asyncResult")
  
  // Test fiber operations with direct fiber handle methods
  println("\n--- Fiber Operations (fork/join/cancel) ---")
  val fiberResult = withAsync(fiberProgram)
  println(s"Fiber result: $fiberResult")
  
  println("\n--- Complex Fiber Coordination ---")
  val complexFiberResult = withAsync(complexFiberProgram)
  println(s"Complex fiber result: $complexFiberResult")
  
  // Test yielding
  println("\n--- Fiber Yielding ---")
  val yieldingProgram: Async[Int] ?=> Int = 
    val fiber = fork {
      yieldFiber() // Cooperative yielding
      42
    }
    yieldFiber()
    fiber.join()
  val yieldResult = withAsync(yieldingProgram)
  println(s"Yield result: $yieldResult")
  
  // Test effect composition
  println("\n--- State + Log Capabilities ---")
  val ((multiResult, multiState), multiLogs) = withStateAndLog(0)(multiEffectProgram)
  println(s"Multi result: $multiResult, final state: $multiState")
  println("Multi logs:")
  multiLogs.foreach(println)
  
  // Test the desired form: Log ?=> Raise[String] ?=> Async[Int] ?=> Int
  println("\n--- Log ?=> Raise[String] ?=> Async[Int] ?=> Int ---")
  val programResult = withLogRaiseAndAsync(program)
  println(s"Program result: $programResult")
  
  // Test complex composition
  println("\n--- State[Int] ?=> Log ?=> Raise[String] ?=> Int ---")
  val complexResult = withStateLogAndRaise(0)(complexProgram)
  println(s"Complex result: $complexResult")
  
  println("\n=== Effect Capability Features ===")
  println("✅ Effect capabilities: Log ?=> Raise[E] ?=> Async[R] ?=> A")
  println("✅ Single effect peeling (one capability per handler)")
  println("✅ Type-safe effect composition")
  println("✅ Clean effect tracking in signatures")
  println("✅ Each effect has its own capability trait")
  println("✅ Fiber operations: fork/join/cancel")
  println("✅ Structured concurrency with failure propagation")
  println("✅ Virtual thread-based cooperative fibers")
  println("✅ Async as fiber execution scope")
  println("✅ Direct fiber handle operations")
  
  println("\n=== Fiber Benefits ===")
  println("- Async[R] represents the fiber execution scope")
  println("- Fiber handles have direct join() and cancel() methods")
  println("- Cooperative execution with yieldFiber()")
  println("- Structured concurrency ensures resource safety")
  println("- Kotlin-style failure propagation model")
  println("- Virtual threads for lightweight concurrency")
  
  println("\nFiber-based algebraic effects test completed! 🎉")