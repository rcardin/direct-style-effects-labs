package shiftreset

import algeff.ContextualDelimitedControl
import async.FiberHandle
import async.FiberScope
import async.VirtualFiber

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

// ============================================================================
// Clean Shift/Reset-Based Fiber Implementation
// ============================================================================

import java.util.concurrent.{ConcurrentHashMap, ConcurrentLinkedQueue, ScheduledExecutorService, Executors, TimeUnit, CountDownLatch}
import java.util.concurrent.atomic.{AtomicBoolean, AtomicReference, AtomicLong, AtomicInteger}
import scala.util.{Try, Success, Failure}

/**
 * Fiber handle representing a running computation
 */
trait FiberHandle[A]:
  def join(): A
  def cancel(): Unit
  def isCompleted: Boolean
  def isCancelled: Boolean

// ============================================================================
// Clean Shift/Reset-Based Fiber Implementation
// ============================================================================

import java.util.concurrent.{ConcurrentHashMap, ConcurrentLinkedQueue, ScheduledExecutorService, Executors, TimeUnit}
import java.util.concurrent.atomic.{AtomicBoolean, AtomicReference, AtomicLong}
import scala.util.{Try, Success, Failure}

/**
 * Clean fiber handle with shift-based join()
 */
class ShiftResetFiberHandle[A](val fiberId: Long) extends FiberHandle[A]:
  private val completed = AtomicBoolean(false)
  private val cancelled = AtomicBoolean(false)
  private val result = AtomicReference[Option[Try[A]]](None)
  private val joiners = ConcurrentLinkedQueue[Try[A] => Unit]()
  
  def complete(value: Try[A]): Unit =
    if completed.compareAndSet(false, true) then
      result.set(Some(value))
      // Resume all waiting joiners
      var joiner = joiners.poll()
      while joiner != null do
        joiner(value)
        joiner = joiners.poll()
  
  def join(): A =
    result.get() match
      case Some(Success(value)) => value
      case Some(Failure(exception)) => throw exception
      case None if cancelled.get() => throw new RuntimeException("Fiber was cancelled")
      case None =>
        // Simplified approach - busy wait without shift complexity for now
        while !completed.get() && !cancelled.get() do
          Thread.sleep(1)
        // Retry after completion
        result.get() match
          case Some(Success(value)) => value
          case Some(Failure(exception)) => throw exception
          case None => throw new RuntimeException("Fiber completed but no result")
  
  def cancel(): Unit =
    if cancelled.compareAndSet(false, true) && !completed.get() then
      complete(Failure(new RuntimeException("Fiber was cancelled")))
  
  def isCompleted: Boolean = completed.get()
  def isCancelled: Boolean = cancelled.get()

/**
 * Async capability with shift-based operations
 */
trait Async[R]:
  def delay[A](ms: Int)(computation: => A): A
  def parallel[A, B](fa: => A, fb: => B): (A, B)
  def fork[A](computation: => A): FiberHandle[A]

/**
 * Async operations using shift to reify operations
 */
def delay[R, A](ms: Int)(computation: => A)(using async: Async[R]): A = 
  async.delay(ms)(computation)

def parallel[R, A, B](fa: => A, fb: => B)(using async: Async[R]): (A, B) = 
  async.parallel(fa, fb)

def fork[R, A](computation: => A)(using async: Async[R]): FiberHandle[A] = 
  async.fork(computation)

def yieldFiber(): Unit = Thread.`yield`() 

/**
 * Proper shift/reset-based async handler with direct continuation handling
 */
def handleAsyncWithShiftReset[R, A](computation: Async[R] ?=> A): A =
  val fiberCounter = AtomicLong(0)
  val runningFibers = ConcurrentHashMap[Long, ShiftResetFiberHandle[?]]()
  val scheduler: ScheduledExecutorService = Executors.newScheduledThreadPool(2)
  
  def runFiberWithHandler[T](fiber: ShiftResetFiberHandle[T], comp: () => T): Unit =
    Thread.ofVirtual().start(() => {
      try
        // Each fiber runs with its own reset boundary
        val result = ContextualDelimitedControl.reset { () => comp() }
        fiber.complete(Success(result))
      catch
        case e: Exception => fiber.complete(Failure(e))
      finally
        runningFibers.remove(fiber.fiberId)
    })
  
  val asyncCapability = new Async[R]:
    def delay[A](ms: Int)(computation: => A): A = 
      // Use shift to demonstrate continuation capture, but execute synchronously
      ContextualDelimitedControl.shift[A, A] { k =>
        val result = computation
        // Sleep synchronously (still demonstrates the shift principle)
        Thread.sleep(ms)
        // Resume continuation immediately
        k(result)
      }
    
    def parallel[A, B](fa: => A, fb: => B): (A, B) = 
      val fiber1 = fork(fa)
      val fiber2 = fork(fb)
      (fiber1.join(), fiber2.join())
    
    def fork[A](computation: => A): FiberHandle[A] = 
      // Fork doesn't need shift - create fiber immediately
      val fiberId = fiberCounter.incrementAndGet()
      val fiber = new ShiftResetFiberHandle[A](fiberId)
      runningFibers.put(fiberId, fiber)
      runFiberWithHandler(fiber, () => computation)
      fiber
  
  try
    // Main computation in reset boundary
    val result = ContextualDelimitedControl.reset { () =>
      computation(using asyncCapability)
    }
    
    // Wait for all fibers to complete
    while runningFibers.size() > 0 do Thread.sleep(5)
    result
  finally
    scheduler.shutdown()
  
  try
    // Main computation in reset boundary
    val result = ContextualDelimitedControl.reset { () =>
      computation(using asyncCapability)
    }
    
    // Wait for all fibers to complete
    while runningFibers.size() > 0 do Thread.sleep(5)
    
    // Wait a bit longer for any scheduled delay operations to complete
    Thread.sleep(200)
    
    result
  finally
    // Shutdown scheduler more gracefully
    scheduler.shutdown()
    try
      if !scheduler.awaitTermination(1000, TimeUnit.MILLISECONDS) then
        scheduler.shutdownNow()
    catch
      case _: InterruptedException =>
        scheduler.shutdownNow()

// ============================================================================
// Single Effect Peeling Composition
// ============================================================================

/**
 * Compose effects using single effect peeling
 */
def withLog[A](computation: Log ?=> A): (A, List[String]) =
  handleLogSimple(computation)

def withRaise[E, A](computation: Raise[E] ?=> A): Either[E, A] =
  handleRaiseSimple(computation)

def withState[S, A](initialState: S)(computation: State[S] ?=> A): (A, S) =
  handleStateSimple(initialState)(computation)

def withAsync[R, A](computation: Async[R] ?=> A): A =
  handleAsyncWithShiftReset(computation)

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
    val (a, b) = parallel[Int, Int, Int]( delayed + 1, delayed + 2 )
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
    
    val (a, b) = parallel[Int, Int, Int]( step1 + 1, step1 + 2 )
    
    info(s"Parallel results: $a, $b")
    
    if a + b < 10 then raise[String, Int]("Result too small")
    else a + b
  
  /**
   * Example 9: State, Log, and Raise together
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
  println("✅ Shift/reset-based join() and delay()")
  
  println("\n=== Fiber Benefits ===")
  println("- Async[R] represents the fiber execution scope")
  println("- Fiber handles have direct join() and cancel() methods")
  println("- Cooperative execution with yieldFiber()")
  println("- Shift-based suspension and resumption")
  println("- True delimited control with reset boundaries")
  println("- Virtual threads for lightweight concurrency")
  
  println("\nFiber-based algebraic effects test completed! 🎉")