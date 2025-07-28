// Replay-Based shift/reset Implementation using Java Continuation API
// This approach replays the computation from the reset boundary for each continuation call

import jdk.internal.vm.{Continuation, ContinuationScope}
import scala.collection.mutable
import java.util.concurrent.atomic.AtomicInteger

/**
 * A replay-based implementation of delimited continuations (shift/reset)
 * using Java 21's internal Continuation API.
 * 
 * Strategy: Instead of trying to clone Java continuations (which is impossible),
 * we record computations at reset boundaries and replay them with different
 * values when continuations are invoked.
 */
object ReplayShiftReset:
  
  // Unique ID generator for shift points
  private val shiftIdGenerator = new AtomicInteger(0)
  
  // Thread-local storage for continuation state
  private val currentResetFrame = new ThreadLocal[Option[ResetFrame]]:
    override def initialValue(): Option[ResetFrame] = None
  
  // Stack of nested reset frames
  private val resetStack = new ThreadLocal[mutable.Stack[ResetFrame]]:
    override def initialValue(): mutable.Stack[ResetFrame] = mutable.Stack()
  
  // Track shift IDs per reset frame to ensure consistency across replays
  private val shiftIdCounter = new ThreadLocal[AtomicInteger]:
    override def initialValue(): AtomicInteger = new AtomicInteger(0)
  
  /**
   * Represents a reset boundary with recorded computation and shift values
   */
  case class ResetFrame(
    scope: ContinuationScope,
    computation: () => Any,
    shiftValues: mutable.Map[Int, Any] = mutable.Map(),
    var isReplaying: Boolean = false,
    var result: Option[Any] = None,
    var nextShiftId: Int = 0  // Track the next shift ID for this frame
  )
  
  /**
   * Exception used to escape from shift during initial execution
   * (when we're not replaying)
   */
  case class ShiftEscape(shiftId: Int, result: Any) extends Exception
  
  /**
   * Creates a delimited continuation boundary.
   * All shift operations within this boundary are captured and can be replayed.
   */
  def reset[A](computation: () => A): A = {
    val scope = new ContinuationScope(s"reset-${System.identityHashCode(computation)}")
    val frame = ResetFrame(scope, computation.asInstanceOf[() => Any])
    val stack = resetStack.get()
    
    stack.push(frame)
    currentResetFrame.set(Some(frame))
    
    try {
      executeWithContinuation(frame)
    } finally {
      stack.pop()
      currentResetFrame.set(stack.headOption)
      // Reset the shift ID counter when exiting a reset frame
      frame.nextShiftId = 0
    }
  }
  
  /**
   * Captures the current continuation as a function that can be called multiple times.
   * When called, the function will replay the computation from the reset boundary
   * with the provided value substituted at this shift point.
   */
  def shift[A, B](handler: (A => B) => B): A = {
    val frameOpt = currentResetFrame.get()
    if (frameOpt.isEmpty) {
      throw new IllegalStateException("shift called outside of reset")
    }
    
    val frame = frameOpt.get
    
    if (frame.isReplaying) {
      // During replay, get the next shift ID and return the pre-recorded value
      val shiftId = frame.nextShiftId
      frame.nextShiftId += 1
      
      frame.shiftValues.get(shiftId) match {
        case Some(value) => 
          value.asInstanceOf[A]
        case None => throw new IllegalStateException(s"No value recorded for shift $shiftId in frame with values: ${frame.shiftValues}")
      }
    } else {
      // During initial execution, get the shift ID and create continuation
      val shiftId = frame.nextShiftId
      frame.nextShiftId += 1
      
      val continuation: A => B = { value =>
        // Create a clean copy of the frame for replay
        val replayFrame = frame.copy(
          shiftValues = frame.shiftValues.clone(),
          isReplaying = true,
          nextShiftId = 0
        )
        
        // Record this value for replay
        replayFrame.shiftValues(shiftId) = value
        
        // Update the current frame reference for the replay
        val originalFrame = currentResetFrame.get()
        currentResetFrame.set(Some(replayFrame))
        
        try {
          executeWithContinuation(replayFrame).asInstanceOf[B]
        } finally {
          // Restore the original frame
          currentResetFrame.set(originalFrame)
        }
      }
      
      // Escape from the current computation to invoke the handler
      throw ShiftEscape(shiftId, handler(continuation))
    }
  }
  
  /**
   * Executes a computation with Java continuation support.
   * Handles ShiftEscape exceptions during initial execution.
   */
  private def executeWithContinuation[A](frame: ResetFrame): A = {
    var finalResult: Option[A] = None
    
    val continuation = new Continuation(frame.scope, () => {
      try {
        val result = frame.computation()
        finalResult = Some(result.asInstanceOf[A])
      } catch {
        case ShiftEscape(shiftId, result) =>
          // During initial execution, shift escapes and returns handler result
          finalResult = Some(result.asInstanceOf[A])
      }
    })
    
    continuation.run()
    
    // Return the result that was captured during execution
    finalResult.getOrElse(
      throw new IllegalStateException("Continuation completed without producing a result")
    )
  }

// Example usage and tests
object Example:
  import ReplayShiftReset.*
  
  def main(args: Array[String]): Unit = {
    println("=== Basic shift/reset example ===")
    basicExample()
    
    println("\n=== Multiple continuation calls ===")
    multipleCalls()
    
    println("\n=== Nested shift/reset ===")
    nestedExample()
  }
  
  def basicExample(): Unit = {
    val result = reset { () =>
      val x = 10
      val y = shift[Int, Int] { k =>
        println(s"In shift handler, about to call continuation")
        k(5)
      }
      println(s"After shift: x=$x, y=$y")
      x + y
    }
    println(s"Final result: $result") // Should be 15
  }
  
  def multipleCalls(): Unit = {
    val result = reset { () =>
      val x = 10
      val y = shift[Int, Int] { k =>
        println("Calling continuation with 5 and 7")
        k(5) + k(7)
      }
      println(s"Computing: $x + $y")
      x + y
    }
    println(s"Final result: $result") // Should be 32 ((10+5) + (10+7))
  }
  
  def nestedExample(): Unit = {
    val result = reset { () =>
      val outer = shift[Int, Int] { k1 =>
        reset { () =>
          val inner = shift[Int, Int] { k2 =>
            k1(k2(10))
          }
          inner + 1
        }
      }
      outer + 100
    }
    println(s"Nested result: $result") // Should be 111 (10 + 1 + 100)
  }

// Advanced examples showing the power of delimited continuations
object AdvancedExamples:
  import ReplayShiftReset.*
  
  // Simulate exceptions using shift/reset
  def simulateExceptions(): Unit = {
    println("=== Exception simulation ===")
    
    def raise[A](error: String): A = shift[A, String] { k =>
      s"Error: $error"
    }
    
    def handle[A](computation: () => A)(handler: String => A): A = {
      reset { () =>
        try {
          s"Success: ${computation()}"
        } catch {
          case _: Exception => "Unexpected error"
        }
      } match {
        case result if result.startsWith("Success:") => 
          result.drop(9).asInstanceOf[A] // Remove "Success: " prefix
        case error if error.startsWith("Error:") => 
          handler(error.drop(7)) // Remove "Error: " prefix
        case other => other.asInstanceOf[A]
      }
    }
    
    val result1 = handle(() => 42)(_ => -1)
    println(s"Normal case: $result1")
    
    val result2 = handle(() => raise[Int]("division by zero"))(msg => {
      println(s"Caught: $msg")
      0
    })
    println(s"Exception case: $result2")
  }
  
  // Simulate nondeterminism using shift/reset
  def simulateNondeterminism(): Unit = {
    println("=== Nondeterminism simulation ===")
    
    def choose[A](options: List[A]): A = shift[A, List[A]] { k =>
      options.flatMap(k)
    }
    
    def fail[A](): A = shift[A, List[A]] { k =>
      List.empty[A]
    }
    
    val results = reset { () =>
      val x = choose(List(1, 2, 3))
      val y = choose(List(10, 20))
      val product = x * y
      if (product > 25) List(product) else fail()
    }
    
    println(s"Nondeterministic results: $results") // Should be List(30, 60)
  }