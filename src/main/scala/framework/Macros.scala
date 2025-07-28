import scala.quoted.*
import scala.util.control.ControlThrowable

// ============================================================================
// Core Shift/Reset Implementation
// ============================================================================

object DelimitedControl:
  // Meta-continuation storage (thread-local for safety)
  private val metaCont = new ThreadLocal[Any => Any]:
    override def initialValue(): Any => Any = identity

  // Exception type for escaping with shift - with proper extractor
  private class ShiftEscape(val value: Any) extends ControlThrowable
  
  // Add extractor for pattern matching
  private object ShiftEscape:
    def unapply(e: ShiftEscape): Option[Any] = Some(e.value)
    def apply(value: Any): ShiftEscape = new ShiftEscape(value)

  def reset[A](computation: () => A): A =
    val saved = metaCont.get()
    try
      metaCont.set(identity)
      computation()
    catch
      case ShiftEscape(value) => 
        val k = metaCont.get()
        k(value).asInstanceOf[A]
    finally
      metaCont.set(saved)

  def shift[A, B](f: (A => B) => B): A =
    val currentCont = metaCont.get()
    val captured: A => B = (a: A) => currentCont(a).asInstanceOf[B]
    val result = f(captured)
    metaCont.set((_: Any) => result)
    throw ShiftEscape(result)

// ============================================================================
// Working State Effect Implementation
// ============================================================================

object StateEffect:
  // Use ThreadLocal for state storage - clean and simple
  private val stateStorage = new ThreadLocal[Any]()

  /**
   * Run a stateful computation in direct style
   */
  def runState[S, A](initialState: S)(computation: () => A): (A, S) =
    val savedState = stateStorage.get()
    stateStorage.set(initialState)
    try
      // Use reset to create delimited context
      val result = DelimitedControl.reset { () =>
        computation()
      }
      val finalState = stateStorage.get().asInstanceOf[S]
      (result, finalState)
    finally
      stateStorage.set(savedState)

  /**
   * Get current state
   */
  def get[S](): S = 
    val currentState = stateStorage.get()
    if currentState == null then
      throw new IllegalStateException("No state context - must be called within runState")
    currentState.asInstanceOf[S]

  /**
   * Set new state
   */
  def put[S](newState: S): Unit =
    stateStorage.set(newState)

  /**
   * Modify current state
   */
  def modify[S](f: S => S): Unit =
    val currentState = get[S]()
    put(f(currentState))

  /**
   * Get and transform state in one operation
   */
  def gets[S, A](f: S => A): A =
    f(get[S]())

// ============================================================================
// Direct Style API
// ============================================================================

object StateDSL:
  /**
   * Simple wrapper that automatically wraps code blocks in runState
   */
  transparent inline def stateful[S, A](initialState: S)(inline body: A): (A, S) =
    StateEffect.runState(initialState)(() => body)

  /**
   * State variable that provides direct assignment syntax
   */
  case class StateVar[S]():
    def apply(): S = StateEffect.get[S]()
    def update(value: S): Unit = StateEffect.put(value)
    def modifyState(f: S => S): Unit = StateEffect.modify(f)

  /**
   * Create a state variable accessor
   */
  inline def state[S]: StateVar[S] = StateVar[S]()

// ============================================================================
// Examples and Usage
// ============================================================================

object StateExamples:
  import StateEffect.*

  // Example 1: Simple counter
  def counterExample(): (Int, Int) =
    runState(0) { () =>
      val x = get[Int]()          // x = 0
      put(x + 1)                  // state = 1  
      val y = get[Int]()          // y = 1
      put(y * 2)                  // state = 2
      val z = get[Int]()          // z = 2
      x + y + z                   // return 0 + 1 + 2 = 3
    }

  // Example 2: Compare with traditional monadic style
  case class StateM[S, A](run: S => (A, S))
  
  def monadicCounter(): StateM[Int, Int] =
    StateM { s0 =>
      val (x, s1) = (s0, s0)           // get
      val (_, s2) = ((), s1 + 1)       // put(s1 + 1)  
      val (y, s3) = (s2, s2)           // get
      val (_, s4) = ((), s3 * 2)       // put(s3 * 2)
      val (z, s5) = (s4, s4)           // get
      (x + y + z, s5)
    }

  // Our direct style is much cleaner!
  def directCounter(): (Int, Int) = runState(0) { () =>
    val x = get[Int]()
    put(x + 1)
    val y = get[Int]()  
    put(y * 2)
    val z = get[Int]()
    x + y + z
  }

  // Example 3: Stack operations
  case class Stack[A](items: List[A]):
    def isEmpty: Boolean = items.isEmpty
    def size: Int = items.length
  
  def push[A](item: A): Unit =
    modify[Stack[A]](stack => Stack(item :: stack.items))
  
  def pop[A](): Option[A] =
    val stack = get[Stack[A]]()
    stack.items match
      case head :: tail => 
        put(Stack(tail))
        Some(head)
      case Nil => None

  def stackExample(): (List[Option[Int]], Stack[Int]) =
    runState(Stack[Int](Nil)) { () =>
      push(1)
      push(2) 
      push(3)
      List(pop[Int](), pop[Int](), pop[Int](), pop[Int]())
    }

  // Example 4: Using the DSL with variable-like syntax
  import StateDSL.*
  
  def dslExample(): (String, Int) = stateful(0) {
    val s = state[Int]
    val count = s()           // equivalent to get()
    s() = count + 5           // equivalent to put(count + 5)  
    val newCount = s()
    s.modifyState(_ * 2)      // equivalent to modify(_ * 2)
    val finalCount = s()
    s"Count: $count -> $newCount -> $finalCount"
  }

// ============================================================================
// Testing
// ============================================================================

@main def testStateEffect(): Unit =
  import StateExamples.*
  
  println("=== State Effect Examples ===")
  
  println(s"Counter: ${counterExample()}")
  // Expected: (3, 2)
  
  println(s"Direct vs Monadic:")
  println(s"  Direct: ${directCounter()}")
  println(s"  Monadic: ${monadicCounter().run(0)}")
  
  println(s"Stack operations: ${stackExample()}")
  
  println(s"DSL with variable syntax: ${dslExample()}")
  
  // Test that state is properly isolated between different runState calls
  println("\n=== Testing State Isolation ===")
  
  val (result1, state1) = StateEffect.runState(100) { () =>
    StateEffect.put(200)
    "first"
  }
  
  val (result2, state2) = StateEffect.runState(300) { () =>
    StateEffect.put(400)  
    "second"
  }
  
  println(s"First computation: ($result1, $state1)")
  println(s"Second computation: ($result2, $state2)")
  
  assert(result1 == "first" && state1 == 200)
  assert(result2 == "second" && state2 == 400)
  println("State isolation test passed! ✅")
  
  // Test nested state contexts
  println("\n=== Testing Nested State ===")
  
  val (outerResult, outerState) = StateEffect.runState(1) { () =>
    val x = StateEffect.get[Int]()
    StateEffect.put(x + 10)  // state = 11
    
    val (innerResult, innerState) = StateEffect.runState(100) { () =>
      val y = StateEffect.get[Int]()
      StateEffect.put(y + 20)  // inner state = 120
      y + 1000  // return 1100
    }
    
    val z = StateEffect.get[Int]()  // should still be 11
    StateEffect.put(z + 1)  // state = 12
    x + innerResult  // 1 + 1100 = 1101
  }
  
  println(s"Nested result: ($outerResult, $outerState)")
  assert(outerResult == 1101 && outerState == 12)
  println("Nested state test passed! ✅")
  
  // Performance comparison
  println("\n=== Performance Test ===")
  
  def timeIt[A](name: String)(f: => A): A =
    val start = System.nanoTime()
    val result = f
    val end = System.nanoTime()
    println(s"$name: ${(end - start) / 1_000_000}ms")
    result
  
  def manualState(n: Int): (Int, Int) =
    def loop(i: Int, acc: Int, state: Int): (Int, Int) =
      if i <= 0 then (acc, state)
      else loop(i - 1, acc + state, state + 1)
    loop(n, 0, 0)
  
  def stateEffectVersion(n: Int): (Int, Int) =
    StateEffect.runState(0) { () =>
      def loop(i: Int, acc: Int): Int =
        if i <= 0 then acc
        else
          val state = StateEffect.get[Int]()
          StateEffect.put(state + 1)
          loop(i - 1, acc + state)
      loop(n, 0)
    }
  
  val n = 1000
  timeIt("Manual state threading") { manualState(n) }
  timeIt("State effect version") { stateEffectVersion(n) }
  
  // Final assertions
  assert(counterExample() == (3, 2))
  assert(directCounter() == monadicCounter().run(0))
  
  println("\nAll tests passed! ✅")
  
  println("\n=== Implementation Notes ===")
  println("This implementation demonstrates:")
  println("- Direct style programming with effects")
  println("- Clean state isolation between contexts")
  println("- Proper nesting of stateful computations")
  println("- ThreadLocal-based state storage with reset delimitation")
  println("- Foundation for extending to full shift/reset implementation")