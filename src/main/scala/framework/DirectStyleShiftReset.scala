// State Effect Implementation using shift/reset
// Let's focus on getting this right step by step

import ReplayShiftReset.{shift, reset}

/**
 * The key insight: State effects transform the "rest of the computation"
 * from `A` to `S => (A, S)` - a function that takes initial state 
 * and returns final result with final state.
 */
object StateEffectDeepDive:

  trait CanState[S]

  // Let's start with the simplest possible version and build up
  object SimpleState:
    
    def get[S](using CanState[S]): S = 
      shift[S, S => (S, S)] { cont =>
        state => cont(state)(state)  // Pass state to continuation, keep same state
      }
    
    def put[S](newState: S)(using CanState[S]): Unit = 
      shift[Unit, S => (Unit, S)] { cont =>
        _ => cont(())(newState)  // Pass (), set new state
      }
    
    def runState[S, A](initialState: S)(computation: CanState[S] ?=> A): (A, S) =
      val stateTransformer = reset { () =>
        given CanState[S] = new CanState[S] {}
        val result = computation
        // Return a state transformer: S => (A, S)
        (state: S) => (result, state)
      }
      stateTransformer(initialState)

  // Let's test this step by step
  object TestSimpleState:
    import SimpleState.*
    
    def testGet(): Unit = {
      println("=== Testing simple get ===")
      val (result, finalState) = runState(42) {
        val x = get[Int]
        x * 2
      }
      println(s"Result: $result, Final state: $finalState")
      // Expected: Result: 84, Final state: 42
    }
    
    def testPut(): Unit = {
      println("=== Testing simple put ===")
      val (result, finalState) = runState(10) {
        put(99)
        "done"
      }
      println(s"Result: $result, Final state: $finalState")
      // Expected: Result: done, Final state: 99
    }
    
    def testGetPut(): Unit = {
      println("=== Testing get then put ===")
      val (result, finalState) = runState(5) {
        val current = get[Int]
        put(current * 2)
        current
      }
      println(s"Result: $result, Final state: $finalState")
      // Expected: Result: 5, Final state: 10
    }

  // Now let's implement a more sophisticated version that handles multiple operations
  object AdvancedState:
    
    def get[S](using CanState[S]): S = 
      shift[S, S => (S, S)] { cont =>
        // cont: S => S => (S, S)
        // We need: S => (S, S)
        state => 
          val (resultValue, newState) = cont(state)(state)
          (resultValue, newState)
      }
    
    def put[S](newState: S)(using CanState[S]): Unit = 
      shift[Unit, S => (Unit, S)] { cont =>
        // cont: Unit => S => (Unit, S) 
        // We need: S => (Unit, S)
        _ => 
          val (resultValue, _) = cont(())(newState)
          (resultValue, newState)
      }
    
    def runState[S, A](initialState: S)(computation: CanState[S] ?=> A): (A, S) =
      val stateTransformer = reset { () =>
        given CanState[S] = new CanState[S] {}
        val result = computation
        // The result should be a state transformer
        (state: S) => (result, state)
      }
      stateTransformer(initialState)

  // Test the advanced version
  object TestAdvancedState:
    import AdvancedState.*
    
    def testSequence(): Unit = {
      println("=== Testing sequence of operations ===")
      val (result, finalState) = runState(1) {
        val a = get[Int]           // a = 1
        put(a + 10)               // state = 11
        val b = get[Int]           // b = 11  
        put(b * 2)                // state = 22
        val c = get[Int]           // c = 22
        a + b + c                 // result = 1 + 11 + 22 = 34
      }
      println(s"Result: $result, Final state: $finalState")
      // Expected: Result: 34, Final state: 22
    }

  // Let's try a different approach - explicit state threading
  object ExplicitThreading:
    
    def get[S](using CanState[S]): S = 
      shift[S, S => (S, S)] { cont =>
        state => cont(state)(state)
      }
    
    def put[S](newState: S)(using CanState[S]): Unit = 
      shift[Unit, S => (Unit, S)] { cont =>
        _ => cont(())(newState) 
      }
    
    def modify[S](f: S => S)(using CanState[S]): Unit = 
      val current = get[S]
      put(f(current))
    
    def runState[S, A](initialState: S)(computation: CanState[S] ?=> A): (A, S) =
      // The key insight: reset should produce a state transformer function
      val transformer: S => (A, S) = reset { () =>
        given CanState[S] = new CanState[S] {}
        val result = computation
        // This lambda is the state transformer
        (inputState: S) => (result, inputState)
      }
      transformer(initialState)

  // Debug version with lots of logging
  object DebugState:
    
    def get[S](using CanState[S]): S = {
      println(s"STATE: get() called")
      shift[S, S => (S, S)] { cont =>
        println(s"STATE: get() shift handler called")
        state => {
          println(s"STATE: get() applying to state: $state")
          val result = cont(state)(state)
          println(s"STATE: get() result: $result")
          result
        }
      }
    }
    
    def put[S](newState: S)(using CanState[S]): Unit = {
      println(s"STATE: put($newState) called")
      shift[Unit, S => (Unit, S)] { cont =>
        println(s"STATE: put() shift handler called")
        _ => {
          println(s"STATE: put() setting state to: $newState")
          val result = cont(())(newState)
          println(s"STATE: put() result: $result")
          result
        }
      }
    }
    
    def runState[S, A](initialState: S)(computation: CanState[S] ?=> A): (A, S) = {
      println(s"STATE: runState called with initial state: $initialState")
      val transformer = reset { () =>
        println(s"STATE: Inside reset")
        given CanState[S] = new CanState[S] {}
        println(s"STATE: About to run computation")
        val result = computation
        println(s"STATE: Computation completed with result: $result")
        (inputState: S) => {
          println(s"STATE: Transformer called with state: $inputState")
          (result, inputState)
        }
      }
      println(s"STATE: About to apply transformer")
      val finalResult = transformer(initialState) 
      println(s"STATE: Final result: $finalResult")
      finalResult
    }

  // Main test runner
  def main(args: Array[String]): Unit = {
    println("=== Testing Simple State ===")
    TestSimpleState.testGet()
    TestSimpleState.testPut() 
    TestSimpleState.testGetPut()
    
    println("\n=== Testing Advanced State ===")
    TestAdvancedState.testSequence()
    
    println("\n=== Debug Version ===")
    val (result, state) = DebugState.runState(100) {
      DebugState.get[Int]
    }
    println(s"Debug result: $result, state: $state")
  }

/**
 * Theory refresher: How state effects work with delimited continuations
 * 
 * The state monad transforms:
 *   A  =>  S => (A, S)
 * 
 * get: S => (S, S)  -- returns current state, doesn't change it
 * put: S => ((), S) -- returns unit, sets new state
 * 
 * With shift/reset:
 * - get captures continuation k: S => ...
 * - transforms it to: state => k(state)(state)
 * - put captures continuation k: () => ...  
 * - transforms it to: _ => k(())(newState)
 * 
 * The reset boundary collects all these transformations and 
 * produces a final state transformer function.
 */
