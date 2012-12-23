package com.totesoft.HierarchicalStateMachines

/**
  * A trait which, when mixed into a top level [[StateMachine]] will activate tracing to
  * the standard output.
  */
trait ConsoleLogging {
    this: StateMachine =>
    
    final override def eventLog(message: => String) = println(message)
    
}

/**
  * A trait which, when mixed into a top level [[StateMachine]] will activate tracing of
  * Lifecycle events.
  * 
  * Note: effective tracing will only occur if the top level [[StateMachine]] also overrides
  * method [[StateMachine]] (which can also be achieved by mixing trait [[ConsoleLogging]]
  * in the top level '''StateMachine''').
  */
trait LifecycleLogging {
    this: StateMachine =>
    
    final override def lifecycleLog(message: => String) = eventLog(message)
    
}


/**
  * A trait which, when mixed into a top level [[StateMachine]] will activate tracing to
  * the standard output for both event handling and lifecycle changes.
  */
trait FullConsoleLogging extends ConsoleLogging with LifecycleLogging {
    this: StateMachine =>
}