package com.totesoft.HierarchicalStateMachines

/**
  * A trait which, when mixed into a '''RootStateMachine''' will activate tracing to
  * the standard output.
  */
trait ConsoleLogging {
	this: RootStateMachine =>
	
	final override def eventLog(message: => String) = println(message)
	
}

/**
  * A trait which, when mixed into a '''RootStateMachine''' will activate tracing of Lifecycle events.
  * 
  * Note: effective tracing will only occur if the '''RootStateMachine''' also overrides
  * method '''eventLog''' (which can also be achieved by mixing trait '''ConsoleLogging'''
  * in the '''RootStateMachine''').
  */
trait LifecycleLogging {
	this: RootStateMachine =>
	
	final override def lifecycleLog(message: => String) = eventLog(message)
	
}
