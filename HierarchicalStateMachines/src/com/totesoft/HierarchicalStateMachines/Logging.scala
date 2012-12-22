package com.totesoft.HierarchicalStateMachines

trait ConsoleLogging {
	this: RootStateMachine =>
	
	final override def eventLog(message: => String) = println(message)
	
}

trait LifecycleLogging {
	this: RootStateMachine =>
	
	final override def lifecycleLog(message: => String) = eventLog(message)
	
}
