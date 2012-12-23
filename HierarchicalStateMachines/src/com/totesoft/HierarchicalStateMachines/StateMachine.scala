package com.totesoft.HierarchicalStateMachines

/**
  * A specialization of [[StateContainer]] which can be mixed into a class/trait to
  * construct top-level state machines
  */
trait StateMachine extends StateContainer {
    
    type ExitEvent = Unit
    
    type OuterTransition = Status
    
    
    /**
      * The status returned when firing an event into a top-level state machine
      */
    sealed trait Status
    /**
      * Everything went well and the sate machine's current state is unset
      */
    case object Terminated extends Status
    /**
      * Everything went well and the sate machine's current state is set
      */
    case object InProgress extends Status
    /**
      * Some error was encountered
      */
    case class InError(err: DispatchError) extends Status
    
    
    /**
      * Fire the specified event into the state machine
      * 
      * @param evt: The event
      * 
      * @return the status representing the result of firing the event
      */
    final def fire(evt : Any): Status = {
        eventLog("Firing " + evt + " in " + StateMachine.this + " {")
        
        var result = onEvent(evt)
        
        eventLog("} => " + result + (result match {
            case InProgress => " - deep state = " + deepState + "\n"
            case _          => ""
        }))
        
        result
    }
    
    
    /**
      * Log the specified message
      * 
      * @param message The message to log
      */
    protected[HierarchicalStateMachines] def eventLog(message: => String) = {}
    
    
    /**
      * Log a lifecycle event described by the specified message
      * 
      * @param message The message to log
      */
    protected[HierarchicalStateMachines] def lifecycleLog(message: => String) = {}
    
    
    /**
      * By default, when trying to override an event handler which is frozen in one of this
      * state machine's sub-state will return an error, the attempt will simply be ignored.
      * 
      * To change this behavior, a top-level state machine can override this method to throw an
      * exception instead
      */
    protected[HierarchicalStateMachines] def cantOverrideFrozenHandler(err : String): Unit = {}
    
    
    /**
      * By default, if firing an event within a state machine results in an error, the state machine will
      * be exited and InError will be returned.
      * 
      * To change this behavior, a top-level state machine can override this method to throw an exception
      * instead
      * 
      * @param err The error to be handled
      * 
      * @return The input error if not overriden
      */
    protected def throwOnError(err: DispatchError): Unit = {}
    
    
    /**
      * Reset the state machine to its default state: i.e. exit the state machine
      * and re-enter it
      */
    private[this] def reset(on: Any) = {
        onExit(on)
        onEnter(on)
    }
    
    
    final override def container = None
    
    
    final override def outerDone = currentState match {
        case Some(_) => InProgress
        case None    => Terminated
    }
    
    
    final override def outerDelegate = InProgress
    
    
    final override def onError(err: DispatchError) = {
        var result = InError(err)
        reset(result)
        throwOnError(err)
        result
    }
    
    
    final override def outerError(err: DispatchError): OuterTransition = InError(err)
    
    
    // Enter the state machine
    onEnter(())
    
    // Upon termination, reset it
    terminate := { e => reset(e); Terminated } freeze
    
}


object StateMachine {
    def apply(n: String, hType: HistoryType = HistoryType.NONE) = new StateMachine {
        override val name = n
        override val historyType = hType
    }
}
