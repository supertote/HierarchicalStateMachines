package com.totesoft.HierarchicalStateMachines

/**
  * A specialization of [[NodeContainer]] which can be mixed into a class/trait to
  * construct top-level state machines
  */
trait StateMachine extends NodeContainer {
    
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
    case class InError(error: String) extends Status
    
    
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
    
    
    protected[HierarchicalStateMachines] def eventLog(message: => String) = {}
    
    
    protected[HierarchicalStateMachines] def lifecycleLog(message: => String) = {}
    
    
    private[this] def currentStatus: Status = {
        currentState match {
            case Some(_) => InProgress
            case None => Terminated
        }
    }
    
    
    final override def container = None
    
    
    final override def outerDone = currentStatus
    
    
    final override def outerDelegate = InProgress
    
    
    protected def throwOnError(err : String): Unit = {}
    
    
    protected[HierarchicalStateMachines] def cantOverrideFrozenHandler(err : String): Unit = {}
    
    
    final override def onError(err: String) = {
        throwOnError(err)
        var result = InError(err)
        reset(result)
        result
    }
    
    
    private[this] def reset(on: Any) = {
        onExit(on)
        onEnter(on)
    }
    
    
    final override def outerError(msg: String): OuterTransition = InError(msg)
    
    
    terminate := { e => reset(e); Terminated } freeze
    
    onEnter(())
}


object StateMachine {
    def apply(n: String, hType: HistoryType = HistoryType.NONE) = new StateMachine {
        override val name = n
        override val historyType = hType
    }
}
