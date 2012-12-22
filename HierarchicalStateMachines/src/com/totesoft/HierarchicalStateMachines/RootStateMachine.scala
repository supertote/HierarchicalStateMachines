package com.totesoft.HierarchicalStateMachines


trait RootStateMachine extends StateContainer {
    
    type ExitEvent = Unit
    
    type OuterTransition = Status
    
    
    sealed trait Status
    case object Terminated extends Status
    case object InProgress extends Status
    case class InError(error: String) extends Status
    
    
    final def fire(evt : Any): Status = {
        eventLog("Firing " + evt + " in " + RootStateMachine.this + " {")
        
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
    
    
    final override def onDone = currentStatus
    
    
    final override def onDelegate = InProgress
    
    
    protected def throwOnError(err : String): Unit = {}
    
    
    protected[HierarchicalStateMachines] def cantOverrideFrozenHandler(err : String): Unit = {}
    
    
    final override def onError(err: String) = {
        throwOnError(err)
        var result = InError(err)
        reset(result)
        result
    }
    
    
    override def terminateNotSet = InError("No terminate handler defined in " + RootStateMachine.this)
    
    
    private[this] def reset(on: Any) = {
        onExit(on)
        onEnter(on)
    }
    
    
    private[this] def doTerminate(on: Any): Status = {
        reset(on)
        Terminated
    }
    
    
    terminate { e => doTerminate(e) } freeze
    
    onEnter(())
}

