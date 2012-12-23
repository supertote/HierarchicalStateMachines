package com.totesoft.HierarchicalStateMachines


trait StateMachine extends NodeContainer {
    
    type ExitEvent = Unit
    
    type OuterTransition = Status
    
    
    sealed trait Status
    case object Terminated extends Status
    case object InProgress extends Status
    case class InError(error: String) extends Status
    
    
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
    
    
    private[this] def doTerminate(on: Any): Status = {
        reset(on)
        Terminated
    }
    
    
    final override def outerError(msg: String): OuterTransition = InError(msg)
    
    
    terminate := { e => doTerminate(e) } freeze
    
    onEnter(())
}


object StateMachine {
    def apply(n: String, hType: HistoryType = HistoryType.NONE) = new StateMachine {
        override val name = n
        override val historyType = hType
    }
}
