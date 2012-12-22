package com.totesoft.HierarchicalStateMachines

trait StateContainer extends StateNode {
    
    type ExitEvent
    
    type InnerTransition = Transition
    
    type SubState = ChildNode
    
    sealed trait Transition
    
    case object Done extends Transition
    case object Delegate extends Transition
    case class Error(err: String) extends Transition
    case class MoveTo(state: SubState) extends Transition
    case class Resume(state: SubState) extends Transition
    case class Terminate(evt: ExitEvent) extends Transition
    
    
    sealed trait ChildNode extends StateNode {
        
        type OuterTransition = StateContainer.this.InnerTransition
        
        final override def container = Some(StateContainer.this)
        
    }
    
    
    trait ChildState extends ChildNode {
        
        val events = new EventHandler[Any, OuterTransition]("    Handling ", _ => Error("No event handler defined in " + this))
        
        
        final override def onEvent(evt : Any) = events.run(evt)
        
        final override def onEnter(evt: Any) = enter.run(evt)
        
        final override def onResume(evt: Any) = if (historyType == HistoryType.NONE) enter.run(evt) else resume.run(evt)
        
        final override def onExit(evt: Any) = exit.run(evt)
        
    }
    
    
    class State(override val name: String, override val historyType: HistoryType = HistoryType.NONE) extends ChildState {
        events := { _ => Delegate }
    }
    
    
    trait ChildStateMachine extends ChildNode {
        this : StateContainer =>
        
            
        final override def outerDelegate = StateContainer.this.Delegate
        
        
        final override def outerDone = StateContainer.this.Done
        
        
        override def onError(err: String) = StateContainer.this.Error(err)
        
        
        override def terminateNotSet = StateContainer.this.Error("No terminate handler defined in " + this)
        
    }
    
    
    class StateMachine[T](override val name: String, override val historyType: HistoryType = HistoryType.NONE) extends StateContainer with ChildStateMachine {
        type ExitEvent = T
    }
    
    
    protected[HierarchicalStateMachines] var currentState: Option[SubState] = None
    private[this] var historyState: Option[SubState] = None
    private[this] var prevState: Option[SubState] = None
    protected final def previousState: Option[SubState] = prevState
    
    final def deepState: StateNode = {
        currentState match {
            case Some(s : StateContainer) => s.deepState
            case Some(s)                  => s
            case _                        => this
        }
    }
    
    
    val events = new EventHandler[Any, InnerTransition]("    Handling ", _ => Error("No event handler defined in " + this))
    
	val terminate = new EventHandler[ExitEvent, OuterTransition]("    Terminating ", _ => terminateNotSet)
    
    
    protected[HierarchicalStateMachines] def terminateNotSet: OuterTransition
    
    
    protected[HierarchicalStateMachines] def outerDone: OuterTransition
    
    
    protected[HierarchicalStateMachines] def outerDelegate: OuterTransition
    
    
    def onError(err: String): OuterTransition
    
    
    def initialState(evt: Any): Option[SubState] = None
    
    
    private[this] def changeState(evt: Any, newState: SubState, lifecycleChange: (SubState, Any) => Unit): Unit = {
        currentState match {
            case Some(s) => s.onExit(evt)
            case _ =>
        }
        
        currentState = if (newState != null) Some(newState) else None
        
        currentState match {
            case Some(s) => lifecycleChange(s, evt)
            case _ =>
        }
    }
    
    
    private[this] def performTransition(evt: Any, node: StateNode, handleEvent: Any => InnerTransition): OuterTransition = {
        try {
            handleEvent(evt) match {
                case Done =>
                	outerDone
                case Delegate =>
                    if (StateContainer.this eq node) outerDelegate
                    else performTransition(evt, StateContainer.this, e => events.run(evt))
                case MoveTo(newState) =>
                    changeState(evt, newState, (s, e) => s.onEnter(e))
                    outerDone
                case Resume(newState) =>
                    changeState(evt, newState, (s, e) => s.onResume(e))
                    outerDone
                case Terminate(exitEvent) =>
                    terminate.run(exitEvent)
                case Error(error) =>
                    onError(error)
            }
        }
        catch {
            case me : MatchError =>
                onError("Unhandled event " + evt + " in " + path)
            case e : Throwable =>
                onError(e.toString)
        }
    }
    
    
    final override def onEvent(evt: Any) = {
        currentState match {
            case Some(subState) =>
                performTransition(evt, subState, e => subState.onEvent(e))
            case None =>
                performTransition(evt, StateContainer.this, e => events.run(e))
        }
    }
    
    
    private[this] final def onEnterOrResume(evt: Any, hType : HistoryType) = {
        prevState = currentState
        currentState = if (hType == HistoryType.NONE) initialState(evt) else historyState
        
        currentState match {
            case Some(s) =>
            	if (hType == HistoryType.DEEP)
            	    s.onResume(evt)
	            else
	                s.onEnter(evt)
            case _ =>
        }
        
        historyState = None
    }


    final override def onEnter(evt: Any) = {
        enter.run(evt)
        onEnterOrResume(evt, HistoryType.NONE)
    }
    
    
    final override def onResume(evt: Any) = {
        resume.run(evt)
        onEnterOrResume(evt, historyType)
    }
    
    
    final override def onExit(evt: Any) = {
        historyState = currentState
        currentState = None
        prevState = None
        
        historyState match {
            case Some(s) => s.onExit(evt)
            case _ =>
        }
        
        if (historyState == HistoryType.NONE) historyState = None
        
        exit.run(evt)
    }
    
}

