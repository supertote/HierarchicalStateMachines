package com.totesoft.HierarchicalStateMachines

/**
  * The Container trait defines the attributes and methods common to all [[Nodes]] which contain
  * other nodes, i.e. [[RootStateMachine]]s and [[Container.ChildStateMachine]]
  */
trait Container extends Node {
    
    type ExitEvent
    
    type SubState = ChildNode
    
    
    sealed trait InnerTransition
    case object Done                      extends InnerTransition
    case object Delegate                  extends InnerTransition
    case class  Error(err: String)        extends InnerTransition
    case class  MoveTo(state: SubState)   extends InnerTransition
    case class  Resume(state: SubState)   extends InnerTransition
    case class  Terminate(evt: ExitEvent) extends InnerTransition
    
    
    sealed trait ChildNode extends Node {
        
        type OuterTransition = Container.this.InnerTransition
        
        final override def container = Some(Container.this)
        
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
        this : Container =>
        
            
        final override def outerDelegate = Container.this.Delegate
        
        
        final override def outerDone = Container.this.Done
        
        
        override def onError(err: String) = Container.this.Error(err)
        
        
        override def terminateNotSet = Container.this.Error("No terminate handler defined in " + this)
        
    }
    
    
    class StateMachine[T](override val name: String, override val historyType: HistoryType = HistoryType.NONE) extends Container with ChildStateMachine {
        type ExitEvent = T
    }
    
    
    /**
      * The current sub-state of this state machine
      */
    protected[HierarchicalStateMachines] var currentState: Option[SubState] = None
    /**
      * The last sub-state of this state machine saved when this state machine was exited
      */
    private[this] var historyState: Option[SubState] = None
    /**
      * The previous sub-state of this state machine
      */
    private[this] var prevState: Option[SubState] = None
    /**
      * The next sub-state of this state machine
      */
    private[this] var nexState:  Option[SubState] = None
    
    
    /**
      * The event handler of this state machine used to handle input events
      */
    val events = new EventHandler[Any, InnerTransition]("    Handling ", _ => Error("No event handler defined in " + Container.this))
    /**
      * The event handler of this state machine used to handle exit events
      */
    val terminate = new EventHandler[ExitEvent, OuterTransition]("    Terminating ", _ => terminateNotSet)
    
    
    /**
      * Get the previous sub-state of this state machine. This value is only valid during a state
      * change. Otherwise it will return None
      * 
      * @return the previous sub-state of this state machine or none if no state change is pending
      */
    protected final def previousState: Option[SubState] = prevState
    
    
    /**
      * Get the next sub-state of this state machine. This value is only valid during a state
      * change. Otherwise it will return None
      * 
      * @return the next sub-state of this state machine or none if no state change is pending
      */
    protected final def nextState: Option[SubState] = nexState
    
    
    /**
      * Get the deepest active (sub-)sub-state of this state machine
      * 
      * @return
      * - this state machine if it has no sub-state
      * - this state machine's sub-state if the sub-state is not a state machine
      * - this state machine's sub-state's deepState otherwise
      */
    final def deepState: Node = {
        currentState match {
            case Some(s : Container) => s.deepState
            case Some(s)             => s
            case _                   => this
        }
    }
    
    
    protected[HierarchicalStateMachines] def terminateNotSet: OuterTransition
    
    
    protected[HierarchicalStateMachines] def outerDone: OuterTransition
    
    
    protected[HierarchicalStateMachines] def outerDelegate: OuterTransition
    
    
    /**
      * Transforms the specified error message into a transition.
      * 
      * State machines can override this method to recover from errors which occur during
      * event dispatching
      * 
      * @param err The error message
      * 
      * @return The resulting transition
      */
    def onError(err: String): OuterTransition
    
    
    /**
      * Compute the initial sub-state of this state machine (upon entering into this state machine)
      * 
      * By default the result will be None.
      * 
      * @param evt The event which triggered entering this state machine
      * 
      * @return The initial sub-state or None if this state machine does not have an initial sub-state
      */
    def initialState(evt: Any): Option[SubState] = None
    
    
    /**
      * Replace the current sub-state with the specified one and trigger the specified lifecyle
      * event into the new state
      * 
      * @param evt The event which triggered the state change
      * @param newState The new sub-state which will become the current sub-state of this state machine
      * @param lifecycleChange A method which will trigger a lifecycle event into the new sub-state
      */
    private[this] def changeState(evt: Any, newState: SubState, lifecycleChange: (SubState, Any) => Unit): Unit = {
        nexState = if (newState != null) Some(newState) else None
        
        currentState match {
            case Some(s) => s.onExit(evt)
            case _       =>
        }
        
        prevState = currentState
        currentState = nexState
        
        currentState match {
            case Some(s) => lifecycleChange(s, evt)
            case _       =>
        }
        
        nexState = None
        prevState = None
    }
    
    
    /** 
      * Trigger the specified event handling on the specified event and apply the resulting transition
      * 
      * @param evt The input event
      * @node The node in which the event handler is fired
      * @handleEvent The method which will handle the event
      */
    private[this] def performTransition(evt: Any, node: Node, handleEvent: Any => InnerTransition): OuterTransition = {
        try {
            handleEvent(evt) match {
                case Done =>
                    outerDone
                case Delegate =>
                    if (Container.this eq node) outerDelegate
                    else performTransition(evt, Container.this, e => events.run(evt))
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
    
    
    /**
      * Triggers the enter or resume handler, depending on the specified history value
      * 
      * @param evt The event to which triggered the entering/resumption
      * @param hType The history type which will allow to determine which handler to trigger
      */
    private[this] final def onEnterOrResume(evt: Any, hType : HistoryType) = {
        if (hType == HistoryType.NONE)
            enter.run(evt)
        else
            resume.run(evt)
            
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

    
    final override def onEvent(evt: Any) = {
        currentState match {
            case Some(subState) =>
                performTransition(evt, subState, e => subState.onEvent(e))
            case None =>
                performTransition(evt, Container.this, e => events.run(e))
        }
    }
    

    final override def onEnter(evt: Any) = {
        onEnterOrResume(evt, HistoryType.NONE)
    }
    
    
    final override def onResume(evt: Any) = {
        onEnterOrResume(evt, historyType)
    }
    
    
    final override def onExit(evt: Any) = {
        historyState = currentState
        currentState = None
        
        historyState match {
            case Some(s) => s.onExit(evt)
            case _ =>
        }
        
        if (historyState == HistoryType.NONE) historyState = None
        
        exit.run(evt)
    }
    
}

