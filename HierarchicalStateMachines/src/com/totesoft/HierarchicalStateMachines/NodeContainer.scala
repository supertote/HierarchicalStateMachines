package com.totesoft.HierarchicalStateMachines

/**
  * The NodeContainer trait defines the attributes and methods common to all [[Node]]s which contain
  * sub nodes, i.e. [[RootStateMachine]]s and [[Container.ChildStateMachine]]s
  */
trait NodeContainer extends Node {
    
    /**
      * Define the events which can lead to terminating this state machine
      */
    type ExitEvent
    
    
    /**
      * Define the Transitions which can be performed within this state machine
      */
    sealed trait InnerTransition
    /**
      * Done means the event has been handled by sub states and nothing needs to
      * be done; the state machine will also return Done to its container
      */
    case object Done                      extends InnerTransition
    /**
      * Delegate means the event has not been handled by sub states; the event will be
      * re-dispatched into this state machine; if the resulting transition is also
      * Delegate, the state machine will return Delegate to its container
      */
    case object Delegate                  extends InnerTransition
    /**
      * MoveTo(state) means that the current state of this state machine must be changed
      * to the specified state in which an Enter event will be fired; the state machine will
      * return Done to its container
      */
    case class  MoveTo(state: Child)      extends InnerTransition
    /**
      * Resume(state) means that the current state of this state machine must be changed
      * to the specified state in which a Resume event will be fired; the state machine will
      * return Done to its container
      */
    case class  Resume(state: Child)      extends InnerTransition
    /**
      * Terminate(evt) means that this state machine must be terminated; in this case, the
      * specified event will be re-dispatched into this state machine's '''terminate'''
      * event handler and the resulting value will be returned to the container
      */
    case class  Terminate(evt: ExitEvent) extends InnerTransition
    /**
      * Error(err) means that some error occurred during input event processing; in this
      * case, the specified error message will be re-dispatched into this state machines's
      * '''onError''' method and the resulting value will be returned to the container
      */
    case class  Error(err: String)        extends InnerTransition
    
    
    /**
      * A trait defining the common attributes of sub states and sub state machines of a NodeContainer.
      */
    sealed trait Child extends Node {
        
        type OuterTransition = NodeContainer.this.InnerTransition
        
        final override def container = Some(NodeContainer.this)
        
    }
    
    
    /**
      * A trait which can be mixed into a class/trait to define a sub state of a NodeContainer.
      */
    trait ChildState extends Child {
        
	    /**
	      * The event handler of this state machine used to handle input events
	      */
        val events = new EventHandler[Any, OuterTransition]("    Handling ", _ => Error("No event handler defined in " + this))
        
        
        final override def onEvent(evt : Any) = events.run(evt)
        
        final override def onEnter(evt: Any) = enter.run(evt)
        
        final override def onResume(evt: Any) = if (historyType == HistoryType.NONE) enter.run(evt) else resume.run(evt)
        
        final override def onExit(evt: Any) = exit.run(evt)
        
    }
    
    
    /**
      * A Concrete implementation of a sub state
      * 
      * @constructor Construct a new sub state with the specified name and history type
      * 
      * @param name The name of the new sub state machine
      * @param historyType The history type of the new state machine
      */
    class State(override val name: String, override val historyType: HistoryType = HistoryType.NONE) extends ChildState {
        events := { _ => Delegate }
    }
    
    
    /**
      * A trait which can be mixed into a class/trait to define a sub state machine of a NodeContainer.
      * 
      * Note that when mixing this trait you must also mix the NodeContainer trait
      */
    trait ChildStateMachine extends Child {
        this : NodeContainer =>
        
            
        final override def outerDelegate = NodeContainer.this.Delegate
        
        
        final override def outerDone = NodeContainer.this.Done
        
        
        override def onError(err: String) = NodeContainer.this.Error(err)
        
        
        override def terminateNotSet = NodeContainer.this.Error("No terminate handler defined in " + this)
        
    }
    
    
    /**
      * A Concrete implementation of a sub state machine
      * 
      * @constructor Construct a new sub state machine with the specified name and history type
      * 
      * @param name The name of the new sub state machine
      * @param historyType The history type of the new state machine
      */
    class StateMachine[T](override val name: String, override val historyType: HistoryType = HistoryType.NONE) extends NodeContainer with ChildStateMachine {
        type ExitEvent = T
    }
    
    
    /**
      * The current sub-state of this state machine
      */
    protected[HierarchicalStateMachines] var currentState: Option[Child] = None
    /**
      * The last sub-state of this state machine saved when this state machine was exited
      */
    private[this] var historyState: Option[Child] = None
    /**
      * The previous sub-state of this state machine
      */
    private[this] var prevState: Option[Child] = None
    /**
      * The next sub-state of this state machine
      */
    private[this] var nexState:  Option[Child] = None
    
    
    /**
      * The event handler of this state machine used to handle input events
      */
    val events = new EventHandler[Any, InnerTransition]("    Handling ", _ => Error("No event handler defined in " + NodeContainer.this))
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
    protected final def previousState: Option[Child] = prevState
    
    
    /**
      * Get the next sub-state of this state machine. This value is only valid during a state
      * change. Otherwise it will return None
      * 
      * @return the next sub-state of this state machine or none if no state change is pending
      */
    protected final def nextState: Option[Child] = nexState
    
    
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
            case Some(s : NodeContainer) => s.deepState
            case Some(s)             => s
            case _                   => NodeContainer.this
        }
    }
    
    
    // TODO: Comment
    protected[HierarchicalStateMachines] def terminateNotSet: OuterTransition
    
    
    /**
      * Get the equivalent of a Done transition in this state machine's container
      */
    protected[HierarchicalStateMachines] def outerDone: OuterTransition
    
    
    /**
      * Get the equivalent of a Delegate transition in this state machine's container
      */
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
    def initialState(evt: Any): Option[Child] = None
    
    
    /**
      * Replace the current sub-state with the specified one and trigger the specified lifecyle
      * event into the new state
      * 
      * @param evt The event which triggered the state change
      * @param newState The new sub-state which will become the current sub-state of this state machine
      * @param lifecycleChange A method which will trigger a lifecycle event into the new sub-state
      */
    private[this] def changeState(evt: Any, newState: Child, lifecycleChange: (Child, Any) => Unit): Unit = {
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
                    if (NodeContainer.this eq node) outerDelegate
                    else performTransition(evt, NodeContainer.this, e => events.run(evt))
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
                performTransition(evt, NodeContainer.this, e => events.run(e))
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

