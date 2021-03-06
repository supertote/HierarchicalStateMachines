package com.totesoft.HierarchicalStateMachines

/**
  * The StateContainer trait defines the attributes and methods common to all [[Node]]s which contain
  * sub nodes, i.e. [[StateMachine]]s and [[Container.StateMachine]]s
  */
trait StateContainer extends State {
    
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
      * case, the specified error be re-dispatched into this state machines's
      * '''onError''' method and the resulting value will be returned to the container
      */
    case class  Error(err: DispatchError) extends InnerTransition
    
    
    /**
      * A trait defining the common attributes of sub states and sub state machines of a [[StateContainer]].
      */
    sealed trait Child extends com.totesoft.HierarchicalStateMachines.State {
        
        type OuterTransition = StateContainer.this.InnerTransition
        
        final override def container = Some(StateContainer.this)
        
	    final override def outerError(err: DispatchError): OuterTransition = Error(err)
	    
    }
    
    
    /**
      * A trait which can be mixed into a class/trait to define a sub state of a [[StateContainer]].
      */
    trait State extends Child {
        
        type InnerTransition = StateContainer.this.InnerTransition
        
        
        final override def onEvent(evt : Any) = events.run(evt)
        
        final override def onEnter(evt: Any) = enter.run(evt)
        
        final override def onResume(evt: Any) = if (historyType == HistoryType.NONE) enter.run(evt) else resume.run(evt)
        
        final override def onExit(evt: Any) = exit.run(evt)
        
	    final override def innerError(err: DispatchError): OuterTransition = Error(err)
	    
    }
    
    
    object State {
        /**
          * Construct a new sub state with the specified name and history type
          * 
	      * @param name The name of the new sub state
	      * @param historyType The history type of the new sub state
          */
        def apply(n: String, hType: HistoryType = HistoryType.NONE) = new State {
            lazy val name = n
            override val historyType = hType
            events := { _ => Delegate }
        }
    }
    
    
    /**
      * A trait which can be mixed into a class/trait to define a sub state machine of a [[StateContainer]].
      */
    trait StateMachine[T] extends StateContainer with Child {
        
        type ExitEvent = T
        
        
        final override def outerDelegate = StateContainer.this.Delegate
        
        
        final override def outerDone = StateContainer.this.Done
        
        
        override def onError(err: DispatchError) = StateContainer.this.Error(err)
        
    }
    
    
    object StateMachine {
        def apply[T](n: String, hType: HistoryType = HistoryType.NONE): StateMachine[T] = new StateMachine[T] {
            lazy val name = n
            override val historyType = hType
        }
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
      * The event handler of this state machine used to handle exit events
      */
    val terminate = new EventHandler[ExitEvent, OuterTransition]("    Terminating ", _ => outerError(ErrorMessage("No terminate handler defined in " + this)))
    
    
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
    final def deepState: com.totesoft.HierarchicalStateMachines.State = {
        currentState match {
            case Some(s : StateContainer) => s.deepState
            case Some(s)             => s
            case _                   => StateContainer.this
        }
    }
    
    
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
      * @param err The error
      * 
      * @return The resulting transition
      */
    def onError(err: DispatchError): OuterTransition
    
    
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
    private[this] def performTransition(evt: Any, node: com.totesoft.HierarchicalStateMachines.State, handleEvent: Any => InnerTransition): OuterTransition = {
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
                onError(ErrorMessage("Unhandled event " + evt + " in " + path))
            case e : Exception =>
                onError(UnhandledException(e))
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
                performTransition(evt, StateContainer.this, e => events.run(e))
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
    
    
    /**
      * Get an InnerTransition corresponding to the specified error
      * 
      * @param err The specified error
      */
    protected[HierarchicalStateMachines] final def innerError(err: DispatchError): InnerTransition = Error(err)
    
}

