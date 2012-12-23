package com.totesoft.HierarchicalStateMachines


/**
  * The Node trait defines the common base of all elements composing a state machine hierarchy:
  * i.e. [[StateMachine]]s, [[StateContainer.State]]s and [[StateContainer.StateMachine]]s
  */
trait State {
    
    /**
      * Encapsulates method(s) which will be triggered at convenient points within states and/or
      * state machines.
      * 
      * A default method must be provided during instantiation.
      * 
      * The registered methods can be overriden/augmented by user code as follows:
      * 
      * {{{
      * val handler: Handler[IN, OUT] = ...
      * <...>
      * // Add a handler which will be called before the already registered handlers
      * handler << { i => { <do something with i, e.g. tracing> } }
      * // Add a handler which will replace the already registered handler
      * handler := { i => { <do something with i>; <some result> } }
      * // Add a handler which will be called after the already registered handlers
      * handler >> { (i, o) => { <do something with i and o, i.e. tracing>; o } }
      * }}}
      * 
      * Note that overriding/augmenting the methods can fail if it has been previously disabled with
      * a call to method '''freeze'''. In this case, the behavior depends on the [[StateMachine]]
      * of which this instance is part of:
      * - by default the overriding/augmentation attempt will be simply ignored;
      * - a sub-class of [[StateMachine]] can decide to change that (e.g. throwing an exception)
      * by overriding method '''cantOverrideFrozenHandler''' method
      *
      * @constructor Construct a new instance which encapsulates the specified method
      * 
      * @param handler The initial value of the encapsulated method
      * 
      * @tparam I The type of the method's input parameter
      * @tparam O The type of the method's result
      */
    sealed abstract class Handler[I, O](default: I => O) {
        
        import scala.collection.mutable.ListBuffer
        
        private[this] var frozen: Boolean       = false
        private[this] var before: I => Unit     = { i => }
        private[this] var handler: I => O       = default
        private[this] var after: (I, O) => Unit = { (i, o) => }
        
        /**
          * Prevent further overriding of the encapsulated method
          */
        def freeze: Unit = frozen = true
        
        
        /**
          * Trigger the execution of the encapsulated methods, or of the default one if none
          * has been registered
          * 
          * @param i The input parameter
          * 
          * @return The result of the encapsulated last handler call
           */
        protected def run(i: I): O = {
            before(i)
            val result = handler(i)
            after(i, result)
            result
        }
        
        
        /**
          * Add a new handler which will be triggered before all previously registered handlers
          * 
          * @param h The new handler to be prepended
          * 
          * @return The current instance so that a call to '''freeze''' can be chained
          */
        def <<(h: I => Unit): Handler[I, O] = {
            if (frozen) root.cantOverrideFrozenHandler("")
            else        before = { i => h(i); before(i) }
            
            this
        }
        
        
        /**
          * Override the encapsulated method with the given method
          * 
          * @param h The new handler which will replace the existing one
          * 
          * @return The current instance so that a call to '''freeze''' can be chained
          */
        def :=(h: I => O): Handler[I, O] = {
            if (frozen) root.cantOverrideFrozenHandler("")
            else        handler = h
            
            this
        }
        
        
        /**
          * Add a new handler which will be triggered after all previously registered handlers
          * 
          * @param h The new handler to be appended
          * 
          * @return The current instance so that a call to '''freeze''' can be chained
          */
        def >>(h: (I, O) => Unit): Handler[I, O] = {
            if (frozen) root.cantOverrideFrozenHandler("")
            else        after = { (i, o) => after(i, o); h(i, o) }
            
            this
        }
        
    }
    
    
    /**
      * A specialized version of [[Handler]] for encapsulation methods triggered on input events
      * or exit events.
      * 
      * @constructor Construct a new instance which, if not override, returns the specified default
      * value. The encapsulated method will also prints a trace starting with the specified message
      * if it is not null
      * 
      * @param message A prefix which will be printed along with the input parameter and the current
      * [[StateNode]] each time the method will be triggered.
      * @param handler The initial value of the encapsulated method
      * 
      * @tparam I The type of the method's sole input parameter
      * @tparam O The type of the method's result
      */
    sealed class EventHandler[I, O](message: String, handler: I => O) extends Handler[I, O](handler){
        
        /**
          * @inheritdoc
          * 
          * Traces will be printed before and after the execution of the encapsulated method if the
          * [[StateMachine]] containing this instance is configured to do so
          */
        override def run(i: I) = {
            root.eventLog(message + i + " in " + State.this)
            val result = super.run(i)
            root.eventLog("        => " + result)
            result
        }
        
    }
    
    
    /**
      * A specialized version of [[Handler]] for encapsulation methods triggered when state changes occur.
      * When triggered, the method will be passed the event which triggered the change.
      * 
      * @constructor Construct a new instance which, if not override, does nothing. Traces will be printed
      * before triggering the encapsulated method if the [[StateMachine]] containing this instance is
      * configured to do so.
      * 
      * @param kind A value representing the type of lifecycle event, for tracing
      */
    sealed class LifecycleHandler(kind: String) extends Handler[Any, Unit](_ => {}){
        
        /**
          * @inheritdoc
          * 
          * Traces will be printed before the encapsulated method if the [[StateMachine]] containing
          * this instance is configured to do so
          */
        override def run(e: Any) = {
            root.lifecycleLog("    " + kind + " " + State.this + " on " + e)
            super.run(e)
        }
        
    }
    
    
    /**
      * Represent the type of transition that is computed when firing an event into this instance
      * from its container
      */
    type OuterTransition
    
    
    /**
      * Represent the type of transition that is computed when firing an event into this instance
      * internally
      */
    type InnerTransition
    
    
    /**
      * The name of this instance
      */
    val name: String
    
    /**
      * Defines how this instance supports resumption
      */
    val historyType: HistoryType = HistoryType.NONE
    
    /**
      * A method triggered when this instance is entered
      */
    val enter = new LifecycleHandler("Entering")
    
    /**
      * A method triggered when this instance is resumed
      */
    val resume = new LifecycleHandler("Resuming")
    
    /**
      * A method triggered when this instance is exited
      */
    val exit = new LifecycleHandler("Exiting")
    
    /**
      * The event handler of this state machine used to handle input events
      */
    val events = new EventHandler[Any, InnerTransition]("    Handling ", _ => innerError("No event handler defined in " + State.this))
    
    /**
      * The path of this instance starting at it's '''root'''
      */
    final lazy val path: String =
        container match {
            case Some(c) => c.path + "." + name
            case None    => name
        }
    
    
    /**
      * The top level [[StateMachine]] of which this instance is a part
      */
    final lazy val root: StateMachine =
        container match {
            case Some(c) => c.root
            case None    => State.this.asInstanceOf[StateMachine]
        }
    
    
    /**
      * The [[StateContainer]] of which this instance is a a child
      */
    def container: Option[StateContainer]
    
    
    /**
      * Trigger the execution of the '''enter''' method of this instance
      * 
      * @param evt The event to pass to the enter method
      */
    protected[HierarchicalStateMachines] def onEnter(evt: Any): Unit
    
    /**
      * Trigger the execution of the '''enter''' method of this instance
      * 
      * @param evt The event to pass to the enter method
      */
    protected[HierarchicalStateMachines] def onResume(evt: Any): Unit
    
    /**
      * Trigger the execution of the '''exit''' method of this instance
      * 
      * @param evt The event to pass to the enter method
      */
    protected[HierarchicalStateMachines] def onExit(evt: Any): Unit
    
    /**
      * Trigger the execution of the '''events''' method of this instance
      * 
      * @param evt The event to pass to the enter method
      */
    protected[HierarchicalStateMachines] def onEvent(evt : Any): OuterTransition
    
    
    /**
      * Get an InnerTransition corresponding to the specified error
      * 
      * @param msg: the error message
      */
    protected[HierarchicalStateMachines] def innerError(msg: String): InnerTransition
    
    
    /**
      * Get an OuterTransition corresponding to the specified error
      * 
      * @param msg: the error message
      */
    protected[HierarchicalStateMachines] def outerError(msg: String): OuterTransition
    
    
    override def toString = path
    
}

