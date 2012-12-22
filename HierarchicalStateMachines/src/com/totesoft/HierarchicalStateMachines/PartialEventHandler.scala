package com.totesoft.HierarchicalStateMachines

trait PartialEventHandler {
    this : StateContainer =>
    
    private[this] var handlers: List[PartialFunction[Any, InnerTransition]] = List()
    
    
    def +(handler: PartialFunction[Any, InnerTransition]) = handlers = handler :: handlers
    
    
    def -(handler: PartialFunction[Any, InnerTransition]) = handlers = handlers filterNot { _ == handler }
    
    
    events { e => handlers.find(_.isDefinedAt(e)) match {
        case Some(handler) => handler(e)
        case None => Error("Unhandled Event " + e)
    }} freeze
    
}

