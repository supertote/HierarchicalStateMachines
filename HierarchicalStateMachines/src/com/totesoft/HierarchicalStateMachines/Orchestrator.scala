package com.totesoft.HierarchicalStateMachines

class Orchestrator(override val name: String) extends StateMachine(name) with PartialEventHandler {
    
    type SimpleChildActivity = this.ChildState
    
    type SimpleActivity = this.State
    
    
    type ChildActivity = this.ChildStateMachine
    
    type Activity[T] = this.StateMachine[T]
  
}

