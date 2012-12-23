package com.totesoft.HierarchicalStateMachines

class Orchestrator(override val name: String) extends StateMachine with PartialEventHandler {
    
    type SimpleActivity = this.State
    
    
    type Activity[T] = this.StateMachine[T]
    
}

