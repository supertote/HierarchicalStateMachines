package test

import com.totesoft.HierarchicalStateMachines.{StateMachine, FullConsoleLogging}


sealed trait SubSMExit
case object COMPLETELY extends SubSMExit
case object TO_STATE_1 extends SubSMExit
case object TO_STATE_2 extends SubSMExit

object Test2 {

//    class TestSM extends RootStateMachine("TestSM") with FullConsoleLogging
    
    
    val rootSm: StateMachine = new { override val name = "TestSM" } with StateMachine with FullConsoleLogging {
	    
	    val State1: State = State("State1")
	    State1.events := { _ match {
	        case 2 => MoveTo(State2)
	        case 3 => MoveTo(State3)
	        case _ => Done
	    }}
	
	    
	    val State2: State = State("State2")
	    State2.events := { _ match {
	        case 1 => MoveTo(State1)
	        case 3 => MoveTo(State3)
	        case _ => Done
	    }}
	    
	    
	    val State3: StateMachine[SubSMExit] = new StateMachine[SubSMExit] {

	        override val name = "State3"
	            
		    val SubState1 = State("SubState1")
		
		    val SubState2 = State("SubState2")
		    
	        
		    events := { _ match {
		        case 1 => MoveTo(SubState1)
		        case 2 => MoveTo(SubState2)
		        case 3 => Terminate(TO_STATE_1)
		        case 4 => Terminate(TO_STATE_2)
		        case 5 => Terminate(COMPLETELY)
		        case _ => Done
		    }}
	        
	    }
        
	    State3.terminate := { _ match {
            case COMPLETELY => Terminate()
            case TO_STATE_1 => MoveTo(State1)
            case TO_STATE_2 => MoveTo(State2)
        }}
	    
	    
	    events := { _ match {
	        case 1 => MoveTo(State1)
	        case 2 => MoveTo(State2)
	        case 3 => MoveTo(State3)
	        case _ => Done
	    }}
	    
    }
    
    
	def main(args: Array[String]): Unit = {
	    rootSm.fire(1)
	    rootSm.fire(3)
	    rootSm.fire(1)
	    rootSm.fire(5)
	}
	
}
