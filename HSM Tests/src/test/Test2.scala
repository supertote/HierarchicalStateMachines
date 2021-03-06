package test

import com.totesoft.HierarchicalStateMachines.{StateMachine, FullConsoleLogging}


sealed trait SubSMExit
case object COMPLETELY extends SubSMExit
case object TO_STATE_1 extends SubSMExit
case object TO_STATE_2 extends SubSMExit

object Test2 {

    val rootSm: StateMachine = new StateMachine with FullConsoleLogging {
	    lazy val name = "TestSM"
	    
	    val State1 = State("State1")
	
	    val State2 = State("State2")
	    
	    val State3 = new StateMachine[SubSMExit] {

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
