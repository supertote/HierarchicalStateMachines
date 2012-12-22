package test

import com.totesoft.HierarchicalStateMachines.{StateMachine, ConsoleLogging, LifecycleLogging}

class TestSM(override val name: String) extends StateMachine(name) with ConsoleLogging with LifecycleLogging {

    sealed trait SubSMExit
    case object COMPLETELY extends SubSMExit
    case object TO_STATE_1 extends SubSMExit
    case object TO_STATE_2 extends SubSMExit
    
    
    val State1: ChildState = new State("State1") {
        events := { _ match {
	        case 2 => MoveTo(State2)
	        case 3 => MoveTo(State3)
	        case _ => Done
	    }}
    }

    
    val State2: ChildState = new State("State2") {
        events := { _ match {
	        case 1 => MoveTo(State1)
	        case 3 => MoveTo(State3)
	        case _ => Done
	    }}
    }
    
    
    val State3: ChildStateMachine = new StateMachine[SubSMExit]("State3") {
        
	    val SubState1: ChildState = new State("State1")
	
	    
	    val SubState2: ChildState = new State("State2")
	    
        
	    events := { _ match {
	        case 1 => MoveTo(SubState1)
	        case 2 => MoveTo(SubState2)
	        case 3 => Terminate(TO_STATE_1)
	        case 4 => Terminate(TO_STATE_2)
	        case 5 => Terminate(COMPLETELY)
	        case _ => Done
	    }}
	    
	    terminate := { _ match {
            case COMPLETELY => TestSM.this.Terminate()
            case TO_STATE_1 => TestSM.this.MoveTo(State1)
            case TO_STATE_2 => TestSM.this.MoveTo(State2)
        }}
    }
    
    
    events := { _ match {
        case 1 => MoveTo(State1)
        case 2 => MoveTo(State2)
        case 3 => MoveTo(State3)
        case _ => Done
    }}
    
}


object Test {

	def main(args: Array[String]): Unit = {
	    val sm = new TestSM("TestSM")
	    
	    sm.fire(1)
	    sm.fire(3)
	    sm.fire(1)
	    sm.fire(5)
	}
}
