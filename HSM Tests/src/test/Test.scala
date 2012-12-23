package test

import com.totesoft.HierarchicalStateMachines.{StateMachine, FullConsoleLogging}

object Test {

    sealed trait SubSMExit
    case object COMPLETELY extends SubSMExit
    case object TO_STATE_1 extends SubSMExit
    case object TO_STATE_2 extends SubSMExit
    

    val root = new { override val name = "TestSM" } with StateMachine with FullConsoleLogging
    root.events := { _ match {
        case 1 => root.MoveTo(State1)
        case 2 => root.MoveTo(State2)
        case 3 => root.MoveTo(State3)
        case _ => root.Done
    }}
    
    
    val State1 = root.State("State1")

    val State2 = root.State("State2")
    
    val State3 = root.StateMachine[SubSMExit]("State3")
    State3.events := { _ match {
        case 1 => State3.MoveTo(SubState1)
        case 2 => State3.MoveTo(SubState2)
        case 3 => State3.Terminate(TO_STATE_1)
        case 4 => State3.Terminate(TO_STATE_2)
        case 5 => State3.Terminate(COMPLETELY)
        case _ => State3.Done
    }}
    State3.terminate := { _ match {
        case COMPLETELY => root.Terminate()
        case TO_STATE_1 => root.MoveTo(State1)
        case TO_STATE_2 => root.MoveTo(State2)
    }}
    
    val SubState1 = State3.State("SubState1")

    val SubState2 = State3.State("SubState2")


	def main(args: Array[String]): Unit = {
	    root.fire(1)
	    root.fire(3)
	    root.fire(1)
	    root.fire(5)
	}
}
