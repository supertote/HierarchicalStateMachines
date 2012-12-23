package com.totesoft.HierarchicalStateMachines

/**
  * Define the different kind of errors which can occur during event dispatching
  */
sealed trait DispatchError
/**
  * Encapsulates an exception caught during event dispatching
  */
case class UnhandledException(e: Exception) extends DispatchError
/**
  * Encapsulates an error message generated during event dispatching
  */
case class ErrorMessage(m: String) extends DispatchError