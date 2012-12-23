package com.totesoft.HierarchicalStateMachines

sealed trait DispatchError
case class UnhandledException(e: Exception) extends DispatchError
case class ErrorMessage(m: String) extends DispatchError