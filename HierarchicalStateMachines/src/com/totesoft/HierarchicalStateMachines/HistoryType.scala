package com.totesoft.HierarchicalStateMachines

sealed trait HistoryType

/**
  * Defines the various types of history supported by state machines and states:
  * 
  * - NONE: Resume is not supported; doing a Resume is equivalent to doing an Enter
  * 
  * - SHALLOW: Resume will restore the saved sub state and Enter it
  * 
  * - DEEP: Resume will restore the saved sub state and Resume it
  * 
  * Note: for simple states SHALLOW and DEEP are equivalent since simple states do not have sub states
  */
object HistoryType {
    /**
      * Resume is not supported and will be replaced by Enter
      */
    case object NONE extends HistoryType
    /**
      * Resume will restore the saved sub state and Enter it
      */
    case object SHALLOW extends HistoryType
    /**
      * Resume will restore the saved sub state and Resume it
      */
    case object DEEP extends HistoryType
}

