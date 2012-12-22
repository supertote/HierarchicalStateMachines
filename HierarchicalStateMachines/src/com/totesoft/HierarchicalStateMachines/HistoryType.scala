package com.totesoft.HierarchicalStateMachines

sealed trait HistoryType

object HistoryType {
    case object NONE extends HistoryType
    case object SHALLOW extends HistoryType
    case object DEEP extends HistoryType
}

