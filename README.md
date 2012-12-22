HierarchicalStateMachines
=========================

An implementation in Scala of Hierarchical State Machines.
<h2>Introduction</h2>
In traditional [Finite State Machines](http://en.wikipedia.org/wiki/Finite-state_machine) you have only one level of nesting: a State Machine contains States.
In [Hierarchical State Machines](http://www.barrgroup.com/Embedded-Systems/How-To/Introduction-Hierarchical-State-Machines) a State Machine can also be one of the sub-states of a State Machine, allowing to divide the problem of writing a complex State Machine into writing a set of simpler sub State Machines wired together in a containing State Machine (or an arbitrarily deep hierarchy of sub State Machines).
