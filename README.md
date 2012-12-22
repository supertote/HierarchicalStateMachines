## Introduction
In traditional [Finite State Machines](http://en.wikipedia.org/wiki/Finite-state_machine) you have only one level of nesting: a State Machine contains States.

In [Hierarchical State Machines](http://www.barrgroup.com/Embedded-Systems/How-To/Introduction-Hierarchical-State-Machines) a State Machine can also be one of the sub-states of a containing State Machine. The problem of implementing a complex State Machine can thus be simplified by:
* implementing a set of simpler sub State Machines;
* wiring together these sub State Machines in a containing State Machine (or an arbitrarily deep hierarchy of sub State Machines).

