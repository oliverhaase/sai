# sai
Scala Abstract Interpreter

## Aim 
This project is intended to implement an abstract interpreter of Java Bytecode in the Scala language. When the interpreter abstractly 
executes a method, it only computes its effect on the object heap, i.e. newly created objects as well as new references between preexisting objects. Because the interpreter doesn't do any arithmetic computation, it can only approximate the effect on the object heap; the approximation computes the maximum effect in terms of established references; in the context of static code analysis - which is the main purpose of the interpreter - the maximum effect corresponds to a conservative approximation.

## Basic Design
The interpreter works on the Java bytecode in the BCEL Abstract Syntac Tree (AST) representation. Basically, each class, method and opcode instruction in BCEL foprmat is wrapped by a Scala object that extends the BCEL counterparts by additional attributes that represent the abstract interpretation of the code. For example, the result of the abstract interpretation of a method is computed in the summary attribute of the method. This attribute contains all objects as well as all inter-object references that are created within the method.The method summary is computed using the output state of the method's exit point (a virtual, single exit instruction that all return instructions jump to). The output state of an instruction, in turn, is calculated from the instruction's input state and the instruction's semantics, and so on. 


## Reading list:

* Jave Virtual Machine Specification https://docs.oracle.com/javase/specs/jvms/se8/jvms8.pdf
* Choi, J.-D. et al: Java Escape Analysis http://doi.acm.org/10.1145/320384.320386
* Apache BCEL https://commons.apache.org/proper/commons-bcel/

