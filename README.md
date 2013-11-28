llvm-dse
========

A pass that aims to remove dead stores on LLVM using cloning techniques.

How to use
----------

1. Clone the project on the folder lib/Transforms/ of your llvm installation
2. You can compile it either using Makefile or cmake
3. Load the module on you opt command line, and specify the pass name, for example: `opt -load /path/to/your/module/LLVMDSE.so -dead-store-elimination module.ll`

In order to acquire more precision, we recommend using Tristan's llvm alias analysis from [here](https://github.com/TristanSchmelcher/llvm-andersen).

