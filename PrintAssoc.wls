#!/usr/bin/env wolframscript

PacletDirectoryLoad[Directory[]]; 
Get["KirillBelov`Internal`Console`"]; 

Echo[#, "Print Assoc Example"]& @ 
<|"DateObject" -> Now, "BoolValues" -> {True, False}, "Array" -> {1, 2, 3, 4, 5}, "String" -> "Hello, World!"|>;