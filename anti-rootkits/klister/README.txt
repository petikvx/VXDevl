
KLISTER v 0.4

About 
------

klister is a simple set of utilities for Windows 2000, designed to
read the internal kernel data structures, in order to get reliable
information about the system state, which can be compromised by some
smart rootkits.

It consists of a kernel module and a simple, command line programs,
which provides the user's interface.

Process listing
---------------- 

In current version only process listing has been implemented. Klister
is using 3 internal dispatcher data structures in order to find
running processes:

- KiDispatcherReadyListHead,
- KiWaitInListHead_addr,
- KiWaitOutListHead_addr.

Unfortunately addresses of these structures are not exported by the
kernel, so you will have to use debug symbols (which can be downloaded
from Microsoft) to get their addresses.

SDT listing 
------------ 

sdt.exe utility can be used to obtain the real address of the Service
Table which is used by all threads running in the system (by examining
pSDT filed in each KTHREAD structure). It also dumps its contents, so
you can catch all simple rootkit which hooks that table.

IDT listing 
------------ 

idt.exe just dumps the contents of IDT table (pointed by IDTR
register).

Usage
------

You will have to use 3rd party utility program to load klister's
kernel module (kmodule.sys) into kernel. You can use Schreiber's
program w2k_load for example, which is attached in the file
w2k_internals.zip

w2k_load kmodule.sys

This is a proof-of-concept code, and no warranty is given.
Use at your own risk.

Currently only Windows 2000 is supported!

Credits
--------

-> fuzzen_op, for writing fu rootkit, which inspired me
   to develope this tool ;)
-> Greg, for pioneer work on windows rootkits and for rootkit.com.
-> Sven Schreiber, for really good book about windows internals 
   (and useful tools:))
-> Microsoft, for writing Windows ;)

Author
-------

Joanna Rutkowska
joanna at mailsnare dot net


