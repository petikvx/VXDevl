
LiveKd
Copyright (C) 2001-2003 Mark Russinovich 
Sysinternals - www.sysinternals.com

LiveKd
------
LiveKd, a utility I wrote for the CD included with Inside Windows 
2000, 3rd Edition, is now freely available. LiveKd allows you 
to run the Kd and Windbg Microsoft kernel debuggers, which 
are part of the Debugging Tools for Windows package, locally 
on a live system. While the latest versions of Windbg and Kd 
have a similar capability on Windows XP and Server 2003, 
LiveKd works on NT 4 through Server 2003 and enables more 
functionality, such as viewing thread stacks with the 
!thread command, than debugger's own live kernel debugging 
facility.

Installation 
------------
First download and install the Debugging Tools for Windows 
package from Microsoft's web site: 

http://www.microsoft.com/whdc/ddk/debugging/

If you install the tools to their default directory of 
\Program Files\Microsoft\Debugging Tools for Windows, you 
can run LiveKd from any directory. If you haven't installed 
symbols for the system on which you run LiveKd, LiveKd will 
ask if you want it to automatically configure the system to 
use Microsoft's symbol server (see the Debugging Tools for 
Windows documentation for information on symbol files and the Microsoft symbol server). 
 


Terms of Use
------------

This software is provided "as is", without any guarantee made
as to its suitability or fitness for any particular use. It may
contain bugs, so use of this tool is at your own risk. We take
no responsilbity for any damage that may unintentionally be caused
through its use.

You may not distribute LiveKd in any form without express written
permission of Mark Russinovich. 

Licensing
---------

If you are interested in redistributing LiveKd, either in original
or modified form, or wish to use LiveKd source code in a product,
please send e-mail to licensing@sysinternals.com with details.

Reporting Problems
------------------

If you encounter problems, please visit http://www.sysinternals.com
and download the latest version to see if the issue has been resolved.
If not, please send a bug report to:

	mark@sysinternals.com

