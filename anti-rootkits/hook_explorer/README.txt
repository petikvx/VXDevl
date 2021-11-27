
Author:     David Zimmer <david@idefense.com, dzzie@yahoo.com>

CopyRight:  Copyright: 2005 iDefense a Verisign Company 
            GPL Olly.dll is Copyright (C) 2001 Oleh Yuschuk - http://ollydbg.de

License: GNU General Public License

         This program is free software; you can redistribute it 
         and/or modify it under the terms of the GNU General 
         Public License as published by the Free Software Foundation;
         either version 2 of the License, or (at your option) any 
         later version.

         This program is distributed in the hope that it will 
         be useful, but WITHOUT ANY WARRANTY; without even the implied
         warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
         PURPOSE. See the GNU General Public License for more details.

         You should have received a copy of the GNU General Public 
         License along with this program; if not, write to the Free 
         Software Foundation, Inc., 59 Temple Place, Suite 330, Boston,
         MA 02111-1307 USA


Dependancies
-------------------------------------------------
Hook Explorer is written in VB6. Your system will
need the VB6 runtimes and the Microsoft Common
Controls OCX (mscomctl.ocx)


Installation
-------------------------------------------------
Unzip and run. If you get an error that msvbvm60.dll
is missing, you will have to install the vb6 runtimes.

If mscomctl.ocx is missing you will need to download
and register the control. Google is your friend.


Hook Explorer
-------------------------------------------------

This is a small application designed to scan a single
process looking for IAT or detours style hooks.

HookExplorer gives the user several scanning and
display options. 

When first run, HookExplorer will enumerate all of
the loaded dlls in the process and scan their import
tables for hijacked function pointers in the import
address table (IAT). The first instruction for each
function pointer is then disassembled and examined
to try to detect standard detours style hooks which
may be in place.

If the "scan all exports" checkbox was selected, then
HookExplorer will also scan every function found in 
the images export table for detours style hooks. For
dynamically loaded dlls, this may be the only test we
can perform on them. Note that this option can take some
extra time to perform, and cannot be added to an existing
scan on the fly. Once you check this option the current 
scan will not be updated and you will have to rescan the
target process.

HookExplorer also supports 4 data display modes to help
you examine the data. These options can be applied on
the fly and will simply re-display the collected scan
data. 

Internally hooks are stored in 3 collections. The first
collection saves references to all functions, the second
only cross module hooks, and finally the third which applies
a user defined filter list to the results.

The display options that relate to these collections are
termed:

1) Standard        
      - displays cross module and same module hooks

2) Use Ignore List  
      - displays filtered cross module hooks only

3) Hide hooks from same module 
      - displays cross module hooks (no filter)

4) Show All  
      - same as standard mode except also displays all entries
        per dll, hooked or not

These options are represented by radio buttons and can
be applied on the fly once a scan has finished.

The IgnoreList is loaded from the file IgnoreList.txt found
in the applications home directory. This file lists the dlls
which you trust and do not want displayed in the results when
using the IgnoreList display option. It is recommended to use
the full dll path to your trusted dlls in this file.

The ignorelist can be edited in notepad and can be updated on
the fly to an existing scan. The edit button on the main interface
will launch notepad on the file allowing you to edit it. Once you
have made your updates, save your changes and hit the reload button
which will reload the file and apply the filter to the current
display results.





