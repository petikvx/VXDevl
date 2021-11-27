      PCinvestigator HookProtect v2.05 Readme File.


Copyright (C) ANNA Ltd., 1998. Zaporozhye, Ukraine.
All rights reserved.

IMPORTANT: 
1. Read license agreement first!
2. Before running the analysis, please, carefully read the
section "Heuristic Analysis" of this Readme.


           About PCinvestigator HookProtect

   HookProtect version 2.05 is an another powerful product 
of PCinvestigator  series. It  is specialized on detecting 
the programs that infringe the privacy and confidentiality  
on  personal   computers. There  are many various types of 
such  programs:  keyloggers,  interceptors, spies, Trojans 
and so on. Their main function  is monitoring of some kind  
of user's activity on a computer (for example, typing  the 
text,  running   the applications,  opening  the  windows, 
Internet activity, etc.). If your computer is connected to 
Internet/intranet or it is accessed  by many people, there 
is  a  probability  that  someone can install remotely  or 
locally  some  kind of logger or monitoring program on it. 
And you have no way to notice it because most of them  run  
invisibly.  All  existing  processes  and  modules viewers  
cannot   detect  the  professional  logger  (for  example, 
SKIn98).  But   using  HookProtect   you  can  immediately 
ascertain  whether your computer is "under observation" or 
it is not.
   The HookProtect main features are:
   - detection  of  loggers and monitoring programs loaded 
in memory;
   - discovery  of loggers and monitoring programs located 
on  hard  drive  but not loaded in memory by using special 
technology of heuristic analysis;
   - listing of all loaded modules and opened files;
   - monitoring  of  files activity (files and directories 
creations, deletions, renames, changes in attributes, size 
and time - all by user's choice) on selected directory;
   - detailed information in log file;
   - Windows 95/98 and Windows NT Workstation 4.0 support;
   - Y2K compliance.
   HookProtect  is  intended  for  persons responsible for 
computer   security,   for   system   administrators   and 
programmers,  systeminternals  specialists  and others who  
want   to   feel  safe  while  working  with  confidential 
information on their PC.

         Description of hooks in Windows 9x/NT
   
   A  hook  is  a  point  in  the  Microsoft(R) Windows(R)  
message-handling   mechanism   where  an  application  can 
install  a subroutine or a separate module to  monitor the 
message traffic  in the system  and process  certain types 
of messages. 
   Windows  contains  many  different  types of hooks. The  
hook  procedures  for some types of hooks can only monitor 
messages;  others  can  modify  messages  or   stop  their 
progress  though  the  hooks  chain,  preventing them from 
reaching  the  next  hook  procedure  or  the  destination 
window.  An  application can install and use some types of 
hooks  simultaneously.  Using of hooks by any  application 
opens  a  very  powerful and flexible way of monitoring of 
all operations performed by user on a  computer, all mouse 
movements and clicks, all keys pressed,all events occurred 
inside the system and so on.
   PCinvestigator  HookProtect  detects  12 basic types of 
hooks  that  are  conditionally  divided  into  2  groups: 
messages hooks and events hooks. 
   For more information see online help.

                  Heuristic Analysis
   
   Heuristic analysis allows  to found  all  modules  that  
contain specific functions used by  loggers and  different  
kinds of  monitoring programs. In fact, such functions are 
used  by  many other programs, so in result dialog box you  
will  see, for  example, some  modules  from  Microsoft(R) 
Office(R) or Adobe(R) Photoshop(R) packages (if, of course 
you  have  them installed)  and  some  DLLs   from  system 
directory. You can save the results of analyzing to a file 
and examine it for strange, outside DLLs. 
   Remember,  that  heuristic  methods don't give an exact 
result.  That's  why  it  is  impossible  to determine the
module where is the detected hook procedure located.  But,  
usually, the person responsible  for computer security has 
sufficient experience  to  discover in obtained results of 
analyzing  the  DLL  that  doesn't belong to system or any 
applications installed.
   For  each  suspicious  module  found  it is pointed the 
state  (loaded in memory or not)  and  type  (for example, 
Win32-based DLL). Of  course, any loaded in memory modules  
should be examined  first.  At  the  end of list the brief 
explanation  of  detected  hook  is  given,  if  any.  See 
"Description  of  Hooks  in   Windows   9x/NT"   for  more 
information.
   Like  any  other  heuristic  method,  this  analysis is 
a  lasting  process,  especially  on  a  slow machine. For 
example,  on  AMD-K6-266/16M/WinNT  it  takes  30 minutes, 
P-100/32M RAM/1,2G HDD/Win95 -  43  minutes.  For   better  
performance  it  is  recommended  to  close  other running 
applications.
   You  have  no  reason  to  worry if there is shell hook 
detected, because it is usually set by  the system itself. 
If  running  on  Win98, it can be computer-based  training  
(CBT) hook detected. But  if you see the string "DETECTED" 
opposite  some  other  type  of  hook,  exit  all  running 
applications.  If  it  doesn't  help,  there  is  a  great 
probability  that some logger is running on your PC. Click  
the  button  Analyse  and  examine  the list of suspicious 
modules  found. There is always HPROT32.DLL loaded because 
it is the HookProtect module. You can also get the list of 
all loaded modules and opened files in a tab "Modules".
   NOTE: It is recommended to run  Heuristic  Analysis  on 
foreground.

       Scanning for Loaded Modules and Opened Files 
   
   All loaded modules and opened files found are listed in 
the tab "Modules". In the upper  left corner of the window 
the  total  number of files is shown. Usually, it is above 
100.  During  the working session this list doesn't update
automatically. So, you  should  press  the  button "Update 
Modules List" to rescan and update the list. 
   Having the list  of loaded modules and opened files you 
can analyse what  applications  are running and what files 
are in use. By saving  this  list to a file  on  a "naked" 
system  (when there are no other applications running) and 
then  comparing  the  lists,  you  can  reveal  the module 
which  belongs  to  some  invisible  logger  or monitoring 
program, if any.

               Monitoring of Files Activity
   
   Monitoring  of  files  activity  gives  you an easy and 
flexible  way to detect any  changes  in  file  system  by  
your  choice,  occurred  on watched  object  (directory or 
directory tree). 
   First, you select  the  directory or directory tree you 
want  to  monitor. Second, check  the  log file name. Then 
you can choose the desired monitoring options you  want to  
use:  files  creations,  renames,  deletions,  changes  in 
subdirectory structure  (has  an effect if you selected to 
monitor directory tree), changes in attributes, files size 
and  write  time.  One  restriction, if you  selected  the  
directory  containing  Windows  swap  file  as the watched 
object,  you   should   choose   "Filename  changes"   and 
"Directory creations and deletions" as monitoring options, 
otherwise your log file will continuously increase.

                        Feedback 

   Your feedback will  help  us  improve  our  software to 
better meet your needs. Please  let  us  know  if you have 
problems or suggestions for future enhancements.
   To contact  us via the Internet, send your comments to:
              pcihprot@anna.zaporizhzhe.ua 
   Thanks in advance for your feedback.

Information about other our products see on:

http://www.geocities.com/SiliconValley/Hills/8839/index.html
or
http://annaltd.webjump.com/index.html

                Thank you for choosing 
              PCinvestigator HookProtect.

            ANNA Ltd., Zaporozhye, Ukraine.

