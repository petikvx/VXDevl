Rootkit Detector v0.6.2
======================


Haxorcitos Rootkit Detector
Programmed by aT4r@3wdesign.es
Homepage: http://www.3WDesign.es/security Copyright (c) 2004
	  http://www.haxorcitos.com



Usage:
-----

rkdetector.exe [params]

	-v 		- Prints Verbose Information.
	-m filename	- Shows md5 checksum for filename


Description:
------------

RKDetector is adiagnostic tool that provides information about Hidden proccess and Services Hooked by an NT rootkit such as Hacker Defender (rootkit.host.sk)
After hidden Handles are identified, rootkit Detector will Try to kill those hidden tasks and reScan the service database in order 
to detect hidden services Installed by Hackers and hidden regkeys (Run, Runonce...).
Another Feature is that rootkit detector have their Own internal MD5 Database with signatures about known rootkits, exploits and hacking
tools that are used to identify malware running in your system.

An extended Report is generated with information about All Tasks and Services all with their own md5 checksum.

If you find any other unknown hacking tools in your system, please send an email with your full report to Security at4r@3wdesign.es
and provide as more information as you can and our security team will include those signatures for next Releases. 
Examples: 
Found tool XXXXXXX.exe with md5 signature FFFFFFFFFFFFFFFFFFFFFF identified as "http Scanner"

if you are not able to identify what the hell is those tool for, please attach extra files to the email.


TODO:
-----
Lots of things.. 



Contact Information:
--------------------

at4r@3wdesign.es		- Submit bugs / Reports
comercial@3wdesign.es		- Security audits 





------------------------------------------------------------
            3W Design Security (c) 20004
------------------------------------------------------------