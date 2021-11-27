Outlook(bat).Mirc(htm).pirch(htm).Bulbasaur.Worm
By [K]Alamar
_______________________________________________________________________
This bath, vbs or wathever you want to call him worm works like this:

The bat writes a vbs in the bat's folder calles "Bulbasaur1.vbs" and
then runs it using wscript. The bat also copy himself to windows.
This vbs ripps the full vbs worm code from the bat, and write it to
%windir%\bulbasaur.vbs, then it's runned.
The last vbs adds himself to startup, send a message whit the bat
attached to all the address list, and searchs for mirc.ini and
events.ini in all the drives of the computer, if they're found,
infect that folder whit a simnple script for mirc or pirch.
The irc worm send a file called bulbasaur.htm that contains the
bat, and when the html is opened it creates and runs the bat.

So, the bat do almost nothing, just contain the code.

The main vbs (second one) contains a routine that makes bulbasaur.bat
bulbasaur.vbs and bulbasaur.html imposible to delete, cause if the worm
detects that they have been deleted they are created again!.
The registry key that makes the worm run at startup is also writted
again if it's deleted!.
To do this, the scrip keeps in memory in an infinite do-loop.

You can use just the vbs, cause it reads the bat all the time, so
to work, the vbs needs the batch in the system, that's why the batch is
always sended in one or another way.
_______________________________________________________________________
I think that it's a good worm.
Thanx to Duke/smf for his help whit the batch file.
_______________________________________________________________________
[K]Alamar