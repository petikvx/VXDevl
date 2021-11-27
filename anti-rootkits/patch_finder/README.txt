Patch Finder 2.12
==================

Overview
---------
Patchfinder (PF) is a sophisticated diagnostic utility designed to
detected system libraries and kernel compromises. Its primary use is
to check if the given machine has been attacked with some modern
rootkits, i.e. programs which tries to hide attacker's activity on the
hacked system, by cheating about the list of active processes, files
on filesystem, running services, registry contents, etc...

New release (2.x) of PF is the first version which is intended to be
not only a proof-of-concept code for developers, but also to be useful
tool for administrators. To make a proper use of the PF, every user
should read the attached PDF paper.

With this tool you should be able to detect even the newest versions
of such  rootkits like: Hacker Defender, APX, Vaniquish, He4Hook, and
many more...

Quick Start:
---------------
C:\pf2>pfInstall.exe --install c:\pf2
<restart the machine>

Now you can use pfAgent (only console version is available now):

C:\pf2>pfAgentConsole.exe

This will run the tests on your machine (taking few seconds, depending
on the CPU speed), display a table with results, and also saves the
results to your system log. You can also specify remote computer name
(e.g. \\KITCHEN) as the first parameter to check the remote system,
provided PF has been installed on it.

PF at least should be run just before every system reboot. This is
crucial for the safety usage. See PDF paper for explanation. You can
add pfAgentConsole.exe to shutdown scripts in Group Policy snap-in:
Group Policy\Local Policy\Computer Configuration\Windows
Settings\Scripts\Shutdown.

It is also a good idea to make use of windows schedule facilities to
run the pfAgentConsole.exe for e.g. every hour.

Supported platforms
-----------------------------
Only Windows 2000. Except support for XP/2003 in future versions.

Download
---------
Get the latest version of patchfinder from http://rootkit.com.

Author
---------
Joanna Rutkowska
joanna at mailsnare dot net

2002-2004.

-----BEGIN PGP PUBLIC KEY BLOCK-----
Version: GnuPG v1.2.1 (GNU/Linux)

mQGiBEANuecRBADy/SxMGtz6wBr1MR9QLoefFAO4DDm910lS2eeJ7jelilik5/b2
zaTQiqmeeVdGTUSWhQ8WyFPAz6ms6A4bJDqPR5gjrvPI0UXca69B69eEVt6nhwaH
Df2duRNAr0rMbn3Q5pqRgncemVUKI6DYq+1NZAgYwV7JdZEYiGUu5Qx/SwCg/Bx2
J4Ts1J50KWgWLMxvBH4lVS8EAJbVvaoRr1nSVKa34PeUBtNujfx6tOFtbU5f79Km
37OeQ1VPOnnSppKuD6dnHBNJnR34+fr2pxphjqVeWNno66qJM9WpGi9D2IODndpi
EqlOSSL2HR5Rn83yheYEMN2qdNv020jdO6CaCBTPFYOW5byXp9mJ6MPAhONytkB5
DZXVBADVjLI3tqSmdIk5ujQ8xVSLLomqDHyrPWjgIY1bCRqBMKg3KOGkmVfU8Nc0
hfYH67PYsUY8VsGYyC+g8RX72ndzp4Jjkb8WAQmh09k0GJlOlO5O+0L6hCXwgmmA
bwwX8VlWaOIeHedT2dF3CsfRxh6xslQNPE4+qEMj3oeup9uTFrQnSm9hbm5hIFJ1
dGtvd3NrYSA8am9hbm5hQG1haWxzbmFyZS5uZXQ+iFkEExECABkFAkANuecECwcD
AgMVAgMDFgIBAh4BAheAAAoJEDkXZKLXxFvOJHsAn1/Co6aZE09cn7J3A3lhsqtd
guxAAKCGUYzcyq+0/nwTboV6YALf262rrLkCDQRADbpVEAgArQ3mDsv9l5Sks037
yxq/JJ7FJNPZdS+Fln0q/HMUFat1jDQFy+xtIgoBWkecx+fZURlvt+dHYEzbdg4z
+6KQCw9fssbEG3w8K8XvVsZVX4He2hiQ5lG0q/MrEfcXvHk3OBVhpbpex6QGoB3O
SuXtdb2HZ+CYHcQp8IoZjumfs0TsvJAgcgVbKsdT5srDO1syGer2O0k0q+R1BIcT
/kCet80rncgtlFpgy5VUTPoH33Qlcj10Beg2HGC2j+lAdl9RkvR43Vn/9hPmjWW7
8SOJMYzCqT5PjYBWLFtSw1KCzrSj4i2OQWBdbluFDxkSWWh4G3TaBtBgRgSn+Ofx
Ans+BwAFEQgAq35FKTMsDLagphyMV1rHNPBJSlJKqdBGTpSIKkXKWYuItM6FaPCM
bT2nRIYWff2lbmJNnMLapaoaAH4ZGeWKVdw8KsFrpDU4CG08K3XF2VOZj+rirvmb
WSE5M2YSb5l6Q0kh02CrwS5dnUrhz+cr7jy+ofkQSga+ODf+nbUg+IGBCDjsF8ma
170L2AfkfTOebLp4ATFhqzSx9bnPTj3OXBiR+/0nvrEaBi6khpEd5B/CNtviEVlv
sx0xt9s8LcMxlCXApxVsedvl1dc/8kQwLOPQDhaFFQc+4mwlqI1ahsALQ9gtxcbZ
OVlA24fKKxdvJ/Y7Ji0aLr9rDa3lQPnQtohGBBgRAgAGBQJADbpVAAoJEDkXZKLX
xFvOxMMAoOiRip+nA4UlxNH8cZ5dfujNoKzLAJ9GWWGA7oInBvs3Q1xhigjGH0Yc
OA==
=QdzS
-----END PGP PUBLIC KEY BLOCK-----


