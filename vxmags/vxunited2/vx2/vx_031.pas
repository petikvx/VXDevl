
{While digging around on simtel.net, i ran across this little cleaner for    }
{the version of tai-pan that the disassembly included is of. I thought       }
{someone out there might want to see this and poke around with it a little.  }
{You gotta love love these little freeware cleaners! This source has been    }
{modified to be compataible with the viewer. For more information on this    }
{tool, and the unmodified source, see the original zip which is included.    }
{You can try to compile this source, but i would advise getting the original }
{from the zip and work with that. Im not a pascal coder, so i might have     }
{fucked things up. Enjoy!



{ FREEWARE 1994 by BUGSY & SPAWN of OBSESSION                                }
{ This is the Tai-Pan nuker source-code.                                     }
{ Do with it as you like, just remember who made it.                         }
{                                                                            }
{ Contact us if you like. Coders are :                                       }
{                                                                            }
{ Benjamin Petersen                                                          }
{ Joergen Jensensvej 16B                                                     }
{ 4700 Naestved, Denmark                                                     }
{ Phone # : +45 53725610 or +45 40204347                                     }
{ Internet mail address : bugsy@cybernet.dk                                  }
{                                                                            }
{ Michael Skovslund                                                          }
{ Stationsvej 2                                                              }
{ 4681 Herfoelge, Denmark                                                    }
{ Phone # : +45 56275314                                                     }
{                                                                            }
{            Well, sorry about missing comments in source code!              }
{                                                                            }
{                          BUGSY & SPAWN of ...                              }
{                   ��� �   ��� ��� ��� ��� � ��� ���                        }
{                   � � ��� ��� ��  ��� ��� � � � � �                        }
{                   � � � � � � �   � � � � � � � � �                        }
{                   ��� ��� ��� ��� ��� ��� � ��� � �                        }

Program NukeTaiPan;
{$A+,B-,D+,E+,F-,G-,I-,L+,N-,O-,P-,Q-,R-,S+,T-,V+,X+,Y+}
{$M 64000,0,655360}  {BP 7.0 compiler options}

Uses
  Crt,
  Dos;

Const
  MyFileSize  = 10950;                  {Remember to change this one}
  MyFileName  = 'NUKETAIP.EXE';
  VirusSize   = 438;
  VirStartPos = 438;
  NameOfVirus = 'Tai-Pan (Whisper)';
  SomeOfVirus = #$E8#$00#$00#$5E#$83#$EE#$03#$B8#$CE#$7B#$CD#$21;

Var
  InFile      : File of Byte;

  DirInfo     : SearchRec;

  Ct          : Byte;
  H           ,
  M           ,
  S           ,
  Hund        ,
  SubCt       : Word;

  NumOfFile   ,
  NumOfVirF   ,
  NumOfVirR   : Longint;

  Path        : PathStr;
  Dir         : DirStr;
  Name        : NameStr;
  Ext         : ExtStr;

  OldDir      ,
  FileNameStr ,
  MyPathStr   ,
  MyDir       ,
  TempPath    ,
  Buffer      : String;

  ESC         ,
  DoSub       ,
  Beep        ,
  NoClean     ,
  Debug       ,
  Prompt      : Boolean;

Procedure Error (Err : Byte);
Begin
  Write ('Error (',Err,') : ');
  Case Err Of
    1 : WriteLn ('No files found.');
    2 : WriteLn ('Can''t find directory.');
    3 : Begin
          WriteLn (NameOfVirus,' virus is resident in memory !');
          WriteLn ('Don''t worry, ',NameOfVirus,' can''t infect this file.');
          WriteLn ('Boot from a clean floppy and try again.');
        End;
    4 : WriteLn (MyFileName, ' has been changed !. Virus ?');
    5 : WriteLn ('Remember to change ''MZ'' to ''ZM''.'); { Internal         }
                                                          { Use any hex file }
                                                          { editor to change }
                                                          { the 2 first byte }
                                                          { in the exefile   }
                                                          { from 'MZ' to 'ZM'}
                                                          { This is done to  }
                                                          { prevent tai-pan  }
                                                          { to infect the    }
                                                          { tai-pan numer    }
    6 : WriteLn ('Can''t seek into file.');
    7 : WriteLn ('Can''t read from file.');
    8 : WriteLn ('Can''t close file.');
    9 : WriteLn ('Can''t open file.');
   10 : WriteLn ('Can''t write to file.');
   11 : WriteLn ('Cat''t truncate file.');
   12 : WriteLn ('Can''t get directory.');
  Else
    WriteLn ('Not defined, programmer forgot this one !');   {hmmmmm....}
  End;
  If OldDir <> '' Then ChDir(OldDir);
  If IOResult <> 0 Then;
  Window (1, 1, 80, 25);
  GotoXY (1, 24);
  Halt (1);
End;

Procedure WriteHelpScr;
Begin
  GotoXY (WhereX, WhereY-1);
  WriteLn ('��������������������������������������������������������������Ŀ');
  WriteLn ('� USAGE : Nuketaip [path]filename.ext [/nc] [/b] [/s] [/p]     �');
  WriteLn ('�                                                              �');
  WriteLn ('� You can use ANY valid dos wildcard.                          �');
  WriteLn ('�                                                              �');
  WriteLn ('� /NC no clean  /P prompt before cleaning file                 �');
  WriteLn ('� /B  beep      /S search subdirectory                         �');
  WriteLn ('��������������������������������������������������������������Ĵ');
  WriteLn ('�  �       Coded by BUGSY & SPAWN of..              �          �');
  WriteLn ('� ����--                                            �          �');
  WriteLn ('�  � ��� �   ��� ��� ��� ��� � ��� ���              �          �');
  WriteLn ('�  | � � ��� ��� ��  ��� ��� � � � � �              �          �');
  WriteLn ('�    � � � � � � �   � � � � � � � � �              �          �');
  WriteLn ('�    ��� ��� ��� ��� ��� ��� � ��� � � �            �          �');
  WriteLn ('� If you want to contact us, our addresses are  :   �          �');
  WriteLn ('���������������������������������������������������Ĵ          �');
  WriteLn ('� Benjamin Petersen      � Michael Skovslund        �          �');
  WriteLn ('� Joergen Jensensvej     � Stationsvej 2            �          �');
  WriteLn ('� 4700 Naestved, Denmark � 4681 Herfoelge, Denmark  �          �');
  WriteLn ('� Phone # : +45 53725610 � Phone # : +45 56275314   �          �');
  WriteLn ('����������������������������������������������������������������');
  Halt (1);
End;

Function VirusIsInMem : Boolean; Assembler;
Var
  TempByte : Boolean;
Asm
    mov  TempByte, 0
    mov  ax, 7BCEh			{Virus id word}
    int  21h
    cmp  ax, 7BCEh
    Jne  @NoVirus
    mov  TempByte, 1
@NoVirus:
    mov  al,TempByte
    xor  ah,ah
End;

Function ChkFile : Boolean;
Var
  Ct       ,
  TempByte : Byte;
  ErrCode  : Integer;

Begin
  Inc (NumOfFile);
  FileMode := 0;   {ReadOnly mode}
  If MyDir[Length(MyDir)] = '\' Then
    Write (MyDir,DirInfo.Name)
  Else
    Write (MyDir,'\',DirInfo.Name);

  Assign (InFile, DirInfo.Name);
  Reset (InFile);
  If IOResult <> 0 Then Begin
    WriteLn (' ERROR : Can''t open file, share violation ?');
    ChkFile := False;
    Exit;
  End;

  Seek (InFile, DirInfo.Size - VirStartPos);
  If IOResult <> 0 Then Error (6);

  Buffer := '';
  While Length (Buffer) < Length(SomeOfVirus) do
    Buffer := Buffer + ' ';
  For Ct := 1 to Length (SomeOfVirus) Do Begin
    Read(InFile, TempByte);
    If IOResult <> 0 Then Error (7);
    Buffer[Ct] := Chr (TempByte);
  End;
  Close (InFile);
  If IOResult <> 0 Then Error (8);

  If Buffer = SomeOfVirus Then Begin
    ChkFile := True;
    WriteLn (', INFECTET WITH ',NameOfVirus);
  End Else Begin
    ChkFile := False;
    WriteLn;
  End;
  FileMode := 2;       {Read/Write mode}
End;

Procedure CleanFile;
Var
  Ch       : Char;
  TempByte : Byte;
  Restore  : Array [1..10] of Byte;
  Bolp     ,
  Pif      : Word;
  RealSize : LongInt;
  Ct       : Byte;
  SndCt    : Word;

Begin
  Inc (NumOfVirF);
  If Beep Then
    For Ct := 1 To 2 Do Begin
      For SndCt := 0 To 100 Do Begin
        Sound(1000+(8*SndCt));
        Delay(1);
      End;
      For SndCt := 100 DownTo 0 Do Begin
        Sound(1000+(8*SndCt));
        Delay(1);
      End;
      NoSound;
      NoSound;    {Just in case}
    End;

  If NoClean Then Exit;

  If Prompt Then Begin
    Write('Do you wish to clean this file (y/n) ? ');
    Ch := Upcase (ReadKey);
    If Ch = 'Y' Then
      WriteLn (Ch)
    Else Begin
      WriteLn ('N');
      Exit;
    End;
  End;

  Write ('Cleaning file, ');
  If DirInfo.Attr AND ReadOnly = ReadOnly Then Begin
    WriteLn ('BAD. ------> read only <------');
    Exit;
  End;

  Assign (InFile, DirInfo.Name);
  Reset (InFile);
  If IOResult <> 0 Then Error (9);

  Seek (InFile, DirInfo.Size - 10);             {Read original SS:SP, CS:IP}
  If IOResult <> 0 Then Error (6);

  For Ct := 1 To 10 Do Begin
    Read (InFile, Restore[Ct]);
    If IOResult <> 0 Then Error (7);
  End;

  Seek (InFile, 14);                            {Restore SS:SP, CS:IP}
  If IOResult <> 0 Then Error (6);
  For Ct := 1 to 10 Do Begin
    Write(InFile, Restore[Ct]);
    If IOResult <> 0 Then Error (7);
  End;

  Seek (InFile, 2);
  If IOResult <> 0 Then Error (6);
  Read (InFile, TempByte);
  If IOResult <> 0 Then Error (7);
  Bolp := TempByte;
  Read (InFile, TempByte);
  If IOResult <> 0 Then Error (7);
  Bolp := Bolp + (TempByte * $100);             {Read byte on last page (BOLP)}
  Read (InFile, TempByte);
  If IOResult <> 0 Then Error (7);
  Pif := TempByte;
  Read (InFile, TempByte);
  If IOResult <> 0 Then Error (7);
  Pif := Pif + (TempByte * $100);               {Read pages in file (PIF)}
  RealSize := Pif * $200 + Bolp;                {Calc. the real filesize}
  RealSize := RealSize - VirusSize;             {Calc. original filesize}

  Pif  := RealSize DIV $200;
  Bolp := RealSize MOD $200;
  Seek(InFile, 2);
  If IOResult <> 0 Then Error (6);
  TempByte := Bolp MOD $100;                    {Write original filesize}
  Write(InFile, TempByte);
  If IOResult <> 0 Then Error (10);
  TempByte := Bolp DIV $100;
  Write(InFile, TempByte);
  If IOResult <> 0 Then Error (10);
  TempByte := Pif MOD $100;
  Write(InFile, TempByte);
  If IOResult <> 0 Then Error (10);
  TempByte := Pif DIV $100;
  Write(InFile, TempByte);
  If IOResult <> 0 Then Error (10);
  Seek (InFile, DirInfo.size - VirusSize);
  If IOResult <> 0 Then Error (6);
  Truncate (InFile);                             {Delete virus from file}
  If IOResult <> 0 Then Error (11);
  Close (InFile);                               {Finito boyz}
  If IOResult <> 0 Then Error (8);
  WriteLn ('Done !');
  Inc (NumOfVirR);
End;

Procedure OneDir;
Var
  Ch : Char;

Begin
  GetDir(0, MyDir);
  If IOResult <> 0 Then Error (12);
  FindFirst (FileNameStr, anyfile, DirInfo);
  While (DosError = 0) AND (ESC = False) Do Begin
    If Keypressed Then Begin
      Ch := ReadKey;
      If Ch = #27 Then
        ESC := True;
        End;
If (DirInfo.Size >= VirusSize) AND (DirInfo.Name <> '.') AND (DirInfo.Name <> '..') Then
      If ChkFile Then
        CleanFile;
    FindNext(DirInfo);
  End;
End;

Procedure DoSubDir (SubDirInfo : SearchRec);
Begin
  If ESC Then Exit;
  If SubCt <> 0 Then Begin
    ChDir (SubDirInfo.Name);
    If IOResult <> 0 Then Error (2);
  End;
  Inc (SubCt);

  If DosError = 0 Then Begin
    FindFirst ('*.*', AnyFile, SubDirInfo);
If (SubDirInfo.Attr AND Directory = Directory) And (SubDirInfo.Name[1] <> '.') Then
      DoSubDir (SubDirInfo);
    While DosError = 0 Do Begin
      FindNext (SubDirInfo);
      If DosError <> 0 Then Break;
If (SubDirInfo.Attr AND Directory = Directory) And (SubDirInfo.Name[1] <> '.')Then
        DoSubDir (SubDirInfo);
    End;
    Dec (SubCt);

    GetDir (0, TempPath);
    If IOResult <> 0 Then Error (2);
    OneDir;
    DosError := 0;
    If SubCt <> 0 Then Begin
      ChDir ('..');
      If IOResult <> 0 Then Error (2);
    End;
  End;
End;

Procedure FindVirus;
Var
  TempStr   : String[2];
  TempByte  : Byte;
  TimeUsed  : LongInt;

Begin
  GetTime (H,M,S,Hund);
  TimeUsed := ((H * 3600) + (M * 60) + S);
  Window (1, 7, 80, 22);
  If VirusIsInMem Then Error (3);

  If NOT Debug Then Begin
{    FindFirst (ParamStr(0), AnyFile, DirInfo);
If (DirInfo.Size <> MyFileSize) OR (DirInfo.Name <> MyFileName) Then Error (4);

    TempStr := '--';
    Assign (InFile, ParamStr(0));
    Reset (InFile);
    Read (InFile, Byte(TempStr[1]));
    Read (InFile, Byte(TempStr[2]));
    Close (InFile);
    If TempStr <> 'ZM' Then Error (5);}
  End;

  GetDir (0, OldDir);
  If MyPathStr <> '' Then Begin
    ChDir(MyPathStr);
    If IOResult <> 0 Then Error (2);
  End;

  If DoSub Then Begin
    Ct := 0;
    DirInfo.Name := FileNameStr;
    DosError := 0;
    DoSubDir (DirInfo)
  End Else Begin
    OneDir;
  End;

  If MyPathStr <> '' Then Begin
    ChDir (OldDir);
    If IOResult <> 0 Then Error (2);
  End;

  GetTime (H,M,S,Hund);
  TimeUsed := ((H * 3600) + (M * 60) + S) - TimeUsed;

  WriteLn;
  WriteLn ('Files checked  : ', NumOfFile);
  WriteLn ('Infectet files : ', NumOfVirF);
  WriteLn ('Repaired files : ', NumOfVirR);
  If TimeUsed <> 0 Then Begin
  WriteLn ('Files/second   : ', (NumOfFile/TimeUsed):0:2);
WriteLn ('Timed used     : ', TimeUsed DIV 60,' Min ', (TimeUsed - (TimeUsed DIV 60 * 60)):0,' Sec.');
  End;
  WriteLn;

  If ESC Then
    Write ('Terminated by user !')
  Else
    Write ('All files done !');

  Window (1, 1, 80, 25);
  GotoXY (1, 24);
End;

Procedure UpcaseStr (Var Str : String);
Var
  Ct : Byte;

Begin
  For Ct := 1 to Length (Str) Do
    Str[Ct] := Upcase(Str[Ct]);
End;

Procedure ChkParam;
Var
  Ct         : Byte;
  MyParamStr : String;

Begin
  ESC        := False;
  DoSub      := False;
  Beep       := False;
  NoClean    := False;
  Debug      := False;
  Prompt     := False;
  MyParamStr := '';
  OldDir     := '';
  NumOfFile  := 0;
  NumOfVirF  := 0;
  NumOfVirR  := 0;

  If ParamCount < 1 Then WriteHelpScr;

  If POS ('/',ParamStr(1)) <> 0 Then WriteHelpScr;
  For Ct := 1 To ParamCount Do
    MyParamStr := MyParamStr + ParamStr(Ct) + ' ';
  UpcaseStr (MyParamStr);

  If Pos ('/B ' , MyParamStr) > 0 Then Beep    := True;
  If Pos ('/NC ', MyParamStr) > 0 Then NoClean := True;
  If Pos ('/S ' , MyParamStr) > 0 Then DoSub   := True;
  If Pos ('/D ' , MyParamStr) > 0 Then Debug   := True; {undoc param HA!}
  If Pos ('/P ' , MyParamStr) > 0 Then Prompt  := True;

  Ct          := 0;                             {Split path and filename}
  MyPathStr   := '';
  FileNameStr := '';

  MyParamStr := ParamStr(1);                    {[path]Filename}
  UpcaseStr(MyParamStr);

  FSplit (MyParamStr, Dir, Name, Ext);

  MyPathStr   := Dir;
  If MyPathStr[Length(MyPathStr)] = ':' Then {Patch for : 'c:'}
    MyPathStr := MyPathStr + '\'
  Else
    If MyPathStr[Length(MyPathStr)] = '\' Then
If (MyPathStr[Length(MyPathStr) - 1] <> ':') AND (Length(MyParamStr) <> 1) Then
Delete (MyPathStr, Length(MyPathStr), 1); {Patch for : 'c:\' and patch for : 'c:\test\tmpdir\'}

  FileNameStr := Name + Ext;
  If FileNameStr = '' Then
    FileNameStr := '*.*';

  WriteLn ('Path      : ', MyPathStr);
  WriteLn ('Filename  : ', FileNameStr);
  WriteLn ('����������������������������������������������������������������');
  GotoXY (1, 23);
  WriteLn ('����������������������������������������������������������������');
End;

Begin
  Clrscr;
  WriteLn;
  WriteLn ('Nuke ',NameOfVirus,' virus v 2.2 FREEWARE 1994 by OBSESSION.');
  WriteLn;
  ChkParam;
  FindVirus;
End.

