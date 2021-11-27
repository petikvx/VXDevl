;**********************FLOPDIE V. 2.0**********************************
;	Coded and Copyrighted Opic + Lord Natas [CodeBreakers 1998]
;       A CodeBreaker Production: http:\\www.codebreakers.org
;**********************************************************************
;flopdie v2.0 essentially writes binary (1's 'n 0's) to an entire 
;disk (including boot sectors etc..ie: the entire disk), wiping its contents
;in this fashion it is a standard wiping utility. Yet it is coded entirly
;in assembly and may be usful source as it has many nice little rutines
;which you may incorporate into other progs. (ie: wiping, reading from
;key buffer etc.) it was an old rainy day project of ours i dug up for
;nuc, hope it helps some of you new to asm.
;compiles: a86 flopdie2.asm

start:
	mov	si,80h          ;point to length of command line
        lodsb                   ;put it in al
	or	al,al           ;is it zero
	jz	help            ;yes, print help
	lodsb                   ;next byte
	lodsw                   ;get a word
	cmp	ah,':'          ;did they specify a valid drive?
	jne	help            ;no, show help
	and	al,0dfh         ;convert to uppercase
	sub	al,'A'          ;convert letter to num

	mov	byte ptr [disk],al

print:
	mov	ah,09h          ;using function 09h
	lea	dx,mess         ;display a message on the screen
	int	21h

what:

	mov	ah,1
	int	21h             ;get keyboard input w/echo

good:
	cmp	al,'Y'          ;is al=Y?
	je	yes             ;if yes then display it again
	cmp	al,'y'
	je	yes
	cmp	al,'N'          ;is al=y?
	je	no1             ;if yes then display it again
	cmp	al,'n'
	je	no1
	jmp	try_again

help:
	mov	ah,09h
	lea	dx,usage
	int	21h

	call	overwrite

	mov	ax,4c00h
	int	21h

yes:
	mov	ah,09h
	lea	dx,sure
	int	21h

	mov	ah,1
	int	21h             ;get keyboard input w/echo

	cmp	al,'Y'
	je	dead
	cmp	al,'y'
	je	dead
	cmp	al,'N'
	je	no1
	cmp	al,'n'
	je	no1

try_again:
	lea	dx,nope         ;display a message on the screen  
	mov	ah,09h          ;using function 09h
	int	21h             ;of interrupt 21h
	jmp	what

no1:
	jmp	no

dead:
	mov	ah,09h
	lea	dx,die
	int	21h

kil:
	cld
	mov	ax,'01'                 ;character to store
	mov	cx,256                  ;number of times
	mov	di,offset buffer        ;storage buffer
	rep	stosw                   ;store 'em

	mov	bx,offset buffer        ;what to write
	mov	cx,1                    ;number of sectors
	xor	dx,dx                   ;the sector to start at
kill_loop:
	mov	al,byte ptr [disk]      ;drive A:
	int	26h
	popf                            ;pop the flags off the stack
	inc	dx                      ;next sector
	cmp	dx,2880                 ;have we reached the end?
	jne	kill_loop               ;no, kill more

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
finish:
	mov	ah,09h
	lea	dx,fin
	int	21h


;;;;;;;;next?;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

	mov	ah,09h
	lea	dx,next
	int	21h

	mov	ah,1
	int	21h             ;get keyboard input w/echo

	cmp	al,'Y'
	je	dead
	cmp	al,'y'
	je	dead

no:
	mov	ah,09h
	lea	dx,nodie
	int	21h
done:
	mov	ah,09h
	lea	dx,bye
	int	21h

len	equ	$-yes

	call	overwrite
	mov	ax,4c00h        ;terminate program DOSusing
	int	21h

;the following overwrites our code so this program can't be unpacked as
;easily

overwrite:
	cld
	mov	al,'0'          ;character to store
	mov	cx,len          ;number of times
	mov	di,offset yes
	rep	stosb           ;store 'em

	ret

CR	equ	13              ;enter
LF	equ	10              ;line-feed


usage   db  'Usage: FLOPDIE DRIVE:',CR,LF,'$'

mess    db  '*WELCOME TO FLOPDIE V2.0*',CR,LF,CR,LF
   db  'This program will PERMANENTLY destroy the data on any floppy disk',CR,LF
        db  'present in the drive',CR,LF,CR,LF
        db  'Do you wish to continue? [Y/N] ',07,'$'

nope    db  CR,LF,'Please choose either [Y]es or [N]o $'

sure  db  CR,LF,CR,LF,'Are you SURE you want to kill this floppy? [Y/N] ',07,'$'

die     db  CR,LF,CR,LF,'***TERMINATING FLOPPY***',CR,LF,'$' 

fin     db  CR,LF,'*****FLOPPY EXTINCT*****',CR,LF,'$'

bye     db  CR,LF,'Thank you for using FLOPDIE v2.0...',CR,LF
        db  CR,LF,'Coded & (C) Opic + Lord Natas [CodeBreakers 98]',CR,LF,'$'

nodie   db  CR,LF,CR,LF,'***FLOPDIE v2.0 ABORTED***',CR,LF,'$'

next    db  CR,LF,'Press [Y]es to kill another, any other key to exit... $'

disk    db  0

buffer  equ $           ;crap to write to disk
