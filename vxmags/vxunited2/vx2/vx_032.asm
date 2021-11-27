;              'ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿'
;               º ****::: Crack- HexWorkshop v2.54 :::**** º
;               º ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ º
;               º ******* -= nUcLeii/Vx United! =- ******* º
;              'ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ'


comment *
	Ok, i know this really doesnt belong here, but what the hell. One thing
you will learn over your years of programming and being a general computer nerd
is the fact that software is expensive. Most is way over priced, and it makes 
one almost feel obligated to steal it. I don't really support stealing software
but if i was too worried about ethics in the computer world, then i wouldnt be 
coding viruses.
	Anyway, after putting up with annoying nag screens, and timed out demos
it starts to bug you, hey thats the point right? Well, you can't live with it, 
and if your like me being tight up for cash, you can't really afford to shell 
out that hard earned buck. Like any programmer, your next thought is, hmm, i 
wonder if i can remove the protection from this software. The answer is most 
often yes. For most smaller applications, removing these nag screens and such 
is a simple task. Its harder to get motivation to search the web for a crack 
than it is to do it yourself. Just go grab a copy of W32Dasm or soft ice and go
to work. For this one, i just searched for the error string that you get when
an invalid registration number is entered. After finding the routine that
displays it, search for where it was called from. In this case it was a simple
jne error_routine. All that was needed was to patch the jne to a je. Simple hu?
The only problem here is i never went through the code to patch the jne for 
when a correct reg number is entered. So if you crack it with this, then get
around to buying the software, you need a new exe cause its gonna reject your
legit registration.
HexWorkshop v2.54 32 bit is available from http://www.bpsoft.com

*
.model tiny
.code
org  100h
start:
	lea	dx,maintitle            ;point to the title prompt
	call	print

openfile:
	lea     dx,filename             ;what file to open
	mov     ax,3d02h                ;function to open file read/write access
	int     21h                     ;open it
	cmp	ax,02h                  ;do we have it?
	jne	write                   ;no? then lets get outta here

filedontexist:
	lea	dx,notfound
	call	print
	jmp	exit

write:
	xchg	bx,ax                   ;save the file handle
        xor     cx,cx                   ;offset origin of new file position
        mov     dx,909ah                ;offset to point to
        mov     ax,4200h                ;function to set the file pointer
        int     21h                     ;move that pointer


	lea     dx,string               ;data to write
	mov     ah,40h                  ;function to write to file
	mov     cx,01h                  ;number of bytes to write
	int     21h                     ;patch it

	lea     dx,cracked              ;finished message.		
	call	print
exit:
	mov     ah,3eh                  ;function to close file with handle
	int     21h                     ;close it
	mov     ax,4c00h                ;time to leave
	int     21h                     ;bye bye!
Print:
	mov	ah,9h
	int	21h
	ret

maintitle       db 0Dh,0Ah
                db '       ÉÍÍÍÍÍËÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍËÍÍÍÍÍ»',0Dh,0Ah
                db '       ³ ::::³Hex Workshop  v2.54 32bit³:::: ³',0Dh,0Ah
                db '       º ÄÄÄÄÁÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÁÄÄÄÄ º',0Dh,0Ah
                db '       ³ ::: Patch by nUcLeii - (c) 1998 ::: ³',0Dh,0Ah
                db '       ÌÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ¹',0Dh,0Ah
                db '       ³Available from http://www.bpsoft.com ³',0Dh,0Ah
                db '       ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ',0Dh,0Ah
                db 0Dh,0Ah,'$'

filename db "HWORKS32.EXE",0
notfound db 0dh,0ah,"File not found!$"
cracked  db 0dh,0ah,"File successfully patched. Enter any serial/name/org. Enjoy! $"
string   db 85h,0
end start
