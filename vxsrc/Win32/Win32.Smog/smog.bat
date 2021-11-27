CLS
ECHO Lets compile Win32.Smog
ECHO (c)by Necronomikon[ZeroGravity]
tasm32 -ml -m -l smog.asm
tlink32 -Tpe -c -aa smog.obj,,, import32.lib
