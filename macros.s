/**
	#macro which calls a function to print a string
	.macro mWriteString str              
    	leaw  \str, %si
    	call .writeStringIn
    .endm

    #function to print the string
    .func writeStringIn
    .writeStringIn:
        lodsb                   # load byte at ds:si into al (advancing si)
        orb  %al, %al           # test if character is 0 (end)
        jz   .writeStringDone   # jmp to end if 0
        movb $0x0e, %ah
        int  $0x10
        jmp  .writeStringIn
    .writeStringDone:
        retw
    .endfunc
*/
    .macro mDisplayInfo str, len, poisition
        lea \str, %bp
        mov \len, %cx
        mov \poisition, %dx
        mov	$0x000F, %bx
        call .DisplayString
    .endm

    .macro mDisplayWarn str, len, poisition
        lea \str, %bp
        mov \len, %cx
        mov \poisition, %dx
        mov	$0x008C, %bx
        call .DisplayString
    .endm

    .func DisplayString
    .DisplayString:        
        push %es
	    mov	%ds, %ax
	    mov	%ax, %es            # ES:BP

        mov	$0x1301, %ax
        int  $0x10
        pop %es
    
        retw
    .endfunc

