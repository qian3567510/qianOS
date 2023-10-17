/* ==================================================================
Routine: ReadSector
Action: Read %cl Sectors From %ax sector(floppy) to %es:%bx(memory)
Assume sector number is 'x', then:
x/(BPB_SecPerTrk) = y,
x%(BPB_SecPerTrk) = z.
The remainder 'z' PLUS 1 is the start sector number;
The quotient 'y' devide by BPB_NumHeads(RIGHT SHIFT 1 bit)is cylinder
number;
AND 'y' by 1 can got magnetic header.
=======================================================================*/
ReadSector:
	push %ebp
	mov %esp,%ebp
	sub $2,%esp 						# Reserve space for saving %cl 
	mov %cl,-2(%ebp)
	push %bx 							# Save bx 
	mov (BPB_SecPerTrk), %bl 			# %bl: the devider 
	div %bl 							# 'y' in %al, 'z' in %ah 
	inc %ah 							# z++, got start sector 
	mov %ah,%cl 						# %cl <- start sector number 
	mov %al,%dh 						# %dh <- 'y' 
	shr $1,%al 							# 'y'/BPB_NumHeads 
	mov %al,%ch 						# %ch <- Cylinder number(y>>1) 
	and $1,%dh 							# %dh <- Magnetic header(y&1) 
	pop %bx 							# Restore %bx 

	# Now, we got cylinder number in %ch, start sector number in %cl, magneticheader in %dh. 

	mov (BS_DrvNum), %dl
	
GoOnReading:
	mov $2,%ah
	mov -2(%ebp),%al 					# Read %al sectors 
	int $0x13
	jc GoOnReading 						# If CF set 1, mean read error, reread. 
	
	add $2,%esp							#恢复栈帧
	pop %ebp
	ret


#==================	新写法，便于理解 =================================#
/* ==================================================================
Routine: ReadSector
Action: Read %cl Sectors From %ax sector(floppy) to %es:%bx(memory)
Assume sector number is 'x', then:
x/(BPB_SecPerTrk) = y,
x%(BPB_SecPerTrk) = z.
The remainder 'z' PLUS 1 is the start sector number;
The quotient 'y' devide by BPB_NumHeads(RIGHT SHIFT 1 bit)is cylinder
number;
AND 'y' by 1 can got magnetic header.
======================================================================*/
/*
ReadSector:
	push %ebp
	mov %esp, %ebp
	sub $2, %esp
	push %bx

	mov	(BPB_SecPerTrk), %bl
	div	%bl
	inc	%ah								#余数，扇区号（每个磁道18个扇区，从1开始计数）
	mov	%ah, (dbSectorNum)
	mov	%al, %ah 						#商，磁道
	shr	$1, %al
	mov	%al, (dbTrackNum)				#磁道/柱面 Cylinder number
	and	$1, %ah							#商，最后一位[0 or 1],表示在哪个磁头。Magnetic header
	mov	%ah, (dbResistiveNum)

	mov $0x2, %ah						#AH=02,INT13H，读入指定扇区
	mov	%cl, %al
	mov	(BS_DrvNum),%dl
	mov	(dbResistiveNum), %dh
	mov (dbTrackNum), %ch
	mov (dbSectorNum), %cl

	pop %bx

_Go_On_Reading:
	int $0x13
	jc	_Go_On_Reading

	add $2, %esp						#恢复栈帧
	pop %ebp
	ret
*/
/* ==================================================================
Routine: GetFATEntry
Action: Find %ax sector's index in FAT, save result in %ax
*/
GetFATEntry:
	push %es
	push %bx
	push %ax

	mov $0, %ax
	mov %ax,%es 						# ????Left 4K bytes for FAT 
	pop %ax
	movb $0,(bOdd)
	mov $3,%bx
	mul %bx 							# %dx:%ax = %ax*3 
	mov $2,%bx
	div %bx 							# %dx:%ax/2 
	cmp $0,%dx 							# remainder %dx = 0
	jz LABEL_EVEN
	movb $1,(bOdd)

LABEL_EVEN:
	xor %dx,%dx 						# Now %ax is the offset of FATEntry in FAT 
	mov (BPB_BytsPerSec),%bx
	div %bx 							# %dx:%ax/BPB_BytsPerSec 
	push %dx
	mov $0x8000, %bx					#Temp load in 8000h of memory
	add $SecNoOfFAT1,%ax 				# ax FATEntry's sector 
	mov $2,%cl 							# Read 2 sectors in 1 time, because FATEntry 
	call ReadSector 					# may be in 2 sectors. 
	pop %dx
	add %dx,%bx
	mov %es:(%bx),%ax
	cmpb $1,(bOdd)
	jnz LABEL_EVEN_2
	shr $4,%ax

LABEL_EVEN_2:
	and $0x0FFF,%ax
LABEL_GET_FAT_ENTRY_OK:
	pop %bx
	pop %es
	ret

.set SecNoOfFAT1, 1 					#1st sector of FAT1

bOdd: .byte 0 							# odd or even?
dbResistiveNum: .byte 0					#;磁头/柱面数[0, 1]
dbTrackNum: .byte 0						#;磁道数
dbSectorNum: .byte 0					#;扇区数
