.equ BOOTSEG, 0x07c0					# original address of boot
.set BaseOfStack, 0x7c00 				# Stack base address, inner
/**
* 2023/09/03 实验证明 16位模式下也可以按20位寻址Seg::Offset
* loader.bin的链接文件lds中只需要地址同OffsetOfLoader，但OffsetOfLoader后四位应为0
* set OffsetOfLoader, 0x0EF1 / 0xEF8就是错误的；合理的有 0x0100,0x0EF0, 0x0000
* 之前判定的超16位寻址极限是错误的；loader中需要首先讲DS寄存器修改为和CS一致；DS=CS后就能正常寻址
*/
.set BaseOfLoader, 0x1000 				#Section loading address of LOADER.BIN 
.set OffsetOfLoader, 0x0000 			#Loading offset of LOADER.BIN

.section .text
.globl _start
.code16
_start:
	#Floppy header of FAT12
	jmp _entry 							#Start to boot.
	nop 								#nop required
	.include "fat12.s"
	.include "macros.s"

/*Global Variables*/
wRootDirSizeForLoop: .word 0			#14个扇区
wSectorNo: .word 0 						# Current sector number to read 

/*String Table*/
StartBootMessage:	.ascii	"Start Boot"
NoLoaderMessage:	.ascii	"ERROR:No LOADER Found"
LoaderFileName:		.asciz	"LOADER  BIN"

_entry:
	# Initial registers.
	mov %cs,%ax
	mov %ax,%ds
	mov %ax,%es

	xor %ax, %ax
	mov %ax,%ss
	mov $BaseOfStack, %sp

	# Clear screen 
	mov $0x0600,%ax 					# %ah=6, %al=0 
	mov $0x0700,%bx 					# Black white 
	mov $0,%cx 							# Top left: (0,0) 
	mov $0x184F,%dx 					# Bottom right: (80,50) 
	int $0x10 							# BIOS int 10h, ah=6: Initialize/Clear screen 

	# Display "Start Boot" 
	# 用宏写，更直观易懂
	mDisplayInfo StartBootMessage, $10, $0x0100  # dh=1,Row 1; dh=0,Col 0;

	# Reset floppy */
	xor %ah,%ah
	xor %dl,%dl 						# %dl=0: floppy driver 0 
	int $0x13 							# BIOS int 13h, ah=0: Reset driver 0 

	# Find LOADER.BIN in root directory of driver 0 
	movw $SecNoOfRootDir, (wSectorNo)	#19; FAT1 FAT2各占9个扇区，加上启动扇区；所以目录区从19开始
	movw $RootDirSectors, (wRootDirSizeForLoop)		#需遍历14个扇区

	# Read root dir sector to memory 
_SearchInRootDirBegin:
	cmpw $0,(wRootDirSizeForLoop) 		# If searching in root dir，遍历14次，遍历14个扇区 
	jz _NoLoaderBinFile 				# can find LOADER.BIN ? 
	decw (wRootDirSizeForLoop)
	mov $0,%ax
	mov %ax,%es 						# %es <- BaseOfLoader
	mov $0x8000,%bx 					# %bx <- OffsetOfLoader ES:BX the sector been loaded
	mov (wSectorNo),%ax 				# %ax <- sector number in root 
	mov $1,%cl
	call ReadSector						#读入一个扇区, AX中为扇区号，从19开始。
	
	mov $0x8000,%di 					# %ds:%di -> the sector been loaded
	mov $LoaderFileName,%si 			# si file name LOADER  BIN
	cld									#将DF标志寄存器清零，用于指示变址寄存器SI/DI地址指针自动向后增加
	mov $0x10,%dx						#DX=10h=16，每个扇区有512/32=16个目录项需遍历

/* Search for "LOADER BIN", FAT12 save file name in 12 bytes, 8 bytes for
file name, 3 bytes for suffix, last 1 bytes for '20'. If file name is
less than 8 bytes, filled with '20'. So "LOADER.BIN" is saved as:
"LOADER BIN"(4f4c 4441 5245 2020 4942 4e).
*/
_SearchForLoaderBin:
	cmp $0,%dx 							# Read control 判断是否已经遍历了一个扇区中的所有目录项 
	jz _GoNextSectorInRootDir			#JZ，运算结果为0则跳转读下一个扇区
	dec %dx
	mov $11,%cx							#FAT12，文件名最多11位

_CmpFilename:
	cmp $0,%cx
	jz _FileFound 						#If 11 chars are all identical，说明找到了loader.bin 
	dec %cx
	lodsb 								# %ds:(%si) -> %al 将SI所指的一个字节加载到AL
	cmp %es:(%di),%al					#对文件名的11个字节逐个按字节比对
	jz _KeepCmpFilename					#相等则继续比对
	jmp _FilenameDifferent 				#不相等，说明没有比中 Different 

_KeepCmpFilename:
	inc %di								#将DI指向下一个字节
	jmp _CmpFilename 					#继续比较直至发现差异或11个字节都一致 

_FilenameDifferent:
	and $0xFFE0,%di 					# Go to head of this entry，将DI复位至目录项的头部 
	add $0x20,%di						#偏移32，指向下一个目录项
	mov $LoaderFileName,%si 			
	jmp _SearchForLoaderBin

_GoNextSectorInRootDir:					#指向下一个扇区，读入此扇区继续比对
	addw $1,(wSectorNo)
	jmp _SearchInRootDirBegin

	# Not found LOADER.BIN in root dir. 在遍历14个扇区后
_NoLoaderBinFile:
	mDisplayWarn NoLoaderMessage, $21, $0x0200		# dh=2,Row 2; dh=0, Col 0;

	jmp . 								# Infinite loop 

	# Found. 
_FileFound:
	mov $RootDirSectors,%ax
	and $0xFFE0,%di 					# Start of current entry, 32 bytes per entry 
	add $0x1A,%di 						# 将DI偏移26个字节，指向FAT表中的起始簇号 
	movw %es:(%di),%cx					# 将起始簇号（指向的是扇区号）存储在CX
	push %cx 							# Save index of this sector in FAT 缓存簇号，11行之后GetFATEntry作为入参
	add %ax,%cx							# 偏移14个扇区
	add $DeltaSecNo,%cx 				# LOADER.BIN's start sector saved in %cl #注释错误，偏移17个扇区
	mov $BaseOfLoader,%ax
	mov %ax,%es 						# %es <- BaseOfLoader 
	mov $OffsetOfLoader,%bx 			# %bx <- OffsetOfLoader ES:BX Loader.bin been loaded
	mov %cx,%ax 						# %ax <- Sector number AX中为逻辑扇区号

	# Load LOADER.BIN's sector's to memory. #每加载一个扇区，打印一个>到屏幕
_LoadingFile:
	push %ax
	push %bx
	mov $0x0E,%ah
	mov $'>',%al 						# Char to print 
	mov $0x0F,%bl 						# Front color: white 
	int $0x10 							# BIOS int 10h, ah=0xe: Print char 
	pop %bx
	pop %ax

	mov $1,%cl
	call ReadSector						#读入簇号所指代的扇区，在AX
	pop %ax 							#从堆栈中取出起始簇号（来自11行前代码），用作GetFATEntry的入参 
	call GetFATEntry
	cmp $0x0FFF,%ax                     #判断是否已经到最后一个簇
	jz _FileLoaded
	push %ax 							# Save index of this sector in FAT 
	mov $RootDirSectors,%dx
	add %dx,%ax							#偏移14个扇区
	add $DeltaSecNo,%ax					#偏移17个扇区
	add (BPB_BytsPerSec),%bx			#偏移512字节，预备读入下一个扇区
	jmp _LoadingFile

_FileLoaded:
	ljmp $BaseOfLoader, $OffsetOfLoader
	#jmp $0x1000, $0x0000				# 段间跳转，CS寄存器将会赋值为0x1000
	#jmp $0xE000						# 段内跳转，CS寄存器将不会改变；且有16位寻址限制
	#这两种跳转目前已知的区别是CS寄存器，如果直接跳转到E000,CS寄存器不变； 使用段间跳转，CS会被赋值为0E00
	# 2023/9/3 应使用ljmp $BaseOfLoader, $OffsetOfLoader这样的写法，可保证CS寄存器设置为正确地址
/*******************************************************************
	Jump to LOADER.BIN start address in memory.
********************************************************************/

/*2023/9/15 经验证将函数定义放在最后，WinImage才可正确识别软盘*/
.include "bootsector.s"


/*两种结尾填充写法都可以*/
#.org 510 								# Skip to address 0x510. 
#.word 0xAA55 							# Write boot flag to 1st sector(512 bytes) end

#.org 496
#LABEL_DESC_CODE32: .word 0xA1B2, 0xC3D4, 0xE5F6, 0x01AB
# 小端存储 高位在高地址区，低位在低地址区；对一个变量，以8bit为单位前后交叉
#.quad 0xA1B2C3D4E5F601AB 小端存储为 AB 01 F6 E5 D4 C3 B2 A1
#           其中AB为低位，则首先存储AB，再存储01；存储时内存区域都是从低地址区 向 高地址区扩展
#.long 0xA1B2C3D4, 0xE5F601AB 小端存储为 d4 c3 b2 a1 ab 01 f6 e5
# 说明是以变量为单位，按8bit前后交叉存储；8bit内的顺序不做交换。
# .word 0xA1B2, 0xC3D4, 0xE5F6, 0x01AB 小端存储为 b2 a1 d4 c3 f6 e5 ab 01
# 进一步说明存储时是以变量为单位，每个变量内部按8bit前后交叉存储，低地址区开始存放低位，例如B2
#
# 以存储FF FF 00 00 01 9A CF 00为例，其代表的quad定义 0x00CF9A010000FFFF
# 这是GDT中的CODE32代码段定义，0b0000000011001111100110100000000100000000000000001111111111111111
# 从后往前，代表从0位 至 63位
# LABEL_DESC_CODE32: .quad 0b0000000011001111100110100000000100000000000000001111111111111111
# 说明定义变量时，还是有必要按其规定长度匹配对应的类型


/*两种结尾填充写法都可以*/
_end: 
	.fill 0x01FE - (_end - _start)
	.byte 0x55, 0xAA					#注意上一行的写法为Intel小端字序，实际存储的是55 AA

