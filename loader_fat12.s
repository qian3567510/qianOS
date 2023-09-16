.code16
.section .text

.globl _start

_start:
    jmp _init
    nop

/*Const Values Table*/
.equ LOADERSEG, 0x10000
.set DATASEG, 0x1000				# DATA DS段地址，同CS段；在使用相对地址寻址时（例如变量名），DS=CS
									# 在使用绝对地址寻址时，需要将DS等段寄存器（例如ES）修改为0
.equ BaseOfKernelFile, 0x0000 				
.equ OffsetOfKernelFile, 0x100000

.equ BaseTmpOfKernelFile, 0x0000 				
.equ OffsetTmpOfKernelFile, 0x7E00

.set MemoryStructBufferAddr, 0x7E00

.equ RootDirSectors, 14 			#Root directory sector count FAT12总共14个扇区，需遍历14次
.equ SecNoOfRootDir, 19 			#1st sector of root directory 
.equ SecNoOfFAT1, 1 				#1st sector of FAT1 
.equ DeltaSecNo, 17 				#BPB_(RsvdSecCnt+NumFATs*FATSz) -2
 
/*Variables Table*/
OffsetOfKernelFileCount: .long OffsetOfKernelFile
wRootDirSizeForLoop: .word  RootDirSectors	#14个扇区
wSectorNo: .word 0 						# Current sector number to read 

/*Display Messages Table*/
StartLoaderMessage:	.ascii   "Start myLoader........"
NoLoaderMessage:	.ascii	"ERROR:No KERNEL Found"
KernelFileName:		.ascii	"KERNEL  BIN"
StartGetMemStructMessage:	.ascii	"Start Get Memory Struct (address,size,type)."
GetMemStructErrMessage:	.ascii	"Get Memory Struct ERROR"
GetMemStructOKMessage:	.ascii	"Get Memory Struct SUCCESSFUL!"

StartGetSVGAVBEInfoMessage:	.ascii	"Start Get SVGA VBE Info"
GetSVGAVBEInfoErrMessage:	.ascii	"Get SVGA VBE Info ERROR"
GetSVGAVBEInfoOKMessage:	.ascii	"Get SVGA VBE Info SUCCESSFUL!"

StartGetSVGAModeInfoMessage:.ascii	"Start Get SVGA Mode Info"
GetSVGAModeInfoErrMessage:	.ascii	"Get SVGA Mode Info ERROR"
GetSVGAModeInfoOKMessage:	.ascii	"Get SVGA Mode Info SUCCESSFUL!"

.include "fat12.s"
.include "bootsector.s"
.include "macros.s" 

#Global Descriptor Table
/**
* 目前已知的有两种设置GDT CODE段的方式。
* 方式1：
*      将段基址显式设定为loader.bin的加载地址BaseOfLoader，实际也是CS段寄存器；要求loader加载地址的偏移量最后四位为0
*          LABEL_DESC_CODE32:  Descriptor  0x10000, 0xFFFFF, (DA_C + DA_32)
*      使用ljmp $SelectorCode32, $LABEL_SEG_CODE32
*
* 方式2：
*      LABEL_DESC_CODE32:  Descriptor  0, (SegCode32Len - 1), (DA_C + DA_32)
*      之后将段基址准确计算出来，精确指向LABEL_SEG_CODE32的实际地址
*      使用ljmp $SelectorCode32, $0
*
* 但都要求必须显示设置GdtPtr的值为 DS<<4 + LableGDT；因为Gnu Assembly中无法准确变量地址设置带段的偏移量
*/

#方式1：GDT简易定义法，示例书籍中一般使用此定义法；要点是CODE段基址要同加载地址=CS段地址
LABEL_GDT: .long 0,0
LABEL_DESC_CODE32: .long 0x0000FFFF, 0x00CF9A01
	#段基址 0x00010000  Limit：F FFFF; 004F中的4代表L=1，暂时无意义，9A代表可读可执行代码段		
	#段基址需要显式定义为和Loader加载的段地址一致，=CS<<4
LABEL_DESC_DATA32: .long 0x0000FFFF, 0x00CF9200
	#段基址 0x00000000  Limit：F FFFF（5个F）；00CF中的C代表L=1 AVL=1,92代表存在内存中的可读写数据段
	#因后续内存页等使用绝对地址定义，所以DATA段基址需要设定为0，不可和CODE段再混淆在一起
LABEL_DESC_VIDEO:  .long 0x8000FFFF, 0x00CF920B
	#段基址 0x000B8000，指向显存区域

#方式2: 宏定义法，优点是直观、易懂；推荐！
/**
LABEL_GDT:          Descriptor  0,                        0, 0
#LABEL_DESC_CODE32:  Descriptor  0,       (SegCode32Len - 1), (DA_C + DA_32)
LABEL_DESC_CODE32:  Descriptor  0x10000,       0xFFFFF, (DA_C + DA_32)
LABEL_DESC_DATA32:  Descriptor  0x00000,       0xFFFFF, DA_DRW
LABEL_DESC_VIDEO:   Descriptor  0xB8000,        0xffff, DA_DRW
*/

.set GdtLen, (. - LABEL_GDT)	#GDT Length，每一段长度为8 Byte

GdtPtr: .word  (GdtLen - 1)		#GDT Limit
        .long  LABEL_GDT		#GDT BaseAddress，should be specified as DS<<4 + LableGDT after.
		#GNU Asm中没有类似org的伪指令，所有LABEL实际是相对于文件头的偏移量，所以需要在运行时重新计算
		#在lgdt GdpPtr指令中，实际是lgdt DS:GdtPtr，所以需要注意DS的值是否为CS 或 指向CS的选择子

# GDT Segment Selectors
.set    SelectorCode32, (LABEL_DESC_CODE32 - LABEL_GDT)
.equ    SelectorData32, (LABEL_DESC_DATA32 - LABEL_GDT)
.set    SelectorVideo,  (LABEL_DESC_VIDEO  - LABEL_GDT)

#Section GDT64
	LABEL_GDT64: .quad 0
	LABEL_DESC_CODE64: .quad 0x0020980000000000
	LABEL_DESC_DATA64: .quad 0x0000920000000000
	
# The length of GDT64 (48 Bype)
	.equ GdtLen64, (. - LABEL_GDT64)

# The pointer of GDT64
	GdtPtr64: .word (GdtLen64-1)
			  .long LABEL_GDT64

# Segment selectors
	.equ SelectorCode64, (LABEL_DESC_CODE64 - LABEL_GDT64)
	.equ SelectorData64, (LABEL_DESC_DATA64 - LABEL_GDT64)

_init:
    mov %cs, %ax
    mov %ax, %ds  #2023/09/03 调整DS=CS之后，能够正常显示；lds文件中只需要指定偏移和OffsetOfLoader一致
    mov %ax, %es

    xor %ax, %ax
	mov %ax, %ss
	mov $0x7C00, %sp

    mov $0xb800,%ax
    mov %ax,%gs
    mov $0xf,%ah
    mov $'L',%al
    mov %ax,%gs:((80*0+39)*2)

    #Display on Screen...
	mDisplayInfo StartLoaderMessage, $22, $0x0200		# dh=2,Row 2; dh=0,Col 0; 
	/*
	mov $0x1301, %ax
	mov $0x000F, %bx
	mov $22, %cx					#StartLoaderMessage的长度为22（十进制）
	mov $0x0200, %dx				#第二行
	
	push %ax
	mov %ds, %ax
	mov %ax, %es
	#xor %ax, %ax
	#mov %ax, %es					#验证BP的默认段寄存器是否为ES
	pop %ax
	
	mov $StartLoaderMessage, %bp	#09/14 证实bp的默认段寄存器是ES，所以前面需要将ES=DS=CS
	int $0x10
	*/

    /* Initialize 32-bits code segment descriptor. */
	/**
	* 2023/09/04 
	* 如果在定义GDT时，已显式指定段基址为Loader的加载地址，同CS<<4；则不需要使用此方法在运行时精确设置段基址。
    xor     %eax, %eax
    mov     %cs, %ax
    shl     $4, %eax
    addl    $(LABEL_SEG_CODE32), %eax
    movw    %ax, (LABEL_DESC_CODE32 + 2)
    shr     $16, %eax
    movb    %al, (LABEL_DESC_CODE32 + 4)
    movb    %ah, (LABEL_DESC_CODE32 + 7)
	*/

    #Prepared for loading GDTR 
	#This is the KEY, otherwise will not get the proper address of gdtr
    xor     %eax, %eax
    mov     %ds, %ax
    shl     $4, %eax
    add     $(LABEL_GDT), %eax		#eax <- gdt base
    movl    %eax, (GdtPtr + 2)

	#Prepared for loading GDTR64 
    xor     %eax, %eax
    mov     %ds, %ax
    shl     $4, %eax
    add     $(LABEL_GDT64), %eax		#eax <- GDT64 base
    movl    %eax, (GdtPtr64 + 2)

	#Open Address via A20;通过置位0x92端口的第一位
	push %ax
	in $0x92, %al
	or $0b00000010, %al		#GNU AS的格式，二进制0b开头，八进制0开头，十六进制0x开头，十进制不能以0开头
	out %al, $0x92
	pop %ax

	cli						#关闭外部中断
	
	lgdt GdtPtr				#加载保护模式下的GTD结构数据
	# 断点调试时，可使用info gdt来查看是否正确加载了GDT；关键在于需要正确的显式设置GDT的实际地址

	mov %cr0, %eax
	or $1, %eax
	mov %eax, %cr0			#通过置位CR0寄存器的第0位开启保护模式

	mov $SelectorData32, %ax
	mov %ax, %fs			#为FS段寄存器加载新的数据段，让fs寄存器具备4G内存寻址能力
	mov %cr0, %eax			#通过复位CR0寄存器的第0位关闭保护模式
	and $0b11111110, %al
	mov %eax, %cr0

	sti						#打开外部中断

/** 2023/09/04 在打开保护模式后，应该要立即使用ljmp指令跳转到32位指令码区域。
*   否则将会导致跳转异常。
*   所以在开启保护模式后，又立即关闭保护模式；检验FS段寄存器是否已被修改。
*/
    # Reset Floppy
	xor %ah, %ah
	xor %dl, %dl
	int $13

	# Serarch for kernel.bin
	movw $SecNoOfRootDir, (wSectorNo)	#目录区从第19扇区开始
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
	call ReadSector						#读入一个扇区
	
	mov $0x8000,%di 					# %es:%di -> the sector been loaded
	mov $KernelFileName,%si 			# %ds:%si file name KERNEL  BIN
	cld									#将DF标志寄存器清零，用于指示变址寄存器SI/DI地址指针自动向后增加
	mov $0x10,%dx						#DX=10h=16，每个扇区有16个目录项需遍历

/* Search for "KERNEL BIN", FAT12 save file name in 12 bytes, 8 bytes for
file name, 3 bytes for suffix, last 1 bytes for '20'. If file name is
less than 8 bytes, filled with '20'. So "KERNEL.BIN" is saved as:
"KERNEL BIN".
*/
_SearchForLoaderBin:
	cmp $0,%dx 							# Read control 
	jz _GoNextSectorInRootDir			#JZ，运算结果为0则跳转
	dec %dx
	mov $11,%cx							#FAT12，文件名最多11位

_CmpFilename:
	cmp $0,%cx
	jz _FileFound 						#If 11 chars are all identical，说明找到了kernel.bin 
	dec %cx
	lodsb 								# %ds:(%si) -> %al将SI所指的FileName一个字节加载到AL
	cmp %es:(%di),%al					#对文件名的11个字节逐个按字节比对
	jz _KeepCmpFilename					#相等则继续比对
	jmp _FilenameDifferent 				#不相等，说明没有比中 Different 

_KeepCmpFilename:
	inc %di								#将DI指向下一个字节
	jmp _CmpFilename 					# Go on loop 

_FilenameDifferent:
	and $0xFFE0,%di 					# Go to head of this entry 
	add $0x20,%di						#偏移32，指向下一个目录项
	mov $KernelFileName, %si 			# Reset %ds:%si file name KERNEL  BIN
	jmp _SearchForLoaderBin

_GoNextSectorInRootDir:					#指向下一个扇区
	addw $1,(wSectorNo)
	jmp _SearchInRootDirBegin

	# Not found KERNEL.BIN in root dir. 
_NoLoaderBinFile:
	mDisplayWarn NoLoaderMessage, $21, $0x0300	# dh=3,Row 3; dh=0,Col 0; 
	/**
	mov	$0x1301, %ax
	mov	$0x008C, %bx
	mov	$0x0300, %dx
	mov	$21, %cx
	push %ax
	mov	%ds, %ax
	mov	%ax, %es
	pop	%ax
	mov	$NoLoaderMessage, %bp
	int	$0x10
	*/

	jmp . 								# Infinite loop 

	# Found. 
_FileFound:
	mov $RootDirSectors,%ax
	and $0xFFE0,%di 					# Start of current entry, 32 bytes per entry 
	add $0x1A,%di 						# 将DI偏移26个字节，指向FAT表中的起始簇号 
	movw %es:(%di),%cx					# 将起始簇号（指向的是扇区号）存储在CX
	push %cx 							# Save index of this sector in FAT 缓存簇号，之后GetFATEntry作为入参
	add %ax,%cx							# 偏移14个扇区
	add $DeltaSecNo,%cx 				# 再偏移17个扇区，将KERNEL.BIN's start sector saved in %cl
	mov $BaseTmpOfKernelFile,%eax
	mov %eax,%es 						# %es <- BaseTmpOfKernelFile 
	mov $OffsetTmpOfKernelFile,%bx 		# %bx <- OffsetTmpOfKernelFile ES:BX Kernel.bin been loaded
	mov %cx,%ax 						# %ax <- Sector number AX中为逻辑扇区号

	# Load KERNEL.BIN's sector's to memory. #每加载一个扇区，打印一个>到屏幕
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
	pop %ax 							#从堆栈中取出缓存的起始簇号（来自11行前代码），用作GetFATEntry的入参

	# Move Kernel.bin to High address
	push %cx
	push %eax							# 起始簇号（来自前序的puch %cs，后续的push %ax），用作GetFATEntry的入参
	push %fs
	push %edi
	push %ds
	push %esi

	mov $0x0200, %cx
	mov $BaseOfKernelFile, %ax
	mov %ax, %fs						# FS段寄存器，之前通过特别手法获得了4G内存寻址能力
	mov (OffsetOfKernelFileCount), %edi	# OffsetOfKernelFileCount 是个临时变量，保存了内存转移过程中的目标地址
	mov $BaseTmpOfKernelFile, %ax
	mov %ax, %ds						# 因需要使用绝对内存地址，对DS段寄存器清零
	mov $OffsetTmpOfKernelFile, %esi

_MovKernel:
	mov %ds:(%esi), %al
	mov %al, %fs:(%edi)					# FS段寄存器，之前通过特别手法获得了4G内存寻址能力

	inc %esi
	inc %edi
	loop _MovKernel						# 循环次数CX = 0x0200，512

	#mov $0x1000, %eax					#将ds指向 0x1000的作用是什么？ 待分析！20220419；原版有 org 10000，所以DS段可以从0x1000开始
	#mov $0x0E00, %eax					#这样赋值也能调用到4-1的最简内核，但跳转到kernel后还是有报错
	mov $DATASEG, %eax					# 换成这个写法，便于理解；涉及到相对地址寻址操作，DS要设为CS.									
	mov %eax, %ds						#                       相对地址寻址-> 给OffsetKerFileCount赋值
	mov %edi, (OffsetOfKernelFileCount)	#保存edi的偏移值到OffsetOfKernelFileCount变量中

	pop %esi
	pop %ds
	pop %edi
	pop %fs
	pop %eax
	pop %cx
	# Move Kernel.bin complete.	

	call GetFATEntry
	cmp $0x0FFF,%ax						# Is the last cluster?
	jz _FileLoaded
	push %ax 							# Save index of this sector in FAT 
	mov $RootDirSectors,%dx
	add %dx,%ax							# %ax指向的簇号，偏移14个扇区
	add $DeltaSecNo,%ax					# 再偏移17个扇区，指向kernel.bin的下一个数据扇区
	#add (BPB_BytsPerSec),%bx			# 与加载Loader.bin不同，kernel.bin每次读入后即转移到搞地址区域
										# 所以每次都是固定读到OffsetTmpOfKernelFile指向的地址
	jmp _LoadingFile

_FileLoaded:	
	#Disply via GS:B800
	mov $0xB800, %ax
	mov %ax, %gs
	mov $0x0F, %ah
	mov $'G', %al
	#将AX填充至GS:B800向后偏移（80*0 + 39）*2的地址处
	#字符模式 从内存地址0B800开始，专门用于显示字符的内存空间，每个字符占两个字节，低字节保存待显示字符，高字节为颜色属性
	#0x0F，表示白色字体，黑色背景
	mov %ax, %gs:((80*0+39)*2)		#mov [gs:((80*0 + 39)*2)], ax  NASM语法

	# Kill Motor
	push %dx
	mov $0x03F2, %dx
	mov $0, %al
	out %al, %dx
	pop %dx
	
	#Get memory address size type
	mDisplayInfo StartGetMemStructMessage, $44, $0x0400  # dh=4,Row 4; dh=0,Col 0;

	mov	$0, %ebx
	mov	$0, %ax
	mov	%ax, %es
	mov	$MemoryStructBufferAddr, %di		# 绝对地址，ES清零，ES:DI

Label_Get_Mem_Struct:
	mov	$0x0E820, %eax
	mov	$20, %ecx
	mov	$0x534D4150, %edx
	int	$0x15
	jc	Label_Get_Mem_Fail
	add	$20, %di
	incl (MemStructNumber)				# don't understand

	cmp	$0, %ebx
	jne	Label_Get_Mem_Struct
	jmp	Label_Get_Mem_OK

Label_Get_Mem_Fail:
	movl $0,(MemStructNumber)
	mDisplayWarn GetMemStructErrMessage, $23, $0x0500  # dh=5,Row 5; dh=0,Col 0;

Label_Get_Mem_OK:
	mDisplayInfo GetMemStructOKMessage, $29, $0x0600  # dh=6,Row 6; dh=0,Col 0;

#=======	get SVGA information
	mDisplayInfo StartGetSVGAVBEInfoMessage, $23, $0x0800  # dh=8,Row 8; dh=0,Col 0;

	mov	$0x00, %ax
	mov	%ax, %es
	mov	$0x8000, %di
	mov	$0x4F00, %ax
	int	$0x10

	cmp	$0x004F, %ax

	jz	.KO
	
#=======	Fail
	mDisplayWarn GetSVGAVBEInfoErrMessage, $23, $0x0900  # dh=9,Row 9; dh=0,Col 0;
	jmp	.

.KO:
	mDisplayInfo GetSVGAVBEInfoOKMessage, $29, $0x0A00  # dh=0A,Row 10; dh=0,Col 0;

#=======	Get SVGA Mode Info
	mDisplayInfo StartGetSVGAModeInfoMessage, $24, $0x0C00  # dh=0C,Row 12; dh=0,Col 0;

	mov	$0x00, %ax
	mov	%ax, %es
	mov	$0x800E, %si

	movl %es:(%si), %esi
	mov	$0x8200, %edi

Label_SVGA_Mode_Info_Get:
	mov %es:(%esi), %cx

#=======	display SVGA mode information

	push	%ax
	
	mov	$0x00, %ax
	mov	%ch, %al
	call	Label_DispAL

	mov	$0x00, %ax
	mov	%cl, %al	
	call	Label_DispAL

	mov $0x00, %ax
	mov $0xAA, %al			#插入AA作为间隔
	call Label_DispAL
	
	pop	%ax

#======
	
	cmp	$0xFFFF, %cx
	jz	Label_SVGA_Mode_Info_Finish

	mov	$0x4F01, %ax
	int	$0x10

	cmp	$0x004F, %ax

	jnz	Label_SVGA_Mode_Info_FAIL	

	incw	(SVGAModeCounter)
	add	$2, %esi
	add	$0x100, %edi

	jmp	Label_SVGA_Mode_Info_Get
		
Label_SVGA_Mode_Info_FAIL:
	mDisplayWarn GetSVGAModeInfoErrMessage, $24, $0x0D00  # dh=0D,Row 13; dh=0,Col 0;

Label_SET_SVGA_Mode_VESA_VBE_FAIL:
	jmp	.

Label_SVGA_Mode_Info_Finish:
	mDisplayInfo GetSVGAModeInfoOKMessage, $30, $0x0E00  # dh=0E,Row 14; dh=0,Col 0;
	
#=======	set the SVGA mode(VESA VBE)
/*********************************************************
    对于超级VGA显示卡，我们可用AX＝4F02H和下列BX的值来设置其显示模式。

    BX显示模式	属性	BX显示模式	属性
    100H	640×400 256色	    101H	640×480 256色
    102H	800×600 16色	    103H	800×600 256色
    104H	1024×768 16色	    105H	1024×768 256色
    106H	1280×1024 16色	    107H	1280×1024 256色
    108H	80×60 文本模式	     109H	 132×25 文本模式
    10AH	132×43 文本模式	     10BH	 132×50 文本模式
    10CH	132×60 文本模式
********************************************************/
	##  调试输出的支持模式
    ##  0100 - 0105
	##  010D - 0118
	##  0140 - 0144, 0146
	mov	$0x4F02, %ax
#	mov	$0x4180, %bx	#========================mode : 0x180 or 0x143  180 too high
#	mov $0x0103, %bx	#800×600 256色, 显存地址 0xA0000
	mov	$0x4143, %bx	#        x 800, y 600, bpp 32, 1920000 bytes visible
#	mov $0x4144, %bx    #0x4144  x 1024, y 768, bpp 32, 3145728 bytes visible
						#0x0146 (320*200)
	int  $0x10

	cmp	$0x004F, %ax
	jnz	Label_SET_SVGA_Mode_VESA_VBE_FAIL

#=======	init IDT GDT goto protect mode 

	cli			#======close interrupt

	lgdt	GdtPtr

#	lidt	(IDT_POINTER)					#暂时未定义

	mov	%cr0, %eax
	or	$1, %eax
	mov	%eax, %cr0

##	ljmp $SelectorCode32, $(LABEL_PE_CODE32 - _start) 
	ljmp $SelectorCode32, $LABEL_PE_CODE32

.section .text
.code32
LABEL_PE_CODE32:
#=======	go to tmp long mode
	mov	$0x10, %ax
	mov	%ax, %ds
	mov	%ax, %es
	mov	%ax, %fs
	mov	%ax, %ss
	mov	$0x7E00, %esp
	
	call	support_long_mode
	test	%eax,	%eax
	jz	no_support

#=======	init temporary page table 0x90000

#	movw $0x91007, (0x90000)
#	movw $0x91007, (0x90800)
#	movw $0x92007, (0x91000)
#	movw $0x000083, (0x92000)
#	movw $0x200083, (0x92008)
#	movw $0x400083, (0x92010)
#	movw $0x600083, (0x92018)
#	movw $0x800083, (0x92020)
#	movw $0xa00083, (0x92028)

	movl $0x91007, (0x90000)
	movl $0x91007, (0x90800)
	movl $0x92007, (0x91000)
	movl $0x000083, (0x92000)
	movl $0x200083, (0x92008)
	movl $0x400083, (0x92010)
	movl $0x600083, (0x92018)
	movl $0x800083, (0x92020)
	movl $0xa00083, (0x92028)

#=======	load GDTR
	mov $0x08, %ax
	mov %ax, %ds	
	lgdtw GdtPtr64			#GdtPrt64的定义段，和CODE段在一起；所以需要再将DS指向08选择子，实际指向0x10000

	mov	$0x10, %ax
	mov	%ax, %ds
	mov	%ax, %es
	mov	%ax, %fs
	mov	%ax, %gs
	mov	%ax, %ss
	mov	$0x7E00, %esp
	
#=======	open PAE

	mov	%cr4, %eax
	bts	$5, %eax
	mov	%eax, %cr4

#=======	load	cr3

	mov	$0x90000, %eax
	mov	%eax, %cr3

#=======	enable long-mode

	mov	$0xC0000080, %ecx				#IA32_EFER
	rdmsr

	bts	$8, %eax
	wrmsr

#=======	open PE and paging

	mov	%cr0, %eax
	bts	$0, %eax
	bts	$31, %eax
	mov	%eax, %cr0

	ljmp $SelectorCode64, $OffsetOfKernelFile
	# jmpl ljmp都遇到了physical address not available，排查发现是GDT64的Data段没有指向0，导致初始化内存分页表没能指向正确的
	#jmp $0x8, $0x100000
	#jmp .

#=======	test support long mode or not

support_long_mode:
	mov	$0x80000000, %eax
	cpuid
	cmp	$0x80000001, %eax
	setnb %al	
	jb	support_long_mode_done
	mov	$0x80000001, %eax
	cpuid
	bt	$29, %edx
	setc	%al
support_long_mode_done:	
	movzx	%al, %eax
	ret

#=======	no support
no_support:
	jmp .
.set PECode32Len, (. - LABEL_PE_CODE32)

.section .text
.code16
#=======	display Number in AL
/* ==================================================================
Routine: DispAL
Action: 
=======================================================================*/
Label_DispAL:

	push	%ecx
	push	%edx
	push	%edi
	
	mov	(DisplayPosition), %edi
	mov	$0x0F, %ah
	mov	%al, %dl
	shr	$4, %al
	mov	$2, %ecx

.begin:
	and	$0x0F, %al
	cmp	$9, %al	
	ja	.1
	add	$'0', %al
	jmp	.2

.1:
	sub	$0x0A, %al
	add	$'A', %al

.2:
	mov	%ax, %gs:(%edi)
	add	$2, %edi
	
	mov	%dl, %al
	loop	.begin

	mov	%edi, (DisplayPosition)

	pop	%edi
	pop	%edx
	pop	%ecx
	
	ret

.section .data
MemStructNumber: .long 0
SVGAModeCounter: .long 0
DisplayPosition: .long 0


    #ljmpl $SelectorCode32, $0							#如果精确计算设置了段基址，验证有效
    #ljmp $SelectorCode32, $0		#ljmp or ljmpl都可以#如果精确计算设置了段基址，验证有效
	#ljmp $SelectorCode32, $LABEL_SEG_CODE32			#2023/09/04 验证无效
	#ljmp $SelectorCode32, $(LABEL_SEG_CODE32 - _start)	#2023/09/04 验证无效
	#ljmp $SelectorCode32, $0x10000 + $LABEL_SEG_CODE32	#2023/09/04 验证无效
	#ljmp $SelectorCode32, $LABEL_SEG_CODE32				#尝试设置好段基址为0x10000，有效

# 测试代码
.section .text
LABEL_SEG_CODE32: 
.code32
    mov     $(SelectorVideo), %ax
    mov     %ax, %gs                /* Video segment selector(dest) */

    movl    $((80 * 10 + 0) * 2), %edi
    movb    $0xC, %ah               /* 0000: Black Back 1100: Red Front */
    movb    $'P', %al

    mov     %ax, %gs:(%edi)

    /* Stop here, infinite loop. */
    jmp     .

/* Get the length of 32-bit segment code. */
.set    SegCode32Len, . - LABEL_SEG_CODE32


