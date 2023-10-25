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
.include "pm.s"
#.include "lib.h"


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
	#段基址 0x00 01 0000  Limit：F FFFF; 004F中的4代表L=1，暂时无意义，9A代表可读可执行代码段		
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
/* Global Descriptor Table */
/**
LABEL_GDT:          Descriptor        0,                  0, 0
#LABEL_DESC_CODE32:  Descriptor        0, (SegCode32Len - 1), (DA_CR + DA_32)
LABEL_DESC_CODE32:  Descriptor        0, 0xFFFFF, (DA_CR + DA_32)
#LABEL_DESC_CODE32:  Descriptor  0x10000,       0xFFFFF, (DA_C + DA_32)
#LABEL_DESC_CODE32:  Descriptor  0x10000,       0xFFFFF, (DA_CR + DA_32)
# 此处尝试将CODE段的基址都指向了0x10000，后续运行时不再重新设置基址
# 这么做的唯一好处是引用变量时，可以直接使用变量名； 注意：变量名/Label名实际都是相对于头部的偏移量
# 例如jmp时，可以指定具体的跳转标签； 
# 尝试将DA_C 修改为 DA_CR Execute/Read；验证发现如果需要将DS手动指向CODE32，必须为DA_CR
LABEL_DESC_DATA32:  Descriptor        0,      (DataLen - 1), DA_DRW
#LABEL_DESC_DATA32:  Descriptor  0x10000,      0xFFFFF, DA_DRW
#LABEL_DESC_DATA32:  Descriptor        0,      0xFFFFF, DA_DRW
# 此处尝试将DATA的基址也指向了0x10000，后续运行时不再重新设置基址。
# 这么做的唯一好处是引用变量时，可以直接使用变量名； 注意：变量名/Label名实际都是相对于头部的偏移量
# 例如显示字符串时，使用字符串变量名（而不是一个Offset）
#LABEL_DESC_CODE32: .long 0x0000FFFF, 0x00CF9A01
	#段基址 0x00010000  Limit：F FFFF; 004F中的4代表L=1，暂时无意义，9A代表可读可执行代码段		
	#段基址需要显式定义为和Loader加载的段地址一致，=CS<<4
#LABEL_DESC_DATA32: .long 0x0000FFFF, 0x00CF9200
	#段基址 0x00000000  Limit：F FFFF（5个F）；00CF中的C代表L=1 AVL=1,92代表存在内存中的可读写数据段
	#因后续内存页等使用绝对地址定义，所以DATA段基址需要设定为0，不可和CODE段再混淆在一起

	#这个定义有效；在后续可以将ds段寄存器切换指向08选择子，也就是CODE32. 09/17
LABEL_DESC_STACK:   Descriptor        0,         TopOfStack, (DA_DRWA + DA_32)
#LABEL_DESC_VIDEO:   Descriptor  0xB8000,             0xffff, DA_DRW
# 因为需要在Ring3权限下写屏幕，需要将VIDEO段声明为DPL3；让Ring3代码可使用
LABEL_DESC_VIDEO:   Descriptor  0xB8000,             0xffff, (DA_DRW + DA_DPL3)
LABEL_DESC_LDT:     Descriptor        0,       (LDTLen - 1), DA_LDT
# Call Gate Code Descriptor
LABEL_DESC_CODECG:  Descriptor        0, (SegCodeCGLen - 1), (DA_C + DA_32)
# Code of Ring3 DPL Descriptor
LABEL_DESC_CODER3:  Descriptor        0, (SegCodeR3Len - 1), (DA_C + DA_32 + DA_DPL3)
LABEL_DESC_STACKR3: Descriptor        0,       TopOfStackR3, (DA_DRWA + DA_32 + DA_DPL3)
# TSS Descriptor
LABEL_DESC_TSS:     Descriptor        0,       (TSSLen - 1), DA_386TSS
# Gates Descriptor
# 因为需要在Ring3权限下调用Gate，需要将声明修改为DPL3
LABEL_CG_TEST:      Gate    SelectorCodeCG, 0, 0, (DA_386CGate + DA_DPL3)
# PDT/PTE
LABEL_DESC_FLAT_C:  Descriptor        0,            0xfffff, (DA_CR|DA_32|DA_LIMIT_4K)
LABEL_DESC_FLAT_RW: Descriptor        0,            0xfffff, (DA_DRW|DA_LIMIT_4K)
*/

.set GdtLen, (. - LABEL_GDT)	#GDT Length，每一段长度为8 Byte

GdtPtr: .word  (GdtLen - 1)		#GDT Limit
        .long  LABEL_GDT		#GDT BaseAddress，should be specified as DS<<4 + LableGDT after.
		#GNU Asm中没有类似org的伪指令，所有LABEL实际是相对于文件头的偏移量，所以需要在运行时重新计算
		#在lgdt GdpPtr指令中，实际是lgdt DS:GdtPtr，所以需要注意DS的值是否为CS 或 指向CS的选择子

# GDT Segment Selectors(TI flag clear)
.set    SelectorCode32, (LABEL_DESC_CODE32 - LABEL_GDT)
.set    SelectorData32, (LABEL_DESC_DATA32   - LABEL_GDT)
#.set    SelectorStack,  (LABEL_DESC_STACK  - LABEL_GDT)
.set    SelectorVideo,  (LABEL_DESC_VIDEO  - LABEL_GDT)
/**
.set    SelectorLDT,    (LABEL_DESC_LDT    - LABEL_GDT)
.set    SelectorCodeCG, (LABEL_DESC_CODECG - LABEL_GDT)
.set    SelectorCGTest, (LABEL_CG_TEST     - LABEL_GDT)
.set    SelectorCodeR3, (LABEL_DESC_CODER3 - LABEL_GDT + SA_RPL3)
.set    SelectorStackR3,(LABEL_DESC_STACKR3- LABEL_GDT + SA_RPL3)
.set    SelectorTSS,    (LABEL_DESC_TSS - LABEL_GDT)
.set    SelectorFlatC  ,(LABEL_DESC_FLAT_C - LABEL_GDT)
.set    SelectorFlatRW ,(LABEL_DESC_FLAT_RW - LABEL_GDT)
*/

/**
# LDT segment 
LABEL_LDT:
#LABEL_LDT_DESC_CODEA:   Descriptor  0, (CodeALen - 1), (DA_C + DA_32)
LABEL_LDT_DESC_CODEA:   Descriptor  0x10000, 0xFFFFF, (DA_C + DA_32)

.set    LDTLen, (. - LABEL_LDT) 
# LDT Selector (TI flag set)
.set    SelectorLDTCodeA, (LABEL_LDT_DESC_CODEA - LABEL_LDT + SA_TIL)
*/

/**
# 32-bit global data segment. 
LABEL_DATA32:
_PMMessage:     .ascii "Welcome to protect mode! ^-^\n\0"
_LDTMessage:    .ascii "Aha, you jumped into a LDT segment.\n\0"
_ARDSTitle:     .ascii "BaseAddrLo BaseAddrHi LengthLo LengthHi   Type\n\0"
_RAMSizeMes:    .ascii "RAM Size:\0"
_LFMes:         .ascii "\n\0"   # Line Feed Message(New line) 
_AMECount:      .4byte 0        # Address Map Entry Counter 
_CursorPos:     .4byte (80*2+0)*2  # Screen Cursor position for printing 
_MemSize:       .4byte 0        # Usable Memory Size 
_ARDStruct:                     # Address Range Descriptor Structure 
  _BaseAddrLow:     .4byte 0    # Low 32 bits of base address 
  _BaseAddrHigh:    .4byte 0    # High 32 bits of base address 
  _LengthLow:       .4byte 0    # Low 32 bits of length in bytes 
  _LengthHigh:      .4byte 0    # High 32 bits of length in bytes 
  _Type:            .4byte 0    # Address type of this range: 0, 1, other 
_AddrMapBuf:  .space 256, 0      # Address map buffer 
_PageTableNum:  .4byte  0       # Number of page tables 

.set    PMMessage,        (_PMMessage - LABEL_DATA32)
.set    LDTMessage,       (_LDTMessage - LABEL_DATA32)
.set    ARDSTitle,        (_ARDSTitle - LABEL_DATA32)
.set    RAMSizeMes,       (_RAMSizeMes - LABEL_DATA32)
.set    LFMes,            (_LFMes - LABEL_DATA32)
.set    AMECount,         (_AMECount - LABEL_DATA32)
.set    CursorPos,        (_CursorPos - LABEL_DATA32)
.set    MemSize,          (_MemSize - LABEL_DATA32)
.set    ARDStruct,        (_ARDStruct - LABEL_DATA32)
  .set  BaseAddrLow,      (_BaseAddrLow - LABEL_DATA32)
  .set  BaseAddrHigh,     (_BaseAddrHigh - LABEL_DATA32)
  .set  LengthLow,        (_LengthLow - LABEL_DATA32)
  .set  LengthHigh,       (_LengthHigh - LABEL_DATA32)
  .set  Type,             (_Type - LABEL_DATA32)
.set    AddrMapBuf,       (_AddrMapBuf - LABEL_DATA32)
.set    PageTableNum,     (_PageTableNum - LABEL_DATA32)
.set    DataLen,          (. - LABEL_DATA32)
*/
/**
# 32-bit global stack segment. 
.align 4
LABEL_STACK:
.space  512, 0
.set    TopOfStack, (. - LABEL_STACK - 1)

# 32-bit ring 3 stack segment. 
LABEL_STACKR3:
.space  512, 0
.set    TopOfStackR3, (. - LABEL_STACKR3)

LABEL_TSS:
    .4byte  0           # Back Link 
    .4byte  TopOfStack  # ESP0 
    .4byte  SelectorStack # SS0 
    .4byte  0           # ESP1 
    .4byte  0           # SS1 
    .4byte  0           # ESP2 
    .4byte  0           # SS2 
    .4byte  0           # CR3(PDBR) 
    .4byte  0           # EIP 
    .4byte  0           # EFLAGS 
    .4byte  0           # EAX 
    .4byte  0           # ECX 
    .4byte  0           # EDX 
    .4byte  0           # EBX 
    .4byte  0           # ESP 
    .4byte  0           # EBP 
    .4byte  0           # ESI 
    .4byte  0           # EDI 
    .4byte  0           # ES 
    .4byte  0           # CS 
    .4byte  0           # SS 
    .4byte  0           # DS 
    .4byte  0           # FS 
    .4byte  0           # GS 
    .4byte  0           # LDT Segment Selector 
    .2byte  0           # Trap Flag: 1-bit 
    .2byte  (. - LABEL_TSS + 2)     # I/O Map Base Address 
    .byte   0xff        # End 
.set    TSSLen, (. - LABEL_TSS)
*/

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

	/**
	# Get System Address Map 
    xor     %ebx, %ebx             # EBX: Continuation, 0 
    mov     $(_AddrMapBuf), %di    # ES:DI: Buffer Pointer, _AddrMapBuf 
	BEGIN.loop:
    mov     $0xe820, %eax          # EAX: Function code, E820h 
    mov     $20, %ecx              # ECX: Buffer size, 20 
    mov     $0x534d4150, %edx      # EDX: Signature 'SMAP' 
    int     $0x15                  # INT 15h 
    jc      BEGIN.getAMfail
    add     $20, %di               # Increase buffer pointer by 20(bytes) 
    incl    (_AMECount)            # Inc Address Map Entry Counter by 1 
    cmp     $0, %ebx               # End of Address Map? 
    jne     BEGIN.loop
    jmp     BEGIN.getAMok
	BEGIN.getAMfail:                   # Failed to get system address map 
    movl    $0, (_AMECount)
	BEGIN.getAMok:                     # Got system address map
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

	/**
	# Initialize 32-bits code segment descriptor. 
    InitDesc LABEL_SEG_CODE32, LABEL_DESC_CODE32

    # Initialize data segment descriptor. 
    InitDesc LABEL_DATA32, LABEL_DESC_DATA32

    # Initialize stack segment descriptor.
    InitDesc LABEL_STACK, LABEL_DESC_STACK

    # Initialize LDT descriptor in GDT.
    InitDesc LABEL_LDT, LABEL_DESC_LDT

    # Initialize code A descriptor in LDT.
    InitDesc LABEL_CODEA, LABEL_LDT_DESC_CODEA

    # Initialize call gate dest code segment descriptor.
    InitDesc LABEL_SEG_CODECG, LABEL_DESC_CODECG

	# Initialize ring 3 stack segment descriptor. 
    InitDesc LABEL_STACKR3, LABEL_DESC_STACKR3

    # Initialize ring 3 dest code segment descriptor. 
    InitDesc LABEL_SEG_CODER3, LABEL_DESC_CODER3

	# Initialize TSS segment descriptor. 
    InitDesc LABEL_TSS, LABEL_DESC_TSS
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

	/*参考Wenbo Yang的资料，进行LDT/IDT的验证*/
	#ljmp $SelectorCode32, $LABEL_SEG_CODE32				#尝试设置好段基址为0x10000，有效
	#ljmp $SelectorCode32, $0					# 运行时已精确初始化段基址到指定偏移量

/** 2023/09/04 在打开保护模式后，应该要立即使用ljmp指令跳转到32位指令码区域。
*   否则将会导致跳转异常。
*   所以在开启保护模式后，又立即关闭保护模式；检验FS段寄存器是否已被修改。
*/
	mov $SelectorData32, %ax
	mov %ax, %fs			#为FS段寄存器加载新的数据段，让fs寄存器具备4G内存寻址能力
	mov %cr0, %eax			#通过复位CR0寄存器的第0位关闭保护模式
	and $0b11111110, %al
	mov %eax, %cr0

	sti						#打开外部中断


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
	#push %fs
	push %edi
	push %ds
	push %esi

	mov $0x0200, %cx
	#mov $0x1000, %cx

	#mov $BaseOfKernelFile, %ax
	#mov %ax, %fs						# FS段寄存器，之前通过特别手法获得了4G内存寻址能力；在物理环境下不能变更，否则会出错
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
	#pop %fs
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
#	mov	$0x4143, %bx	#        x 800, y 600, bpp 32, 1920000 bytes visible 可用
#	mov $0x4144, %bx    #0x4144  x 1024, y 768, bpp 32, 3145728 bytes visible 会进入全屏
	#0x0146 (320*200)
	#147 ?未知
	#148 - 14C 1152*864，其中14C 1152*864 可用
	mov $0x414C, %bx
#	mov $0x4118, %bx	#gonin fullscreen mode
#	mov $0x4177, %bx	#gonin fullscreen mode
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
/**
.section .text
.code32
# 32-bit code segment for GDT 
LABEL_SEG_CODE32: 
	#call    SetupPaging 

    mov     $(SelectorData32), %ax
    mov     %ax, %ds                # Data segment selector
	mov     $(SelectorData32), %ax
    mov     %ax, %es                # ExtData segment selector 
    mov     $(SelectorStack), %ax
    mov     %ax, %ss                # Stack segment selector 
    mov     $(SelectorVideo), %ax
    mov     %ax, %gs                # Video segment selector(dest) 

	# 如果使用movl $PMMessage, %esi，就需要以下两行代码将ds重指向08
	#mov $(SelectorCode32), %ax		# 将ds指向08选择子，也就是code段
	#mov %ax, %ds					# data 段和 code段混用，为了相对寻址可以找到字符串变量名

    mov     $(TopOfStack), %esp

	push    $(ARDSTitle)           # Display addr range descriptor struct title 
    call    DispStr
    add     $4, %esp
    call    DispAddrMap            # Display system address map 

	# Setup and enable paging
    #call    SetupPaging

	call VMDemo            

    push    $(PMMessage)
    call    DispStr
    add     $4, %esp

 	# Load TSS to TR register 
	mov     $(SelectorTSS), %ax    
    ltr     %ax

	# call Ring3
	pushl   $(SelectorStackR3)     # Fake call procedure. 
    pushl   $(TopOfStackR3)
    pushl   $(SelectorCodeR3)
    pushl   $0
    lret                           # return with no call 

CODE32.3:
    mov     $(SelectorLDT), %ax
    lldt    %ax
    
	#ljmp    $(SelectorLDTCodeA), $LABEL_CODEA
	ljmp    $(SelectorLDTCodeA), $0

# Get the length of 32-bit segment code. 
.set    SegCode32Len, . - LABEL_SEG_CODE32

# 32-bit code segment for LDT 
LABEL_CODEA:
    mov     $(SelectorVideo), %ax
    mov     %ax, %gs

    movb    $0xC, %ah               # 0000: Black Back 1100: Red Front 
    xor     %esi, %esi
    xor     %edi, %edi
    movl    $(LDTMessage), %esi
	#movl $LDTMessage, %esi
    movl    $((80 * 12 + 0) * 2), %edi
    cld                         # Clear DF flag. 

# Display a string from %esi(string offset) to %edi(video segment). 
CODEA.1:
    lodsb                       # Load a byte from source 
    test    %al, %al
    jz      CODEA.2
    mov     %ax, %gs:(%edi)
    add     $2, %edi
    jmp     CODEA.1
CODEA.2:

    # Stop here, infinite loop. 
    jmp     .
.set    CodeALen, (. - LABEL_CODEA)

# 32-bit code segment for call gate destination segment 
LABEL_SEG_CODECG:
    mov     $(SelectorVideo), %ax
    mov     %ax, %gs

    movl    $((80 * 11 + 0) * 2), %edi  # line 11, column 0 
    movb    $0xC, %ah               	# 0000: Black Back 1100: Red Front 
    movb    $'C', %al               	# Print a 'C' 

    mov     %ax, %gs:(%edi)
    lret

# Get the length of 32-bit call gate destination segment code. 
.set    SegCodeCGLen, . - LABEL_SEG_CODECG

# 32-bit code segment for running in ring 3. 
LABEL_SEG_CODER3:
    mov     $(SelectorVideo), %ax
    mov     %ax, %gs

    movl    $((80 * 11 + 1) * 2), %edi  # line 11, column 1 
    movb    $0xC, %ah               	# 0000: Black Back 1100: Red Front 
    movb    $'3', %al               	# Print a '3' 

    mov     %ax, %gs:(%edi)

	# Call Code Call Gate in Ring3
	lcall   $(SelectorCGTest), $0

    jmp     .

# Get the length of 32-bit ring 3 segment code. 
.set    SegCodeR3Len, . - LABEL_SEG_CODER3

.include "lib.h"

SetupPaging:
# Directly map linear addresses to physical addresses for simplification 
    # Get usable PDE number from memory size. 
    xor     %edx, %edx
    mov     (MemSize), %eax         # Memory Size 
    mov     $0x400000, %ebx         # Page table size(bytes), 1024*1024*4 
    div     %ebx                    # temp = MemSize/4M 
    mov     %eax, %ecx
    test    %edx, %edx
    jz      SP.no_remainder
    inc     %ecx
SP.no_remainder:
    mov     %ecx, (PageTableNum)    # number of PDE = ceil(temp) 

    # Directly map linear addresses to physical addresses. 
    # Init page table directories of PageDir0, %ecx entries. 
    mov     $(SelectorFlatRW), %ax
    mov     %ax, %es
    mov     $(PageDirBase0), %edi
    xor     %eax, %eax
    # Set PDE attributes(flags): P: 1, U/S: 1, R/W: 1. 
    mov     $(PageTblBase0 | PG_P | PG_USU | PG_RWW), %eax
SP.1:
    stosl                   # Store %eax to %es:%edi consecutively. 
    add     $4096, %eax     # Page tables are in sequential format. 
    loop    SP.1            # %ecx loops. 

    # Init page tables of PageTbl0, (PageTableNum)*1024 pages. 
    mov     (PageTableNum), %eax # Get saved ecx(number of PDE) 
    shl     $10, %eax       # Loop counter, pages: 1024*(PageTableNum). 
    mov     %eax, %ecx
    mov     $(PageTblBase0), %edi
    # Set PTE attributes(flags): P:1, U/S: 1， R/W: 1. 
    mov     $(PG_P | PG_USU | PG_RWW), %eax
SP.2:
    stosl                   # Store %eax to %es:%edi consecutively. 
    add     $4096, %eax     # Pages are in sequential format. 
    loop    SP.2            # %ecx loops. 

    # Do the same thing for PageDir1 and PageTbl1. 

    # Init page table directories of PageDir1, (PageTableNum) entries. 
    mov     $(SelectorFlatRW), %ax
    mov     %ax, %es
    mov     $(PageDirBase1), %edi
    xor     %eax, %eax
    # Set PDE attributes(flags): P: 1, U/S: 1, R/W: 1. 
    mov     $(PageTblBase1 | PG_P | PG_USU | PG_RWW), %eax
    mov     (PageTableNum), %ecx
SP.3:
    stosl                   # Store %eax to %es:%edi consecutively. 
    add     $4096, %eax     # Page tables are in sequential format. 
    loop    SP.3            # %ecx loops. 

    # Init page tables of PageTbl1, (PageTableNum)*1024 pages. 
    mov     (PageTableNum), %eax # Get saved ecx(number of PDE) 
    shl     $10, %eax       # Loop counter: 1024*(PageTableNum). 
    mov     %eax, %ecx
    mov     $(PageTblBase1), %edi
    # Set PTE attributes(flags): P:1, U/S: 1， R/W: 1. 
    mov     $(PG_P | PG_USU | PG_RWW), %eax
SP.4:
    stosl                   # Store %eax to %es:%edi consecutively. 
    add     $4096, %eax     # Pages are in sequential format. 
    loop    SP.4            # %ecx loops. 

    # Locate and modify the page that includes linear address FuncLinAddr.
     * Assume memory is larger than 8 MB. 
    mov    $(FuncLinAddr), %eax
    shr    $12, %eax        # Get index of PTE which contains FuncLinAddr. 
    shl    $2, %eax         # PTE size is 4-bytes. 
    add    $(PageTblBase1), %eax # Get the pointer to that PTE. 
    # Modify the PTE of the page which contains FuncLinAddr. 
    movl   $(BarPhyAddr | PG_P | PG_USU | PG_RWW), %es:(%eax)

    # Use PageDirBase0 first. 
    mov     $(PageDirBase0), %eax
    mov     %eax, %cr3 # Store base address of page table dir to %cr3. 

    # Enable paging bit in %cr0. 
    mov     %cr0, %eax
    or      $0x80000000, %eax
    mov     %eax, %cr0
    ret

# Display system address map. 
DispAddrMap:
    push    %esi
    push    %edi
    push    %ecx

    mov     $(AddrMapBuf), %esi  # int *p = AddrMapBuf;                     
    mov     (AMECount), %ecx     # for (int i=0; i<AMECount; i++) {         
DMS.loop:
    mov     $5, %edx             #   int j = 5;                             
    mov     $(ARDStruct), %edi   #   int *q = (int *)ARDStruct;             
DMS.1:
    push    (%esi)               #   do {                                   
    call    DispInt              #     printf("%xh", *p);                   
    pop     %eax
    stosl                        #     *q++ = *p;                           
    add     $4, %esi             #     p++;                                 
    dec     %edx                 #     j--;                                 
    cmp     $0, %edx
    jnz     DMS.1                #   } while(j != 0);                       
    call    DispLF               #   printf("\n");                          
    cmpl    $1, (Type)           #   if (Type == AddressRangMemory){        
    jne     DMS.2
    mov     (BaseAddrLow), %eax  #     if(ARDStruct.BaseAddrLow             
    add     (LengthLow), %eax    #        + ARDStruct.LengthLow             
    cmp     (MemSize), %eax      #        > MemSize){                       
    jb      DMS.2                #       MemSize = BaseAddrLow + LengthLow; 
    mov     %eax, (MemSize)      #     }                                    
DMS.2:                           #   }                                      
    loop    DMS.loop             # }                                        

    call    DispLF               # printf("\n");                            
    push    $(RAMSizeMes)
    call    DispStr              # printf("%s", RAMSizeMes);                
    add     $4, %esp

    pushl   (MemSize)
    call    DispInt              # printf("%x", MemSize);                   
    add     $4, %esp
    call    DispLF               # printf("\n");                            

    pop     %ecx
    pop     %edi
    pop     %esi
    ret

#A demo for call different func located on same virtual address.
VMDemo:
    mov     %cs, %ax
    mov     %ax, %ds            # Set %ds to code segment. 
    mov     $(SelectorFlatRW), %ax
    mov     %ax, %es            # Set %es to flat memory segment. 

    pushl   $(FooLen)
    pushl   $(FooOffset)
    pushl   $(FooPhyAddr)
    call    MemCpy              # Copy function foo to FooPhyAddr. 
    add     $12, %esp

    pushl   $(BarLen)
    pushl   $(BarOffset)
    pushl   $(BarPhyAddr)
    call    MemCpy              # Copy function bar to BarPhyAddr. 
    add     $12, %esp

    # Restore data segment selector to %ds and %es. 
    mov     $(SelectorData32), %ax
    mov     %ax, %ds
    mov     %ax, %es

    # Setup and start paging
    call    SetupPaging

    # Function call 1, should print "Foo". 
    lcall   $(SelectorFlatC), $(FuncLinAddr)

    # Change current PDBR from PageDirBase0 to PageDirBase1. 
    mov    $(PageDirBase1), %eax
    mov    %eax, %cr3

    # Function call 2, should print "Bar". 
    lcall   $(SelectorFlatC), $(FuncLinAddr)

    ret

# Function foo, print message "Foo". 
foo:
.set    FooOffset, (. - LABEL_SEG_CODE32)
    mov    $0xc, %ah            # 0000: background black, 1100: font red 
    mov    $'F', %al
    mov    %ax, %gs:((80 * 12 + 3) * 2)    # Line 12, column 3 
    mov    $'o', %al
    mov    %ax, %gs:((80 * 12 + 4) * 2)    # Line 12, column 4 
    mov    %ax, %gs:((80 * 12 + 5) * 2)    # Line 12, column 5 
    lret
.set    FooLen, (. - foo)

# Function bar, print message "Bar". 
bar:
.set    BarOffset, (. - LABEL_SEG_CODE32)
    mov    $0xc, %ah            # 0000: background black, 1100: font red 
    mov    $'B', %al
    mov    %ax, %gs:((80 * 12 + 7) * 2)    # Line 12, column 7 
    mov    $'a', %al
    mov    %ax, %gs:((80 * 12 + 8) * 2)    # Line 12, column 8 
    mov    $'r', %al
    mov    %ax, %gs:((80 * 12 + 9) * 2)    # Line 12, column 9 
    lret
.set    BarLen, (. - bar)
*/
