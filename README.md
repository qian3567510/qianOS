# Welcome to qianOS
An boot loader for my own qianOS written by GNU assembly language.

Tool Chain

|             |                                                              |      |
| :---------- | :----------------------------------------------------------- | ---- |
| MinGW64     | Since I often work in Win Env., so MinGW64 is a better choise for GNU |      |
| [Imagefs.exe](https://github.com/henck/imagefs) | A tool subject to raw write floppy disk IMG file in Win Env.   |      |
| Bochs       | Emulator of x86, debug step by step. Cool!                   |      |
|             |                                                              |      |

2023 - 09 - 15

1. 参考《田宇-一个64位操作系统的设计与实现》 和 《台湾杨文博-WriteOS GAS语法》，使用GNU Assembly语法重写了boot loader，并加载预先编译好的kernel.bin（功能有限，显示几个色带，打印Hello World）

2. GNU中缺乏类似NASM中org伪指令（据分析其作用应可直接修改各Label、变量的偏移地址；而GNU中各Label实际只代表了相对文件头的偏移地址）
   - 因loader程序的代码/数据段是混杂的，所以需要在初始化时，将DS段寄存器也指向CS段基址
   - 在类似GdtPtr的定义中，如果直接和NASM一样使用Lable地址，将无法指向正确的物理地址
   - 在加载之前，需要根据CS段地址，左移4位后加上Lable地址，计算出运行时的实际物理地址，再赋值回GdtPtr

3. GDT中CODE段基址需和Loader加载地址一致（此处设置的是0x10000），DATA段基址从零开始（主要原因是设置内存分页时，用的绝对物理地址）

4. 使用变量相对寻址时，默认段寄存器是DS；需要注意检查DS的值是否正确。
5. 使用绝对地址时，注意检查匹配的段寄存器地址是否已设置为0x00； 例如：
        xor %ax, %ax
        mov %ax, %es

    设置堆栈指向了绝对地址，所以需要将SS段设置为零

6. 在进入保护模式后，加载GdtPtr64时，注意适用于规则4中所述。
   - GdtPtr64是一个定义在CODE/DATA混杂的段中，其寻址方式实际为DS:GdtPtr64
   - 需要检查DS段的设置是否指向了0x10000,也就是CODE段基址
   - 保护模式下，DS段设置的应是段选择子，所以需要手动mov $0x08, %ds


2023/09/17

在尝试手动将GDT CODE指向0x10000, DATA段指向0；试图在后续使用到变量标签之前，再手动将DS段指向到CODE32段，以求获取到正确的偏移地址。

调试过程中发现以下差异：
LABEL_DESC_CODE32: .long 0x0000FFFF, 0x00CF9A01
	#段基址 0x00010000  Limit：F FFFF; 004F中的4代表L=1，暂时无意义，9A代表可读可执行代码段		
	#段基址需要显式定义为和Loader加载的段地址一致，=CS<<4
LABEL_DESC_DATA32: .long 0x0000FFFF, 0x00CF9200

使用这样的定义，CODE是可执行、可读； 后续也可以将DS段寄存器指向CODE选择子。

<bochs:53> info gdt
Global Descriptor Table (base=0x0000000000010209, limit=47):
GDT[0x0000]=??? descriptor hi=0x00000000, lo=0x00000000
GDT[0x0008]=Code segment, base=0x00010000, limit=0xffffffff, Execute/Read, Non-Conforming, 32-bit
GDT[0x0010]=Data segment, base=0x00000000, limit=0xffffffff, Read/Write
GDT[0x0018]=Data segment, base=0x00010288, limit=0x000001ff, Read/Write, Accessed
GDT[0x0020]=Data segment, base=0x000b8000, limit=0x0000ffff, Read/Write
GDT[0x0028]=LDT

使用书中定义，code段是ExecuteOnly，所以无法将CODE选择子赋给DS段寄存器。
<bochs:53> info gdt
Global Descriptor Table (base=0x0000000000010209, limit=47):
GDT[0x0000]=??? descriptor hi=0x00000000, lo=0x00000000
GDT[0x0008]=Code segment, base=0x00010000, limit=0x000fffff, Execute-Only, Non-Conforming, 32-bit
GDT[0x0010]=Data segment, base=0x00000000, limit=0x000fffff, Read/Write
GDT[0x0018]=Data segment, base=0x00010288, limit=0x000001ff, Read/Write, Accessed
GDT[0x0020]=Data segment, base=0x000b8000, limit=0x0000ffff, Read/Write
GDT[0x0028]=LDT

#LABEL_DESC_CODE32:  Descriptor  0x10000,       0xFFFFF, (DA_C + DA_32)
LABEL_DESC_CODE32:  Descriptor  0x10000,       0xFFFFF, (DA_CR + DA_32)
需要将定义修改为DA_CR，这样CODE/DATA段才能混用!

2023/9/20
测试Call Gate
测试跳到Ring3代码；主要修改VIDEO选择子的权限定义，增加DPL3，表示允许Ring3代码使用屏幕缓存输出字符
测试在Ring3代码中中通过Call Gate调用实现低权限向高权限转移，需要准备TSS信息供切换

2023/09/21
测试验证虚拟内存的分页

# GNU 中的org，是偏移多少字节的意思；用于内存布局。
# 和NASM中的org地址偏移伪指令有本质上的不同。

2023/09/22
内核编码，设置了交叉编译环境；可直接在Windows中编译elf目标文件。

inline strlen之前需要增加static修饰

分辨率目前还只支持800*600