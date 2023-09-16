# qianOS
An boot loader for my own qianOS writen by GNU assembly language

Tool Chain
MinGW64        #Since I often work in Win enviroment, so MinGW64 is a better choise
imagefs.exe    #A tool subject for raw write floppy disk IMG file
Bochs          #Emulator of x86, debug step by step

2023 - 09 - 15

log
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
