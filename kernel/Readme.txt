https://blog.csdn.net/weixin_44391390/article/details/111054511

1、undefined reference to ‘__stack_chk_fail’
在Makefile出错文件的gcc那一行加上-fno-stack-protector选项，如：
gcc -mcmodel=large -fno-builtin -fno-stack-protector -m64 -c printk.c
或者是在CFLAGS后添加-fno-stack-protector选项


2、printk.c:(.text+0x7b5)：undefined reference to ‘strlen’

sed -i "s/inline/static inline/g" `grep inline -rl ./`
sed -i "s/static static/static/g" `grep inline -rl ./`

需要在strlen函数的inline前加上static


1111 1111 1111 1111 

1000 0000 0 /__PML4E, Index1			0x102007

000 0000 00 /__PDPTE, Index0			0x103003

00 0000 101	/__PDE, Index 5				0xe0000083  指向了显存

0 0000 0000 0000 0000 0000 2M的页

其实VBE是显卡的一个图形规范标准，它定义了显卡的几种图形模式，每个模式包括屏幕分辨率，像素格式与大小，显存大小。调用BIOS 10h中断可以返回这些数据结构。如果你实在对VBE感兴趣，可以自行阅读其规范 。

这里我们选择使用了VBE的118h模式，该模式下屏幕分辨率为1024x768，显存大小是16.8MB。显存开始地址一般为0xe0000000。

屏幕分辨率为1024x768，即把屏幕分成768行，每行1024个像素点，但每个像素点占用显存的32位数据（4字节，红、绿、蓝、透明各占8位）。我们只要往对应的显存地址写入相应的像素数据，屏幕对应的位置就能显示了。