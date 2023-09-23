https://blog.csdn.net/weixin_44391390/article/details/111054511

1、undefined reference to ‘__stack_chk_fail’
在Makefile出错文件的gcc那一行加上-fno-stack-protector选项，如：
gcc -mcmodel=large -fno-builtin -fno-stack-protector -m64 -c printk.c
或者是在CFLAGS后添加-fno-stack-protector选项


2、printk.c:(.text+0x7b5)：undefined reference to ‘strlen’

sed -i "s/inline/static inline/g" `grep inline -rl ./`
sed -i "s/static static/static/g" `grep inline -rl ./`

需要在strlen函数的inline前加上static