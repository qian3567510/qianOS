AS = x86_64-unknown-linux-gnu-as
CC = x86_64-unknown-linux-gnu-gcc
CXX = x86_64-unknown-linux-gnu-g++
LD = x86_64-unknown-linux-gnu-ld
OBJCOPY = x86_64-unknown-linux-gnu-objcopy

OBJCOPY = objcopy

CC_FLAGS = -mcmodel=large -m64 -fno-builtin -fno-stack-protector -c 
KERNEL_LD_FLAGS = -b elf64-x86-64 -z muldefs -T Kernel.lds

all: system.elf
	$(OBJCOPY) -I elf64-x86-64 -S -R ".eh_frame" -R ".comment" -O binary system.elf ../kernel.bin

system.elf:	head.o main.o printk.o
	$(LD) $(KERNEL_LD_FLAGS) -o $@ $^

%.o : %.s
	$(AS) --64 -o $@ $<

%.o : %.S
	$(AS) --64 -o $@ $<

main.o:	main.c
	$(CC) $(CC_FLAGS) -o $@ $<

printk.o: printk.c
	$(CC) $(CC_FLAGS) -o $@ $<

clean:
	rm -f *.o *.elf *.bin 

