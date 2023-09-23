OBJS = boot.bin 
TMP = boot.elf loader.elf
AS = as
LD = ld

AS = x86_64-unknown-linux-gnu-as
CC = x86_64-unknown-linux-gnu-gcc
CXX = x86_64-unknown-linux-gnu-g++
LD = x86_64-unknown-linux-gnu-ld
OBJCOPY = x86_64-unknown-linux-gnu-objcopy

OBJCOPY = objcopy

LDFILE_BOOT=boot.lds
LDFLAGS_BOOT=-T $(LDFILE_BOOT)

LDFILE_LOADER=loader.lds
LDFLAGS_LOADER=-T $(LDFILE_LOADER)

TRIM_FLAGS=-j .text -S -O binary


TARGET = qianOS.img

all : kernel.bin $(TARGET)

%.o : %.s
	$(AS) -o $@ $<

boot.elf : boot_fat12.o
	$(LD) $< -o $@ $(LDFLAGS_BOOT)

loader.elf: loader_fat12.o
	$(LD) $< -o $@ $(LDFLAGS_LOADER)

%.bin: %.elf
	$(OBJCOPY) $(TRIM_FLAGS) $< $@

kernel.bin:
	make -C ./kernel

$(TARGET) : $(OBJS) loader.bin kernel.bin
	imagefs c $@ 2880
	imagefs b $@ boot.bin
	imagefs a $@ loader.bin
	imagefs a $@ kernel.bin

.PHONY: clean
clean:
	make clean -C ./kernel
	rm -f *.o *.elf *.bin $(TARGET)
