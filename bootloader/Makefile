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


all : $(OBJS) loader.bin

%.o : %.s
	$(AS) -o $@ $<

boot.elf : boot_fat12.o
	$(LD) $< -o $@ $(LDFLAGS_BOOT)

loader.elf: loader_fat12.o
	$(LD) $< -o $@ $(LDFLAGS_LOADER)

%.bin: %.elf
	$(OBJCOPY) $(TRIM_FLAGS) $< $@

.PHONY: clean
clean:
	rm -f *.o *.elf *.bin
