OBJS = boot.bin 
TMP = boot.elf loader.elf
AS = as
LD = ld

LDFILE_BOOT=boot.lds
LDFLAGS_BOOT=-T $(LDFILE_BOOT)

LDFILE_LOADER=loader.lds
LDFLAGS_LOADER=-T $(LDFILE_LOADER)

TRIM_FLAGS=-j .text -S -O binary


TARGET = test.img

all:$(TARGET)

%.o : %.s
	$(AS) -o $@ $<

boot.elf : boot_fat12.o
	$(LD) $< -o $@ $(LDFLAGS_BOOT)

loader.elf: loader_fat12.o
	$(LD) $< -o $@ $(LDFLAGS_LOADER)

%.bin: %.elf
	objcopy $(TRIM_FLAGS) $< $@

$(TARGET) : $(OBJS) loader.bin
	imagefs c $@ 2880
	imagefs b $@ boot.bin
	imagefs a $@ loader.bin
	imagefs a $@ kernel.bin

.PHONY: clean
clean: 
	del /Q *.o
	del /Q *.elf
	del /Q boot.bin loader.bin
	del /Q $(TARGET)