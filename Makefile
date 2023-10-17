TARGET = boot.img

all : loader.bin boot.bin kernel.bin $(TARGET)

loader.bin:
	make -C ./bootloader

boot.bin:
	make -C ./bootloader

kernel.bin:
	make -C ./kernel

$(TARGET): boot.bin loader.bin kernel.bin
	imagefs c $@ 2880
	imagefs b $@ ./bootloader/boot.bin
	cp ./bootloader/loader.bin ./
	imagefs a $@ loader.bin
	cp ./kernel/kernel.bin ./
	imagefs a $@ kernel.bin

.PHONY: clean
clean:
	make clean -C ./kernel
	make clean -C ./bootloader
	rm -rf *.bin *.asm~ Makefile~ *.img win_FloppyA.txt