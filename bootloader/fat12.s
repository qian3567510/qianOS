.set RootDirSectors, 14 				#Root directory sector count FAT12总共14个扇区，需遍历14次
.set SecNoOfRootDir, 19 				#1st sector of root directory
.set DeltaSecNo, 17 					#BPB_(RsvdSecCnt+NumFATs*FATSz) -2
.set SecNoOfFAT1, 1 					#1st sector of FAT1

    /*boot sector defination of FAT12*/
	BS_OEMName: .ascii "QIANboot" 		#OEM String, 8 bytes required
	BPB_BytsPerSec: .word 512 			#Bytes per sector 
	BPB_SecPerCluse: .byte 1 			#Sector per cluse 
	BPB_ResvdSecCnt: .word 1 			#Reserved sector count 
	BPB_NumFATs: .byte 2 				#Number of FATs 
	BPB_RootEntCnt: .word 224 			#Root entries count 
	BPB_TotSec16: .word 2880 			#Total sector number 
	BPB_Media: .byte 0xF0 				# Media descriptor 
	BPB_FATSz16: .word 9 				# FAT size(sectors) 
	BPB_SecPerTrk: .word 18 			# Sector per track 
	BPB_NumHeads: .word 2 				# Number of magnetic heads 
	BPB_HiddSec: .long 0 				# Number of hidden sectors 
	BPB_TotSec32: .long 0 				# If TotSec16 equal 0, this works 
	BS_DrvNum: .byte 0 					# Driver number of interrupt 13 
	BS_Reserved1: .byte 0 				# Reserved 
	BS_BootSig: .byte 0x29 				# Boot signal 
	BS_VolID: .long 0 					# Volume ID 
	BS_VolLab: .ascii "boot loader" 	# Volume label, 11 bytes required 
	BS_FileSysType: .ascii "FAT12   " 	# File system type, 8 bytes required
    /**End of boot sector*/
