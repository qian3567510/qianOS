/***************************************************
*		版权声明
*
*	本操作系统名为：MINE
*	该操作系统未经授权不得以盈利或非盈利为目的进行开发，
*	只允许个人学习以及公开交流使用
*
*	代码最终所有权及解释权归田宇所有；
*
*	本模块作者：	田宇
*	EMail:		345538255@qq.com
*
*
***************************************************/

#include "lib.h"
#include "printk.h"

void Start_Kernel(void)
{
	int *addr = (int *)0xffff800000a00000;
	int i;

	//BX=0x4143, 800*600 32bit
	//Pos.XResolution = 800;
	//Pos.YResolution = 600;

	//BX=0x4144, 1024*768 32bit
	//Going fullscreen mode in Bochs2.7, and very difficult to quit from fullscreen mode!	
	//Pos.XResolution = 1024;
	//Pos.YResolution = 768;

	//BX=0x414C, 1152*864 32bit
	Pos.XResolution = 1152;
	Pos.YResolution = 864;

	//BX=0x4177, 1280*768 32bit
	//Going fullscreen mode in Bochs2.7, and very difficult to quit from fullscreen mode!
	//Pos.XResolution = 1280;
	//Pos.YResolution = 768;

	//BX=0x4118, 1024*768?
	//Pos.XResolution = 1024;
	//Pos.YResolution = 768;

	Pos.XPosition = 0;
	Pos.YPosition = 0;

	Pos.XCharSize = 8;
	Pos.YCharSize = 16;

	Pos.FB_addr = (int *)0xffff800000a00000;
	Pos.FB_length = (Pos.XResolution * Pos.YResolution * 4);

	for(i = 0 ;i<Pos.XResolution*20;i++)
	{
		*((char *)addr+0)=(char)0x00;
		*((char *)addr+1)=(char)0x00;
		*((char *)addr+2)=(char)0xff;
		*((char *)addr+3)=(char)0x00;	
		addr +=1;	
	}
	for(i = 0 ;i<Pos.XResolution*20;i++)
	{
		*((char *)addr+0)=(char)0x00;
		*((char *)addr+1)=(char)0xff;
		*((char *)addr+2)=(char)0x00;
		*((char *)addr+3)=(char)0x00;	
		addr +=1;	
	}
	for(i = 0 ;i<Pos.XResolution*20;i++)
	{
		*((char *)addr+0)=(char)0xff;
		*((char *)addr+1)=(char)0x00;
		*((char *)addr+2)=(char)0x00;
		*((char *)addr+3)=(char)0x00;	
		addr +=1;	
	}
	for(i = 0 ;i<Pos.XResolution*20;i++)
	{
		*((char *)addr+0)=(char)0xff;
		*((char *)addr+1)=(char)0xff;
		*((char *)addr+2)=(char)0xff;
		*((char *)addr+3)=(char)0x00;	
		addr +=1;	
	}

	color_printk(YELLOW,BLACK,"Hello\t\t World!\n");

	color_printk(YELLOW,BLACK,"\n");

	color_printk(YELLOW,BLACK,"Welcome\t\t to \t\t qianOS!\n");

	//i = 1/0;			// Make an error to trigger interruput

	while(1)
		;
}
