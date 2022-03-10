# 编译顺序

make all -> Image 
  boot/bootsect 
    boot/bootsect.s   
  boot/setup
    boot/setup.s
  tools/system
    boot/head.o
      boot/head.s
    init/main.o
      init/main.c
      include/unistd.h
      include/sys/stat.h
      include/sys/types.h
      include/sys/times.h
      include/sys/utsname.h
      include/utime.h
      include/time.h
      include/linux/tty.h
      include/termios.h
      include/linux/sched.h
      include/linux/head.h
      include/linux/fs.h
      include/linux/mm.h
      include/signal.h
      include/asm/system.h
      include/asm/io.h
      include/stddef.h
      include/stdarg.h
    kernel/kernel.o
    mm/mm.o
    fs/fs.o
    kernel/blk_drv/blk_drv.a
    kernel/chr_drv/chr_drv.a
    kernel/math/math.a
    lib/lib.a    
  tools/build
    tools/build.c



# 启动顺序

bootsect.s -> setup.s -> head.s

