      character*1 pole1(103),phep(256),pbaz(19),pole2(103),pr,uh(2)
      integer*2 dummy2
      integer*4 dummy4,dummy
      character*1 zak(8),zakaz(8),zak9(9)/9*'9'/,zakpr(92)/92*' '/
      character*1 iuhb(2),ntr(5),ntrb(5),iprb
      character*10 ifam,ifamb
   9  format(i3,8a1,i8,a10,a1,2a1,5a1)
  13  format(103a1)
      open(1,file='f:\rar\omoih.dat',recl=103,access='direct',
     *form='formatted')
      open(2,file='d:\asyip\omoih.dat',recl=103,access='direct',
     *form='formatted')
      open(3,file='d:\asyip\hep.dat',recl=256,access=
     *'direct')
      open(5,file='f:\rar\baz1.dat')
      idat=0
      open(666,file='tmp.txt')
      read(666,779) ifam
  779 format(a10) 
      read(666,778) NZZ
  778 format(i3)
      nk1=1
      nk2=1
      nk3=1
      nk4=1
      read(2,13,end=2) pole2
      read(3,end=23) phep
   23 do 1 i=1,8
      if(pole2(i+1).eq.phep(i)) goto 1
      goto 3
    1 continue
      pause'заказ неисправлен и не может быть записан на сервер       
     1                      для продолжения нажмите кл"ENTER"'
      goto 4
   3  do 10 i=1,8
  10  zakaz(i)=pole2(i+1) 
      pr=pole2(43)
      uh(1)=pole2(79)
      uh(2)=pole2(80)
      do 30 i=1,5
  30  ntr(i)=pole2(i+90)
  11  read(1,13,end=6) pole1
      nk1=nk1+1
      goto 11
    6 if(nk1.eq.1) goto 16
      write(1,13,rec=nk1) zak9,zakpr
      nk1=nk1+1
   16 write(1,13,rec=nk1) pole2
      nk1=nk1+1
      nk2=nk2+1
      read(2,13,end=2) pole2
      goto 16
   2  close (1)
   7  read(5,9,end=8) nzzb,(zak(i),i=1,8),idatb,ifamb,iprb,iuhb,ntrb
      goto 7
   8  backspace 5
      write(5,9) nzz,(zakaz(i),i=1,8),idat,ifam,pr,uh,ntr
      close (5)
   4   end
  
 
