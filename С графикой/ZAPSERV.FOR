      INCLUDE  'FGRAPH.FI'
      INCLUDE  'FGRAPH.FD'    
      character*1 pole1(103),phep(256),pbaz(19),pole2(103),pr,uh(2)
      record / rccoord / curpos
      integer*2 dummy2
      integer*4 dummy4,dummy
      character*1 zak(8),zakaz(8),zak9(9)/9*'9'/,zakpr(92)/92*' '/
      character*1 iuhb(2),ntr(5),ntrb(5),iprb
      character*10 ifam,ifamb
   9  format(i3,8a1,i8,a10,a1,2a1,5a1)
  13  format(103a1)
      open(1,file='f:\asyip\omoih.dat',recl=103,access='direct',
     *form='formatted')
      open(2,file='d:\asyip\omoih.dat',recl=103,access='direct',
     *form='formatted')
      open(3,file='d:\asyip\hep.dat',recl=256,access=
     *'direct')
c     open(4,file='f:\asyip\baz.dat')
      open(5,file='f:\asyip\baz1.dat')
      dummy=setvideomode( $ERESCOLOR )
      dummy=remappalette( 0,$BLUE )
      call clearscreen( $GCLEARSCREEN )
      dummy2=settextcolor(2)
c      call settextposition( 10,25, curpos ) 
c      call outtext( 'задай дату(ччммгггг)' )
c      read(*,777) idat
c  777 format(i8)
      idat=0
      call settextposition( 11,1, curpos ) 
      call outtext( 'задай фамилию,вводившего данные по заказу(10знаков
     1)' )
      read(*,779) ifam
  779 format(a10)
C      call settextposition( 12,25, curpos ) 
C      call outtext( 'задай номер цеха(nnn) ' )
C      read(*,778) NZZ
C  778 format(i3)
      NZZ=117
      nk1=1
      nk2=1
      nk3=1
      nk4=1
      read(2,13,rec=nk2,end=2) pole2
      read(3,rec=nk3,end=23) phep
   23 do 1 i=1,8
      if(pole2(i+1).eq.phep(i)) goto 1
      goto 3
    1 continue
      pause'заказ неисправлен и не может быть записан на сервер       
     1                      для продолжения нажмите кл"ENTER"'
      goto 4
   3  read(5,19,end=33) (zak(i),i=1,8)
  19  format(3x,8a1,26x)
      do 20 i=1,8
      if(pole2(i+1).eq.zak(i)) goto 20
      goto 3
  20  continue    
      pause'заказ не может быть повторно записан на сервер
     1                      для продолжения нажмите кл"ENTER"'
      goto 4
  33  do 10 i=1,8
  10  zakaz(i)=pole2(i+1) 
      pr=pole2(43)
      uh(1)=pole2(79)
      uh(2)=pole2(80)
      do 30 i=1,5
  30  ntr(i)=pole2(i+90)
  11  read(1,13,rec=nk1,end=6) pole1
      nk1=nk1+1
      goto 11
    6 if(nk1.eq.1) goto 16
      write(1,13,rec=nk1) zak9,zakpr
      nk1=nk1+1
   16 write(1,13,rec=nk1) pole2
      nk1=nk1+1
      nk2=nk2+1
      read(2,13,rec=nk2,end=2) pole2
      goto 16
   2  close (1)
      close (5)
      open(5,file='f:\asyip\baz1.dat')
   7  read(5,9,end=8) nzzb,(zak(i),i=1,8),idatb,ifamb,iprb,iuhb,ntrb
c      write(5,9) nzzb,(zak(i),i=1,8),idatb,ifamb,iprb,iuhb,ntrb
      goto 7
   8  backspace 5
      write(5,9) nzz,(zakaz(i),i=1,8),idat,ifam,pr,uh,ntr
c     end file 4
      close (5)
    4 DUMMY=SETVIDEOMODE($DEFAULTMODE)
      end
  
