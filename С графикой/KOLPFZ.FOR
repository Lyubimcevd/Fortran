      INCLUDE  'FGRAPH.FI'
      INCLUDE  'FGRAPH.FD'
      INTEGER*2 A(26,200),S(2,200),P(2,200),TQ(6,500),TQC(4,500),NXZ(10,LPLZ0004
     14)                                                                LPLZ0005
      INTEGER QZ(4,500),IW(200)                                         LPLZ0006
      LOGICAL*1 ZADAN(10)/10*' '/                                       LPLZ0007
      record / rccoord / curpos
      integer*2 dummy2
      integer*4 dummy4,dummy
      open(10,file='f:\asyip\u.dat',form='unformatted')
      open(4,file='f:\asyip\portfel.dat',form='unformatted')
c     write(*,20)
c  20  format(1x,'идет подсчет заказов')
      KOL=0                                                             LPLZ0008
      READ (10) NW,NU                                                   LPLZ0009
      REWIND 10                                                         LPLZ0010
   1  READ(4,END=2) NXZ,(IW(I),I=1,NU),IQ,IP,ID,LZ,MP,NA,NS,NT          LPLZ0011
      IF(NA.LE.-1000) GOTO2                                             LPLZ0012
      READ(4,END=2) ((A(I,J),I=1,26),J=1,NA)                            LPLZ0013
      READ(4,END=2) ((S(I,J),I=1,2),J=1,NS),((P(I,J),I=1,2),J=1,NS)     LPLZ0014
      READ(4,END=2) ((TQ(I,J),I=1,6),J=1,NT),((QZ(I,J),I=1,4),J=1,NT)   LPLZ0015
      READ(4,END=2) ((TQC(I,J),I=1,4),J=1,NT)                           LPLZ0016
      KOL=KOL+1                                                         LPLZ0017
      GOTO1                                                             LPLZ0018
   2  dummy=setvideomode( $ERESCOLOR )
      dummy=remappalette( 0,$BLUE )
      call clearscreen( $GCLEARSCREEN )
      dummy2=settextcolor(2)
      call settextposition( 10,25, curpos ) 
      call outtext( 'задай дату(ччммгг)' )
      read(*,777) idata
  777 format(i6)
      dummy2=settextcolor(2)
      call settextposition( 11,25, curpos ) 
      call outtext( 'задай имя работы(10знаков)' )
      read(*,778) zadan
  778 format(10a1)
      WRITE(*,3) ZADAN,IDATA,KOL                                        LPLZ0021
   3  FORMAT(///5X,'ПPOTOKOЛ KOПИPOBAHИЯ ПOPTФEЛЯ ПOCЛE ЗAДAHИЯ ',10A1, LPLZ0022
     *10X,I6//1X,'KOЛИЧECTBO ЗAKAЗOB  ',I6/////)                        LPLZ0023
      pause'запиши кол-во заказов и нажми кл"ENTER"'
      REWIND 4                                                          LPLZ0024
      DUMMY=SETVIDEOMODE($DEFAULTMODE)
      END                                                               LPLZ0025
