      INCLUDE  'FGRAPH.FI'
      INCLUDE  'FGRAPH.FD'
      INTEGER*2 A(26,200),S(2,200),P(2,200),TQ(6,500),TQC(4,500),NXZ(10,SPPL0004
     14)                                                                SPPL0005
      INTEGER*2 KONST,NZ10/'10'/,NZ15/'15'/                             SPPL0006
c      INTEGER KOL(50)                                                   SPPL0007
      INTEGER*2 U(20,100)                                               SPPL0008
      INTEGER UW(2,200)                                                 SPPL0009
      INTEGER*2  KDP(180)                                               SPPL0010
      INTEGER F(100,3),f1(100.3)                                                  SPPL0011
      INTEGER*2 ZG(100,60),KOEF(78)                                     SPPL0012
      INTEGER QZ(4,500),IW(200)                                         SPPL0013
      INTEGER*2 ZAK(4,100),MTK(50),NOP(50),CO/'KO'/,TQC21(50),PR(50),    SPPL0014
     1KLAS(10,50),PRD(50)                                               SPPL0015
      INTEGER NQZ(2,50),QZ21(50)                                        SPPL0016
      INTEGER*2 ZAK1(5,50),PROB/'  '/                                   SPPL0017
      INTEGER CBZ(50),NTR(50)                                           SPPL0018
      character*4 zakazo/'2046'/
      character*4 zakaz
      character*5 zakaz1
      character*5 zakazs/'20407'/,zakazs1/'20408'/,zakazs2/'20455'/      
      equivalence(nxz(1,1),zakaz),(nxz(1,1),zakaz1)
    3 FORMAT(I8)                                                        SPPL0020
      record / rccoord / curpos
      integer*2 dummy2
      integer*4 dummy4,dummy
      iprtsl=0
      iprtst=0
      iprhz=0
      iprrz=0
      dummy=setvideomode( $ERESCOLOR )
      dummy=remappalette( 0,$BLUE )
      call clearscreen( $GCLEARSCREEN )
      dummy2=settextcolor(2)
      call settextposition( 10,5, curpos ) 
      call outtext( 'задай процент изменения тарифа по слесарям ИП(xxxx)
     1' )
      read(*,777) iprtsl
  777 format(i4)
      dummy2=settextcolor(2)
      call settextposition( 11,5, curpos ) 
      call outtext( 'задай процент изменения тарифа по станочникам ИП(xx
     1xx)' )
      read(*,777) iprtst
c      dummy2=settextcolor(2)
c      call settextposition( 12,5, curpos ) 
c      call outtext( 'задай процент уменьшения н/ч по ЗАО(xx)' )
c      read(*,778) iprhz
c  778 format(i2)
c      dummy2=settextcolor(2)
c      call settextposition( 13,5, curpos ) 
c      call outtext( 'задай процент увеличения расценка по ЗАО(xx)' )
c      read(*,778) iprrz
      KOL=0
      open(10,file='f:\asyip\u.dat',form='unformatted')
      open(unit=4,file='f:\asyip\plan.dat',form='unformatted')
      open(unit=1,file='f:\asyip\plan1.dat',form='unformatted')
      open(3,file='f:\asyip\stzak.dat')
   10 FORMAT(4A2)
c      READ 100,(KOEF(I),I=1,78)                                         SPPL0021
c  100 FORMAT(26I3,2X)                                                   SPPL0022
      IA=1                                                              LDPL0026
   11 READ (3,10,END=18) (ZAK(I,IA),I=1,4)
      IA=IA+1                                                           LDPL0031
      GOTO 11                                                           LDPL0032
  18  IA=IA-1                                                           LDPL0033
      READ (10) NW,NU                                                   SPPL0023
      READ (10) ((UW(I,J),I=1,2),J=1,NW),((U(I,J),I=1,20),J=1,NU)       SPPL0024
      REWIND 10                                                         SPPL0025
      DO 9991 J=1,NU                                                    SPPL0027
      IF(U(7,J).LE.0) GOTO 9991                                         SPPL0028
      NUC=NUC+1                                                         SPPL0029
 9991 CONTINUE                                                          SPPL0030
         READ(4) LP,NN,NK                                               SPPL0031
       LP1=LP+LP                                                        SPPL0032
       READ(4) (KDP(I),I=1,LP1)                                         SPPL0033
         WRITE(1) LP,NN,NK                                              SPPL0034
       WRITE(1) (KDP(I),I=1,LP1)                                        SPPL0035
   1  READ(4) NXZ,(IW(I),I=1,NU),IQ,IP,ID,LD,LZ,LF,MP,MS,NA,NS,NT       SPPL0036
      IF(NA.LE.-1000) GOTO2                                             SPPL0037
      READ(4) ((A(I,J),I=1,26),J=1,NA)                                  SPPL0038
      READ(4) ((S(I,J),I=1,2),J=1,NS),((P(I,J),I=1,2),J=1,NS)           SPPL0039
      READ(4) ((TQ(I,J),I=1,6),J=1,NT),((QZ(I,J),I=1,4),J=1,NT)         SPPL0040
      READ(4) ((TQC(I,J),I=1,4),J=1,NT)                                 SPPL0041
C      N=A(10,1)                                                         SPPL0042
C      N=S(1,N)                                                          SPPL0043
C      N=A(12,N)+A(13,N)-1                                               SPPL0044
C      NM=TQ(2,N)/100                                                    SPPL0045
C      N=TQ(1,N)                                                         SPPL0046
C      NZ=U(1,N)                                                         SPPL0047
      if(zakaz.eq.zakazo) goto 35
      if(zakaz1.ne.zakazs.and.zakaz1.ne.zakazs1.and.zakaz1.ne.zakazs2)
     1goto 22
      DO 13 KI=1,IA                                                     LDPL0049
      DO 14 I=1,4                                                       LDPL0050
      IF(NXZ(I,1).NE.ZAK(I,KI)) GOTO 13                                 LDPL0051
  14  CONTINUE                                                          LDPL0052
      goto 30
  13  continue
  22  DO 50 I=2,NT                                                      SPPF0027
      if(TQC(2,I).EQ.0.AND.TQC(3,I).NE.0) GOTO 50
      if(iprtsl.eq.0.and.iprtst.eq.0) goto 50
      if(QZ(2,I).EQ.0) GOTO 50
      if((TQ(2,I)-TQ(2,I)/100*100).EQ.81.OR.(TQ(2,I)-TQ(2,I)/100*100).EQ
     1.82.OR.(TQ(2,I)-TQ(2,I)/100*100).EQ.83)
     2QZ(2,I)=QZ(2,I)+(QZ(2,I)*(iprtsl/100)+50)/100                                  SPPF0028
      if((TQ(2,I)-TQ(2,I)/100*100).NE.81.AND.(TQ(2,I)-TQ(2,I)/100*100).
     1NE.82.AND.(TQ(2,I)-TQ(2,I)/100*100).NE.83)
     2QZ(2,I)=QZ(2,I)+(QZ(2,I)*(iprtst/100)+50)/100
  50  CONTINUE                                                          SPPF0032
      GOTO 30
  35  DO 51 I=2,NT
      IF(TQC(2,I).EQ.0.AND.TQC(3,I).NE.0) GOTO 51
      if(iprhz.eq.0) goto 52
      IF(QZ(1,I).LE.1) GOTO 52
      QZ(1,I)=QZ(1,I)-(QZ(1,I)*iprhz+50)/100
  52  if(iprrz.eq.0) goto 51
      if(QZ(2,I).EQ.0) GOTO 51
      QZ(2,I)=QZ(2,I)+(QZ(2,I)*iprrz+50)/100
  51  CONTINUE 
  30  WRITE(1) NXZ,(IW(I),I=1,NU),IQ,IP,ID,LD,LZ,LF,MP,MS,NA,NS,NT      SPPL0054
      WRITE(1) ((A(I,J),I=1,26),J=1,NA)                                 SPPL0055
      WRITE(1) ((S(I,J),I=1,2),J=1,NS),((P(I,J),I=1,2),J=1,NS)          SPPL0056
      WRITE(1) ((TQ(I,J),I=1,6),J=1,NT),((QZ(I,J),I=1,4),J=1,NT)        SPPL0057
      WRITE(1) ((TQC(I,J),I=1,4),J=1,NT)                                SPPL0058
      KOL=KOL+1
      GOTO1                                                             SPPL0059
   2  WRITE(1) NXZ,(IW(I),I=1,NU),IQ,IP,ID,LD,LZ,LF,MP,MS,NA,NS,NT      SPPL0060
      READ(4) ((ZG(I,J),I=1,NUC),J=1,LP),((F(I,J),I=1,NUC),J=1,3)       SPPL0061
      WRITE(1) ((ZG(I,J),I=1,NUC),J=1,LP),((F(I,J),I=1,NUC),J=1,3)      SPPL0062
      WRITE(*,5) KOL                                                    SPPF0041
    5 FORMAT(1X,'KOЛ-BO ЗAKAЗOB',I6)                                    SPPF0042
      pause'НАЖМИ КЛ."ENTER"'
      REWIND 4                                                          SPPL0063
      REWIND 1                                                          SPPL0064
      DUMMY=SETVIDEOMODE($DEFAULTMODE)
      END                                                               SPPL0065
