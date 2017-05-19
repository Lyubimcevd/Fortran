      INTEGER*2 A(26,200),S(2,200),P(2,200),TQ(6,500),TQC(4,500),NXZ(10,LPLZ0004
     14)                                                                LPLZ0005
      INTEGER*2  KDP(180)                                               LPLZ0006
      INTEGER QZ(4,500),IW(200)                                         LPLZ0007
      LOGICAL*1 ZADAN(10)/10*' '/                                       LPLZ0008
      integer*2 dummy2
      integer*4 dummy4,dummy
      open(10,file='F:\ASUIPW\tek_INF\u.dat',form='unformatted')
      open(4,file='F:\ASUIPW\tek_INF\plan.dat',form='unformatted')
      kol=0
      READ (10) NW,NU                                                   LPLZ0010
      REWIND 10                                                         LPLZ0011
         READ(4) LP,NN,NK                                               LPLZ0012
       LP1=LP+LP                                                        LPLZ0013
      READ(4) (KDP(I),I=1,LP1)                                          LPLZ0014
   1  READ(4,END=2) NXZ,(IW(I),I=1,NU),IQ,IP,ID,LD,LZ,LF,MP,MS,NA,NS,NT LPLZ0015
      IF(NA.LE.-1000) GOTO2                                             LPLZ0016
      READ(4,END=2) ((A(I,J),I=1,26),J=1,NA)                            LPLZ0017
      READ(4,END=2) ((S(I,J),I=1,2),J=1,NS),((P(I,J),I=1,2),J=1,NS)     LPLZ0018
      READ(4,END=2) ((TQ(I,J),I=1,6),J=1,NT),((QZ(I,J),I=1,4),J=1,NT)   LPLZ0019
      READ(4,END=2) ((TQC(I,J),I=1,4),J=1,NT)                           LPLZ0020
      KOL=KOL+1                                                         LPLZ0021
      GOTO1                                                             LPLZ0022
   2  open(666,file = 'tmp.txt')
      read(666,777) idata
  777 format(i6)
      read(666,778) zadan
  778 format(10a1)
      WRITE(*,3) ZADAN,IDATA,KOL                                        LPLZ0025
      pause'çàïèøè êîë-âî çàêàçîâ è íàæìè êë"ENTER"'
   3  FORMAT(///5X,'ÏPOTOKOË KOÏÈPOBAHÈß ÏËAHA ÏOCËE ÇAÄAHÈß ',10A1,10X,LPLZ0026
     *I6//1X,'KOËÈ×ECTBO ÇAKAÇOB  ',I6/////)                            LPLZ0027
      REWIND 4                                                          LPLZ0028
      END                                                               LPLZ0029
 
