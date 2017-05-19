      INTEGER*2 A(26,200),S(2,200),P(2,200),TQ(6,500),TQC(4,500),NXZ(10,SPPF0004
     14)                                                                SPPF0005
      INTEGER QZ(4,500),IW(200),UW(2,200)                               SPPF0006
      INTEGER*2 ZAK(4,50),MTK(50),NOP(50),CO/'KO'/,U(20,100)            SPPF0007
      INTEGER*2 TQC21(50),NZ10/'10'/,NZ15/'15'/                         SPPF0008
      INTEGER QZ21(50),KOLZ(50)                                         SPPF0009
      INTEGER NQZ(2,50),NTR(50),CBZ(50)                                 SPPF0010
      INTEGER*2 PR(50),KLAS(10,50),PRD(50)                              SPPF0011
      INTEGER*2 ZAK1(5,50),PROB/'  '/,KOEF(78)                          SPPF0012
      character*4 zakazo/'2046'/
      character*5 zakazs/'20407'/
      character*4 zakaz
      character*5 zakaz1
      equivalence(nxz(1,1),zakaz)
      equivalence(nxz(1,1),zakaz1)
   10 FORMAT(4A2,2I2,2I5,I1,5A2,4I5,I2)                                 SPPF0013
    3 FORMAT(I8)                                                        SPPF0014
      integer*2 dummy2
      integer*4 dummy4,dummy
      iprt=0
      iprhz=0
      iprrz=0
      call RusConsole
      open(666,file='tmp.txt')
      read(666,777) iprt
  777 format(i4)
      read(666,777) iprt1
      read(666,778) iprhz
  778 format(i2)
      read(666,778) iprrz
      KOL=0
      open(10,file='F:\ASUIPW\tek_INF\u.dat',form='unformatted')
      open(unit=4,file='F:\ASUIPW\tek_INF\portfel.dat',
     1form='unformatted')
      open(unit=1,file='F:\ASUIPW\tek_INF\portfel1.dat',
     1form='unformatted')
      READ (10) NW,NU                                                   SPPF0018
      READ (10) ((UW(I,J),I=1,2),J=1,NW),((U(I,J),I=1,20),J=1,NU)       SPPF0019
      REWIND 10                                                         SPPF0020
   1  READ(4) NXZ,(IW(I),I=1,NU),IQ,IP,ID,LZ,MP,NA,NS,NT                SPPF0021
      IF(NA.LE.-1000) GOTO2                                             SPPF0022
      READ(4) ((A(I,J),I=1,26),J=1,NA)                                  SPPF0023
      READ(4) ((S(I,J),I=1,2),J=1,NS),((P(I,J),I=1,2),J=1,NS)           SPPF0024
      READ(4) ((TQ(I,J),I=1,6),J=1,NT),((QZ(I,J),I=1,4),J=1,NT)         SPPF0025
      READ(4) ((TQC(I,J),I=1,4),J=1,NT)                                 SPPF0026
      if(zakaz.eq.zakazo) goto 35
      if(zakaz1.eq.zakazs) goto 35
      DO 50 I=2,NT                                                      SPPF0027
      if(TQC(2,I).EQ.0.AND.TQC(3,I).NE.0) GOTO 50
      if(QZ(2,I).EQ.0) GOTO 50
      if(TQ(2,I)-TQ(2,I)/100*100.NE.81.AND.TQ(2,I)-TQ(2,I)/100*100.NE.
     182.AND.TQ(2,I)-TQ(2,I)/100*100.NE.83) GOTO 60
      IF(IPRT.EQ.0) GOTO 50
      QZ(2,I)=QZ(2,I)+(QZ(2,I)*iprt+5000)/10000                         SPPF0028
      GOTO 50
  60  IF(IPRT1.EQ.0) GOTO 50
      QZ(2,I)=QZ(2,I)+(QZ(2,I)*IPRT1+5000)/10000
  50  CONTINUE                                                          SPPF0032
      GOTO 30
  35  DO 51 I=2,NT
      IF(TQC(2,I).EQ.0.AND.TQC(3,I).NE.0) GOTO 51
      if(iprhz.eq.0) goto 52
      IF(QZ(1,I).LT.1) GOTO 52
      QZ(1,I)=QZ(1,I)-(QZ(1,I)*iprhz+50)/100
 52   if(iprrz.eq.0) goto 51
      if(QZ(2,I).EQ.0) GOTO 51
      QZ(2,I)=QZ(2,I)+(QZ(2,I)*iprrz+50)/100
  51  CONTINUE
  30  WRITE(1) NXZ,(IW(I),I=1,NU),IQ,IP,ID,LZ,MP,NA,NS,NT               SPPF0033
      WRITE(1) ((A(I,J),I=1,26),J=1,NA)                                 SPPF0034
      WRITE(1) ((S(I,J),I=1,2),J=1,NS),((P(I,J),I=1,2),J=1,NS)          SPPF0035
      WRITE(1) ((TQ(I,J),I=1,6),J=1,NT),((QZ(I,J),I=1,4),J=1,NT)        SPPF0036
      WRITE(1) ((TQC(I,J),I=1,4),J=1,NT)                                SPPF0037
      KOL=KOL+1                                                         SPPF0038
      GOTO1                                                             SPPF0039
   2  WRITE(1) NXZ,(IW(I),I=1,NU),IQ,IP,ID,LZ,MP,NA,NS,NT               SPPF0040
      WRITE(*,5) KOL                                                    SPPF0041
    5 FORMAT(1X,'KOÀ-BO «AKA«OB',I6)                                    SPPF0042
      pause'Õ¿∆Ã»  À."ENTER"'
      REWIND 4                                                          SPPF0043
      REWIND 1                                                          SPPF0044
      END                                                               SPPF0045
