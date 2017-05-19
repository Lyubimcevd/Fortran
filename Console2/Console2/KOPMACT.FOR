      INTEGER ZAP,TQ1(400)                                              MACT0005
      INTEGER*2 NNZ(100),NPZAK(100)/100*0/                              MACT0006
      INTEGER PRIZ                                                      MACT0007
      INTEGER*2 A(26,200),S(2,200),P(2,200),TQ(6,500),TQC(4,500),       MACT0008
     *U(20,100),NXZ(10,4),KDP(180)                                      MACT0009
      INTEGER*2 NZ(100,4),NM1(100),NM2(100)                             MACT0010
      INTEGER*2 ZG1(100,60)                                             MACT0011
      INTEGER F(100,3),ZG(100,60)/6000*0/                               MACT0012
      INTEGER QZ(4,500),UW(2,200),IW(100)                               MACT0013
      integer*2 dummy2
      integer*4 dummy4,dummy
      call RusConsole
      open(666,file = 'tmp.txt')
      read(666,777) idata
  777 format(i6)
      write(*,*) '‚ÍÎ˛˜Ë ÔËÌÚÂ Ë Ì‡ÊÏË ÍÎ."ENTER"'
      READ(*,777) ipust 
   44 FORMAT(//1X,'Õ≈œŒÀÕŒ“¿  Œ––≈ ÷»» Ã¿—“≈–— »’ ',5X,I6/)
    4 FORMAT(1X,'B «AKA«E',1X,4A2,1X,'MACTEPCKAﬂ',1X,I2,1X,'HECKOPP≈ “»–MACT0014
     *OBAHA ¬ Ã¿–ÿ. ¿–“≈ ',1X,I2,' Œœ-÷»ﬂ π ',I3,
     */19X,'ÿ»‘– ',1X,I6,1X,'Œ“—”“—“¬”≈“ ¬ Ã¿—“≈–— Œ… ',I2)
    5 FORMAT(I1)                                                        MACT0016
    6 FORMAT(I2,4A2,2(I2))                                              MACT0017
      NL=1                                                              MACT0018
      NL1=3                                                             MACT0019
      open(4,file='F:\ASUIPW\tek_INF\u.dat',form='unformatted')
      READ(4) NW,NU                                                     MACT0020
      READ(4) ((UW(I,J),I=1,2),J=1,NW),((U(I,J),I=1,20),J=1,NU)         MACT0021
      NUC=0                                                             MACT0022
      DO 999 J=1,NU                                                     MACT0023
      IF(U(7,J).LE.0) GOTO 999                                          MACT0024
      NUC=NUC+1                                                         MACT0025
      U(7,J)=NUC                                                        MACT0026
  999 CONTINUE                                                          MACT0027
      REWIND 4                                                          MACT0028
      OPEN(8,FILE='F:\ASUIPW\tek_INF\KOPUH.DAT',ACCESS='DIRECT',FORM=   MACT0029
     1'FORMATTED',recl=14)
      DO 7 I=1,100                                                      MACT0030
      READ(8,6,end=8) NNZ(I),(NZ(I,J),J=1,4),NM1(I),NM2(I)              MACT0031
    7 CONTINUE                                                          MACT0033
    8 N=I-1                                                             MACT0034
      open(1,file='F:\ASUIPW\tek_INF\plan.dat',form='unformatted')
      open(3,file='F:\ASUIPW\tek_INF\plan1.dat',form='unformatted')
      open(2,file='F:\ASUIPW\tek_INF\portfel.dat',form='unformatted')
      open(8,file='F:\ASUIPW\tek_INF\portfel1.dat',form='unformatted')
      open(6,file='prn')
      write(*,789)
  789 format(1x,'–¿¡Œ“¿≈“ œ–Œ√–¿ÃÃ¿')
      WRITE(6,44) IDATA
      READ(NL) LP,NN,NK                                                 MACT0036
      LP1=LP+LP                                                         MACT0037
      READ(NL) (KDP(I),I=1,LP1)                                         MACT0038
   27 READ(NL) NXZ,(IW(I),I=1,NU),IQ,IP,ID,LD,LZ,LF,MP,MS,NA,NS,NT      MACT0039
      IF(NA.LE.-1000) GOTO 28                                           MACT0040
      READ(NL) ((A(I,J),I=1,26),J=1,NA)                                 MACT0041
      READ(NL) ((S(I,J),I=1,2),J=1,NS),((P(I,J),I=1,2),J=1,NS)          MACT0042
      READ(NL) ((TQ(I,J),I=1,6),J=1,NT),((QZ(I,J),I=1,4),J=1,NT)        MACT0043
      READ(NL) ((TQC(I,J),I=1,4),J=1,NT)                                MACT0044
      GOTO 27                                                           MACT0045
   28 READ(NL) ((ZG1(I,J),I=1,NUC),J=1,LP),((F(I,J),I=1,NUC),J=1,3)     MACT0046
      REWIND NL                                                         MACT0047
      DO 44444 J=1,LP                                                   MACT0048
      DO 44444 I=1,NUC                                                  MACT0049
44444 ZG(I,J)=ZG1(I,J)*10                                               MACT0050
      READ(NL) LP,NN,NK                                                 MACT0051
      LP1=LP+LP                                                         MACT0052
      READ(NL) (KDP(I),I=1,LP1)                                         MACT0053
      WRITE(NL1) LP,NN,NK                                               MACT0054
      WRITE(NL1) (KDP(I),I=1,LP1)                                       MACT0055
   10 READ(NL) NXZ,(IW(I),I=1,NU),IQ,IP,ID,LD,LZ,LF,MP,MS,NA,NS,NT      MACT0056
      IF(NA.LT.0) GOTO 11                                               MACT0057
      WRITE(NL1) NXZ,(IW(I),I=1,NU),IQ,IP,ID,LD,LZ,LF,MP,MS,NA,NS,NT    MACT0058
   12 READ(NL) ((A(I,J),I=1,26),J=1,NA)                                 MACT0059
      READ(NL) ((S(I,J),I=1,2),J=1,NS),((P(I,J),I=1,2),J=1,NS)          MACT0060
      READ(NL) ((TQ(I,J),I=1,6),J=1,NT),((QZ(I,J),I=1,4),J=1,NT)        MACT0061
      READ(NL) ((TQC(I,J),I=1,4),J=1,NT)                                MACT0062
      ZAP=0                                                             MACT0063
      GOTO 13                                                           MACT0064
    9 NL=2                                                              MACT0065
      NL1=8                                                             MACT0066
   99 READ(NL) NXZ,(IW(I),I=1,NU),IQ,IP,ID,LZ,MP,NA,NS,NT               MACT0067
      IF(NA.LT.0) GOTO 14                                               MACT0068
      WRITE(NL1) NXZ,(IW(I),I=1,NU),IQ,IP,ID,LZ,MP,NA,NS,NT             MACT0069
      GOTO 12                                                           MACT0070
   11 DO 44445 J=1,LP                                                   MACT0071
      DO 44445 I=1,NUC                                                  MACT0072
44445 ZG1(I,J)=ZG(I,J)/10                                               MACT0073
      WRITE(NL1) NXZ,(IW(I),I=1,NU),IQ,IP,ID,LD,LZ,LF,MP,MS,NA,NS,NT    MACT0074
      WRITE(NL1) ((ZG1(I,J),I=1,NUC),J=1,LP),((F(I,J),I=1,NUC),J=1,3)   MACT0075
      REWIND NL                                                         MACT0076
      REWIND NL1                                                        MACT0077
      GOTO 9                                                            MACT0078
   14 REWIND NL                                                         MACT0079
      WRITE(NL1) NXZ,(IW(I),I=1,NU),IQ,IP,ID,LZ,MP,NA,NS,NT             MACT0080
      GOTO 15                                                           MACT0081
   13 DO 16 K=1,N                                                       MACT0082
      DO 17 L=1,4                                                       MACT0083
      IF(NXZ(L,1).NE.NZ(K,L)) GOTO 16                                   MACT0084
   17 CONTINUE                                                          MACT0085
      NPZAK(K)=1
      GOTO 18                                                           MACT0086
   18 CALL PODZAG(U,TQ,QZ,ZG,NT,LP,1,TQC)                               MACT0087
      DO 19 J=1,NT                                                      MACT0088
      IF(TQC(2,J).EQ.0) GOTO 19                                         MACT0089
      TQ1(J)=TQ(2,J)/100                                                MACT0090
      IF(NNZ(K).NE.U(1,TQ(1,J))) GOTO 19                                MACT0091
      IF(TQ1(J).NE.NM1(K)) GOTO 19                                      MACT0092
      TQ1(J)=NNZ(K)*10000+NM2(K)*100+(TQ(2,J)-TQ1(J)*100)               MACT0093
      DO 20 L=1,NW                                                      MACT0094
      IF(UW(1,L).NE.TQ1(J)) GOTO 20                                     MACT0095
      TQ(1,J)=UW(2,L)                                                   MACT0096
      TQ(2,J)=TQ1(J)-TQ1(J)/10000*10000                                 MACT0097
      GOTO 19                                                           MACT0098
   20 CONTINUE                                                          MACT0099
      IF(TQ1(J).EQ.ZAP) GOTO 19                                         MACT0100
      MKT=A(9,(A(1,(TQ(3,J)-TQ(3,J)/1000*1000))))                       OTOB0329
      NOPT=J-A(12,(A(1,(TQ(3,J)-TQ(3,J)/1000*1000))))+1
      WRITE(6,4) (NXZ(L,1),L=1,4),NM1(K),MKT,NOPT,TQ1(J),NM2(K)         MACT0101
      ZAP=TQ1(J)                                                        MACT0102
   19 CONTINUE                                                          MACT0103
      CALL PODZAG(U,TQ,QZ,ZG,NT,LP,0,TQC)                               MACT0104
   16 CONTINUE                                                          MACT0105
  100 WRITE(NL1) ((A(I,J),I=1,26),J=1,NA)                               MACT0106
      WRITE(NL1) ((S(I,J),I=1,2),J=1,NS),((P(I,J),I=1,2),J=1,NS)        MACT0107
      WRITE(NL1) ((TQ(I,J),I=1,6),J=1,NT),((QZ(I,J),I=1,4),J=1,NT)      MACT0108
      WRITE(NL1) ((TQC(I,J),I=1,4),J=1,NT)                              MACT0109
      IF(NL.EQ.1) GOTO 10                                               MACT0110
      if(nl.eq.2) goto 99
      GOTO 9                                                            MACT0111
   45 FORMAT(1X,'«¿ ¿« ',4A2,1X,' Œ“—”“—“¬”≈“ ¬ œÀ¿Õ≈ » œŒ–“‘≈À≈ ')
   15 DO 46 I=1,N
      IF(NPZAK(I).NE.0) GOTO 46
      WRITE(6,45) (NZ(I,J),J=1,4)
   46 CONTINUE
      STOP                                                              MACT0112
      END                                                               MACT0113
      SUBROUTINE PODZAG(U,TQ,QZ,ZG,NT,LP,IND,TQC)                       MACT0114
      INTEGER QZ(4,1),ZG(100,1)                                         MACT0115
      INTEGER*2 TQC(4,1)                                                MACT0116
      INTEGER*2 U(20,1),TQ(6,1)                                         MACT0117
      DO 1 J1=1,NT                                                      MACT0118
      IF(TQC(2,J1).LE.0) GOTO 1                                         MACT0119
      IF(TQ(4,J1).GT.LP.OR.TQ(5,J1).LT.0) GOTO 1                        MACT0120
      IF(TQ(5,J1).GT.LP) I2=LP+1                                        MACT0121
      IF(TQ(5,J1).LE.LP) I2=TQ(5,J1)                                    MACT0122
      I3=TQ(1,J1)                                                       MACT0123
      I4=U(7,I3)                                                        MACT0124
      IF(I4.LE.0) GOTO 1                                                MACT0125
      I1=TQ(4,J1)                                                       MACT0126
      IF(I1.LE.0) I1=1                                                  MACT0127
      IF(IND) 4,4,5                                                     MACT0128
    4 IF(I1.GE.I2) GOTO 8                                               MACT0129
      I=I2-1                                                            MACT0130
      DO 2 J=I1,I                                                       MACT0131
    2 ZG(I4,J)=ZG(I4,J)+U(2,I3)                                         MACT0132
      IF(I2.EQ.LP+1) GOTO 1                                             MACT0133
    3 ZG(I4,I2)=ZG(I4,I2)+QZ(1,J1)-U(2,I3)*(I2-I1)                      MACT0134
      GOTO 1                                                            MACT0135
    8 IF(QZ(1,J1).LE.U(2,I3)) GOTO 3                                    MACT0136
      KTR=QZ(1,J1)/LP                                                   MACT0137
      LPR=LP                                                            MACT0138
      DO 9 J=1,LPR                                                      MACT0139
    9 ZG(I4,J)=ZG(I4,J)+KTR                                             MACT0140
      GOTO 1                                                            MACT0141
    5 IF(I1.GE.I2) GOTO 7                                               MACT0142
      I=I2-1                                                            MACT0143
      DO 6 J=I1,I                                                       MACT0144
    6 ZG(I4,J)=ZG(I4,J)-U(2,I3)                                         MACT0145
      IF(I2.EQ.LP+1) GOTO 1                                             MACT0146
    7 ZG(I4,I2)=ZG(I4,I2)+U(2,I3)*(I2-I1)-QZ(1,J1)                      MACT0147
    1 CONTINUE                                                          MACT0148
      RETURN                                                            MACT0149
      END                                                               MACT0150
 
