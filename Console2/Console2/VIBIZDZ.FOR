      INTEGER*2 A(26,200),S(2,200),P(2,200),TQ(6,500),TQC(4,500),NXZ(10,IZDZ0001
     14),F(10)/'0 ','1 ','2 ','3 ','4 ','5 ','6 ','7 ','8 ','9 '/,Z/' �'IZDZ0002
     2/,CO/'FF'/,U(20,100)                                              IZDZ0003
      INTEGER*2 CP/'��'/,Z10/'10'/,Z15/'15'/                            IZDZ0004
      INTEGER*2 ZD/'*'/,hiz(80)/80*' '/                                 IZDZ0005
      INTEGER*2 M11/'11'/,M12/'12'/                                     IZDZ0006
      INTEGER*2 CTM,GR(1100),KLASIN(2,1100),PROZ(1100),PROZS(1100)      IZDZ0007
      INTEGER*2 PROBEL/'  '/,KDPG(500),MES,DAY                          IZDZ0008
      INTEGER DATAV                                                     IZDZ0009
      INTEGER*2 PR1/'��'/,PR2/'��'/,PR3/'��'/,PR4/'��'/,PR5/'��'/       IZDZ0010
      INTEGER*2 PR6/'� '/,PR7/'� '/,PR8/'��'/                           IZDZ0011
      INTEGER*2 KDP(180),nom4(10),KLD(1)                                       IZDZ0012
      INTEGER*2 ZAK(4),NAIM(5),KLAS(10),MAS,PRIZ,NZZ(2)/'  ','  '/      IZDZ0013
      INTEGER*2 PR,PRR,KOH1/'CO'/,GRK,GRPR/'99'/,GRZ/'  '/,NZZM(2)      IZDZ0014
      INTEGER QZ(4,500),IW(100),KOT(13),BOT(6),UW(2,200)                IZDZ0015
      INTEGER*2 P40/'40'/,IC(2,40)/80*' '/,NOMZ,XVOST2,D2/'  '/,ZZZ(2)  IZDZ0016
      LOGICAL*1 LN(9)/9*' '/,XVOST(2),IZD(2),TEX(2)                     IZDZ0017
      LOGICAL*1 D/' '/,D0/'0'/,D1(4)/'0',' ',' ',' '/                   IZDZ0018
      LOGICAL*1 MPZAK                                                   IZDZ0019
      equivalence (NOMZ,XVOST(1)),(XVOST(2),XVOST2)                     IZDZ0020
      EQUIVALENCE (ZZZ(2),IZD(1)),(ZZZ(1),TEX(1))                       IZDZ0022
      integer*2 dummy2
      integer*4 dummy4,dummy
      DATA NF/1/                                                        IZDZ0024
   88 FORMAT (2A2)                                                      IZDZ0025
   80 FORMAT (80A2)                                                     IZDZ0026
   89 FORMAT (A2)                                                       IZDZ0027
  900 FORMAT (3A2)                                                      IZDZ0028
      open(14,file='F:\ASUIPW\tek_INF\klasin.dat')
      open(4,file='F:\ASUIPW\tek_INF\u.dat',form='unformatted')
      open(unit=2,file='F:\ASUIPW\tek_INF\plan.dat',form='unformatted')
      open(unit=1,file='F:\ASUIPW\tek_INF\portfel.dat',
     1form='unformatted')
      open(8,file='F:\ASUIPW\tek_INF\nakzai.dat')
      open(3,file='F:\ASUIPW\tek_INF\porzex.dat')
      DO 901 J=1,1100                                                   IZDZ0029
      READ(14,900) GR(J),(KLASIN(I,J),I=1,2)                            IZDZ0030
      IF(GR(J).EQ.KOH1) GOTO 902                                        IZDZ0031
  901 CONTINUE                                                          IZDZ0032
  902 KKLAS=J-1                                                         IZDZ0033
      NK=1 
      open(666,file='tmp.txt')                                          IZDZ0034
      read(666,80) ic
      read(666,88) nzz(1),nzz(2)
      read(666,89) grz
      READ(4) NW,NU                                                     IZDZ0038
      READ(4) ((UW(I,J),I=1,2),J=1,NW),((U(I,J),I=1,20),J=1,NU)         IZDZ0039
      READ(2) LP,NN,KK                                                  IZDZ0040
      LP1=2*LP                                                          IZDZ0041
      READ(2) (KDP(I),I=1,LP1)                                          IZDZ0042
      NKP=KK                                                            IZDZ0043
      CALL KALDNI(NKP,0,KLD)                                            IZDZ0044
      NKP=KLD(1)/10                                                     IZDZ0045
      DO 200 I1=LP,LP1                                                  IZDZ0046
      IF(NKP.EQ.KDP(I1)/10) GOTO  201                                   IZDZ0047
  200 CONTINUE                                                          IZDZ0048
  201 NKP=I1-LP                                                         IZDZ0049
      CALL KALDNI(NN,250,KDPG)                                          IZDZ0050
      LP=250                                                            IZDZ0051
      MES=NN/100-NN/10000*100                                           IZDZ0052
      DAY=NN-NN/100*100                                                 IZDZ0053
   60 IF(NF.EQ.1) GOTO 1                                                IZDZ0054
      READ(NF) NXZ,(IW(I),I=1,NU),IQ,IP,ID,LD,LZ,LF,MP,MS,NA,NS,NT      IZDZ0055
      IF(NA.LE.-1000) GOTO 61                                           IZDZ0056
      GOTO 62                                                           IZDZ0057
    1 READ(NF) NXZ,(IW(I),I=1,NU),IQ,IP,ID,LZ,MP,NA,NS,NT               IZDZ0058
      IF(NA.LT.0) GOTO 20                                               IZDZ0059
   62 READ(NF) ((A(I,J),I=1,26),J=1,NA)                                 IZDZ0060
      READ(NF) ((S(I,J),I=1,2),J=1,NS),((P(I,J),I=1,2),J=1,NS)          IZDZ0061
      READ(NF) ((TQ(I,J),I=1,6),J=1,NT),((QZ(I,J),I=1,4),J=1,NT)        IZDZ0062
      READ(NF) ((TQC(I,J),I=1,4),J=1,NT)                                IZDZ0063
      IF(IP.EQ.9) GOTO 60
      IF(PROBEL.EQ.NZZ(1).AND.PROBEL.EQ.NZZ(2)) GOTO 6000               IZDZ0064
      IF(NZZ(2).NE.PROBEL) GOTO 6666                                    IZDZ0065
      IF(NXZ(10,4).NE.NZZ(1)) GOTO 60                                   IZDZ0066
      GOTO 6000                                                         IZDZ0067
 6666 IF(NXZ(8,4).NE.NZZ(1).OR.NXZ(9,4).NE.NZZ(2)) GOTO 60              IZDZ0068
 6000 IF(PROBEL.EQ.IC(1,1).AND.PROBEL.EQ.IC(2,1)) GOTO 82               IZDZ0069
      XVOST2=D2                                                         IZDZ0070
      ZZZ(1)=NXZ(1,1)                                                   IZDZ0071
      ZZZ(2)=NXZ(2,1)                                                   IZDZ0072
      IZD(2)=D                                                          IZDZ0073
      NOMZ=NXZ(4,1)                                                     IZDZ0074
      IF(XVOST2.EQ.D2) IZD(1)=D                                         IZDZ0075
      DO 81 I=1,40                                                      IZDZ0076
      IF(IC(1,I).EQ.ZZZ(1).AND.IC(2,I).EQ.ZZZ(2)) GOTO 82               IZDZ0077
   81 CONTINUE                                                          IZDZ0078
      GOTO 60                                                           IZDZ0079
   82 LFF=LF                                                            IZDZ0080
      CTM=0                                                             IZDZ0081
      NKT=0                                                             IZDZ0082
      DO I=1,6
      BOT(I)=0
      ENDDO
      DO 35 I=2,NT                                                      IZDZ0083
      IF(TQC(2,I).EQ.0) GOTO 35                                         IZDZ0084
      NKT=NKT+QZ(1,I)                                                   IZDZ0085
      IF(U(7,TQ(1,I)).EQ.0) GOTO 35                                     OTPL0084
      BOT(U(8,TQ(1,I)))=BOT(U(8,TQ(1,I)))+QZ(1,I)                       OTPL0085
   35 CONTINUE                                                          IZDZ0086
      N=A(10,1)                                                         IZDZ0087
      N=S(1,N)                                                          IZDZ0088
      N=A(12,N)+A(13,N)-1                                               IZDZ0089
      NM=TQ(2,N)/100                                                    IZDZ0090
      N=TQ(1,N)                                                         IZDZ0091
      NZ=U(1,N)                                                         IZDZ0092
      IF(NZ.NE.10.AND.NZ.NE.15) GOTO 60                                 IZDZ0093
      IF(NM.NE.13) GOTO 30                                              IZDZ0094
      IF(NT.EQ.2) GOTO 30                                               IZDZ0095
      IF(NT.EQ.3.AND.TQ(2,2)/100.EQ.11) NXZ(10,3)=M11                   IZDZ0096
      IF(NT.EQ.3.AND.TQ(2,2)/100.EQ.12) NXZ(10,3)=M12                   IZDZ0097
      DO 50 I=3,NT                                                      IZDZ0098
      IF(TQ(2,I).EQ.1150) GOTO 50                                       IZDZ0099
      IF(TQ(2,I)/100.EQ.11) GOTO 51                                     IZDZ0100
   50 CONTINUE                                                          IZDZ0101
      NXZ(9,3)=M12                                                      IZDZ0102
      CTM=2                                                             IZDZ0103
      GOTO 30                                                           IZDZ0104
   51 NXZ(9,3)=M11                                                      IZDZ0105
      CTM=1                                                             IZDZ0106
   30 CONTINUE                                                          IZDZ0107
      IF(IQ.EQ.0) GOTO 100                                              IZDZ0108
      IF(NXZ(5,1).EQ.CP) GOTO 100                                       IZDZ0109
      IF(TQC(1,1).LT.0) TQC(2,1)=0                                      IZDZ0110
      KT=KT-(KT*(QZ(2,1)+TQC(2,1))*100/IQ+50)/100                       IZDZ0111
      ID=ID-(ID*(QZ(2,1)+TQC(2,1))*10/IQ+5)/10                          IZDZ0112
      IQ=IQ-QZ(2,1)-TQC(2,1)                                            IZDZ0113
      IF(KT.LE.0) KT=0                                                  IZDZ0114
      IF(ID.LE.0) ID=0                                                  IZDZ0115
      IF(IQ.LE.0) IQ=0                                                  IZDZ0116
  100 CONTINUE                                                          IZDZ0117
      IF(LD-LD/1000*1000.LE.127) LD=LD/10                               IZDZ0118
      IF(NF.EQ.1) LD=0                                                  IZDZ0119
      IF(NF.EQ.1) DATAV=0                                               IZDZ0120
      IF(NF.EQ.1) GOTO 203                                              IZDZ0121
      LF=LF+LP                                                          IZDZ0122
      IF(LF.GT.LP+LP) DATAV=0                                           IZDZ0123
      IF(LF.GT.LP+LP) GOTO 203                                          IZDZ0124
      IDATA=DAY                                                         IZDZ0125
      LF=KDPG(LF)/10                                                    IZDZ0126
      IF(LF-LF/100*100.LT.MES) IDATA=IDATA+1                            IZDZ0127
      IF(LF-LF/100*100.EQ.MES.AND.LFF.GT.25) IDATA=DATA+1               IZDZ0128
      DATAV=LF*100+IDATA                                                IZDZ0129
  203 NXZ(5,1)=PROBEL                                                   IZDZ0130
      NXZ(6,1)=PROBEL                                                   IZDZ0131
      IF(NF.EQ.1) NXZ(5,1)=PR4                                          IZDZ0132
      IF(NF.EQ.1) GOTO 204                                              IZDZ0133
      IF(LFF.LE.NKP) NXZ(5,1)=PR1                                       IZDZ0134
      IF(LFF.LE.NKP) GOTO 204                                           IZDZ0135
      IF(NZ.EQ.15) GOTO 205                                             IZDZ0136
      IF(TQ(2,2).NE.1512.AND.TQ(2,2).NE.1513) NXZ(5,1)=PR2              IZDZ0137
      IF(TQ(2,2).NE.1512.AND.TQ(2,2).NE.1513) GOTO 204                  IZDZ0138
      IF(TQC(2,2).LE.0.AND.TQC(1,2).LT.0) NXZ(5,1)=PR2                  IZDZ0139
      IF(TQC(2,2).LE.0.AND.TQC(1,2).LT.0) GOTO 204                      IZDZ0140
      IF(TQC(2,2).GT.0) NXZ(5,1)=PR3                                    IZDZ0141
      IF(TQC(2,2).GT.0) GOTO 204                                        IZDZ0142
      KSCP=0                                                            IZDZ0143
      DO 206 J=3,NT                                                     IZDZ0144
      IF(TQC(2,J).EQ.0.AND.TQC(1,J).LE.0) GOTO 206                      IZDZ0145
      IF(TQ(4,J).LE.LP1/2) KSCP=1                                       IZDZ0146
  206 CONTINUE                                                          IZDZ0147
      IF(KSCP.EQ.0) NXZ(5,1)=PR3                                        IZDZ0148
      IF(KSCP.EQ.1) NXZ(5,1)=PR2                                        IZDZ0149
      GOTO 204                                                          IZDZ0150
  205 IF(TQ(2,NT).NE.101) NXZ(5,1)=PR2                                  IZDZ0151
      IF(TQ(2,NT).NE.101) GOTO 204                                      IZDZ0152
      IF(TQC(2,NT).LE.0.AND.TQC(1,NT).LT.0) NXZ(5,1)=PR2                IZDZ0153
      IF(TQC(2,NT).LE.0.AND.TQC(1,NT).LT.0) GOTO 204                    IZDZ0154
      IF(TQC(2,NT).GT.0) NXZ(5,1)=PR3                                   IZDZ0155
      IF(TQC(2,NT).GT.0) GOTO 204                                       IZDZ0156
      KSCP=0                                                            IZDZ0157
      KTQP=NT-1                                                         IZDZ0158
      N11=A(10,NA)                                                      IZDZ0159
      IF(N11.EQ.0) IZIK=2                                               IZDZ0160
      IF(N11.NE.0) IZIK=A(12,S(1,N11))+A(13,S(1,N11))                   IZDZ0161
      DO 207 J=IZIK,KTQP                                                IZDZ0162
      IF(TQC(2,J).EQ.0.AND.TQC(1,J).LE.0) GOTO 207                      IZDZ0163
      IF(TQ(4,J).LE.LP1/2) KSCP=1                                       IZDZ0164
  207 CONTINUE                                                          IZDZ0165
      IF(KSCP.EQ.0) NXZ(5,1)=PR3                                        IZDZ0166
      IF(KSCP.EQ.1) NXZ(5,1)=PR2                                        IZDZ0167
  204 DO 501 K=1,KKLAS                                                  IZDZ0168
      IF(KLASIN(1,K).NE.NXZ(2,2).OR.KLASIN(2,K).NE.NXZ(3,2)) GOTO 501   IZDZ0169
      GRK=GR(K)                                                         IZDZ0170
      GOTO 500                                                          IZDZ0171
  501 CONTINUE                                                          IZDZ0172
      GRK=GRPR                                                          IZDZ0173
  500 IF(GRZ.EQ.PROBEL) GOTO 5000                                       IZDZ0174
      IF(GRZ.NE.GRK) GOTO 60                                            IZDZ0175
 5000 DO 2 J=1,10                                                       IZDZ0176
      IF(NXZ(4,1).EQ.F(J)) GOTO 8                                       IZDZ0177
    2 CONTINUE                                                          IZDZ0178
      WRITE(3,700) GRK,(NXZ(I,3),I=1,9),D2,(NXZ(I,2),I=1,9),D2,(NXZ(I   IZDZ0179
     1,1),I=1,5),ID,NXZ(8,4),NXZ(9,4),(NXZ(I,4),I=1,5),IQ,NKT,IP,(BOT(I)IZDZ0180
     2,I=1,6)
  700 FORMAT(26A2,I5,7A2,I5,I6,I1,6I6)                                  IZDZ0181
      NK=NK+1                                                           IZDZ0182
      GOTO 60                                                           IZDZ0183
    8 LN(2)=LN(3)                                                       IZDZ0184
      LN(3)=LN(4)                                                       IZDZ0185
      LN(4)=D                                                           IZDZ0186
      WRITE(3,700) GRK,(NXZ(I,3),I=1,9),D2,(NXZ(I,2),I=1,9),D2,(NXZ(I   IZDZ0187
     1,1),I=1,5),ID,NXZ(8,4),NXZ(9,4),(NXZ(I,4),I=1,5),IQ,NKT,IP,(BOT(I)IZDZ0188
     2,I=1,6)
      NK=NK+1                                                           IZDZ0189
      GOTO 60                                                           IZDZ0190
   20 NF=2                                                              IZDZ0191
      REWIND 1                                                          IZDZ0192
      GOTO 60                                                           IZDZ0193
   61 REWIND 2                                                          IZDZ0194
      LZ=0                                                              IZDZ0195
      DO 42 I=1,6                                                       IZDZ0196
   42 BOT(I)=0                                                          IZDZ0197
      CTM=0                                                             IZDZ0198
   43 read(8,40,end=41) zak,klas,ip,iq,nom4,naim,nzzm,izei,id,mp,priz
   40 format(1x,4a2,12x,10a2,i1,i5,10a2,5a2,6x,3a2,i5,i6,22x,i1)
      NKT=0
      IF(NZZ(1).EQ.PROBEL.AND.NZZ(2).EQ.PROBEL) GOTO 610                IZDZ0202
      IF(NZZ(2).NE.PROBEL) GOTO 666                                     IZDZ0203
      IF(NZZ(1).EQ.Z10.AND.NZ.EQ.10) GOTO 610                           IZDZ0204
      IF(NZZ(1).EQ.Z15.AND.NZ.EQ.15) GOTO 610                           IZDZ0205
      GOTO 43                                                           IZDZ0206
  666 IF(NZZ(1).NE.NZZM(1).OR.NZZ(2).NE.NZZM(2)) GOTO 43                IZDZ0207
  610 IF(PROBEL.EQ.IC(1,1).AND.PROBEL.EQ.IC(2,1)) GOTO 86               IZDZ0208
      KPRZ=0                                                            IZDZ0209
      XVOST2=D2                                                         IZDZ0070
      ZZZ(1)=ZAK(1)                                                     IZDZ0210
      ZZZ(2)=ZAK(2)                                                     IZDZ0211
      IZD(2)=D                                                          IZDZ0073
      NOMZ=zak(4)                                                       IZDZ0074
      IF(XVOST2.EQ.D2) IZD(1)=D                                         IZDZ0075
   84 DO 85 I=1,40                                                      IZDZ0222
      IF(IC(1,I).EQ.ZZZ(1).AND.IC(2,I).EQ.ZZZ(2)) GOTO 86               IZDZ0223
   85 CONTINUE                                                          IZDZ0224
      GOTO 43                                                           IZDZ0225
   86 PR=PR5                                                            IZDZ0226
      PRR=PROBEL                                                        IZDZ0227
      IF(PRIZ.EQ.1) PRR=PR6                                             IZDZ0228
      IF(PRIZ.EQ.0.AND.IP.EQ.9) PR=PR8                                  IZDZ0229
      IF(PRIZ.EQ.0.AND.IP.EQ.9) PRR=PR7                                 IZDZ0230
      IF(PRIZ.EQ.0.AND.IP.NE.9) PRR=PR7                                 IZDZ0231
  600 DO 601 K=1,KKLAS                                                  IZDZ0232
      IF(KLASIN(1,K).NE.KLAS(2).OR.KLASIN(2,K).NE.KLAS(3)) GOTO 601     IZDZ0233
      GRK=GR(K)                                                         IZDZ0234
      GOTO 602                                                          IZDZ0235
  601 CONTINUE                                                          IZDZ0236
      GRK=GRPR                                                          IZDZ0237
  602 IF(GRZ.EQ.PROBEL) GOTO 622                                        IZDZ0238
      IF(GRK.NE.GRZ) GOTO 43                                            IZDZ0239
  622 WRITE(3,701) GRK,NOM4,KLAS,ZAK,PR,ID,                             IZDZ0245
     1nzzm,NAIM,iq,NKT,IP,(BOT(I),I=1,6)                                IZDZ0246
  701 FORMAT(26A2,I5,2a2,5A2,i5,I6,I1,6I6)                              IZDZ0247
  604 NK=NK+1                                                           IZDZ0248
      GOTO 43                                                           IZDZ0249
   41 REWIND 8                                                          IZDZ0250
      END                                                               IZDZ0251
 
