      INTEGER*2 A(26,200),TQ(6,500),TQC(4,500),NXZ(10,4),U(             OTOB0001
     120,100),KDP(300)                                                  OTOB0002
      INTEGER*2 AP7(500)                                                OTOB0003
      INTEGER*2 AP3(500),AP5(500),AP4(500),AP6(500),U2(100),U6(100)     OTOB0004
      INTEGER QZ(4,500),IW(100)                                         OTOB0005
      INTEGER*2 S(2,200),P(2,200)                                       OTOB0006
      INTEGER POLE(7,502)/3514*' '/                                     OTOB0007
      INTEGER PROB/'    '/,PZP(500)                                     OTOB0008
      INTEGER UW(2,200)                                                 OTOB0009
      INTEGER*2 MK(502),NOP(502),KOL(502)                               OTOB0010
      INTEGER SH(502)                                                   OTOB0011
      INTEGER TR(502),D1(502),D2(502),CIKL0,CIKL1,CIKL2                 OTOB0012
      INTEGER*2 NZAK(5),PTQ(6,500)                                      OTOB0013
      INTEGER*2 KDPG(500),MES,DAY,KLD                                   OTOB0014
      INTEGER*2 P3/'1 '/,KUKU/'  '/,ZAK(4),zakip(4)/'29','38','85','30'/OTOB0015
      INTEGER*2 PRIZ                                                    OTOB0016
      character*8 zakl(10),zakv
      equivalence (zakv,nxz)
      open(4,file='f:\asyip\u.dat',form='unformatted')
      open(unit=3,file='f:\asyip\plan.dat',form='unformatted')
      open(unit=2,file='f:\asyip\portfel.dat',form='unformatted')
      open(1,file='f:\asyip\sostz1.dat')
      open(5,file='f:\asyip\sostz2.dat')
      OPEN(8,FILE='f:\asyip\nakzai.dat')
      open(5,file='f:\asyip\iszakv.dat')
      DATA N1/03/,N2/04/,N3/01/                                         OTOB0018
      do i=1,10
      read(5,5055) zakl(i)
5055  format(a8) 
      enddo
      N0=1                                                              OTOB0019
      READ(N2) NW,NU                                                    OTOB0020
      READ(N2) ((UW(I,J),I=1,2),J=1,NW),((U(I,J),I=1,20),J=1,           OTOB0021
     1NU)                                                               OTOB0022
      NK=0                                                              OTOB0023
      READ(N1) LP,NN,NK                                                 OTOB0024
      LI=LP+LP                                                          OTOB0030
      READ(N1) (KDP(I),I=1,LI)                                          OTOB0031
      CALL KALDNI(NK,0,KLD)                                             OTOB0032
      DO 83 I1=LP,LI                                                    OTOB0033
      IF(KLD.EQ.KDP(I1)) GOTO 84                                        OTOB0034
   83 CONTINUE                                                          OTOB0035
   84 NKP=I1-LP                                                         OTOB0036
  888 CALL KALDNI(NN,250,KDPG)                                          OTOB0037
      LP=250                                                            OTOB0038
      MES=NN/100-NN/10000*100                                           OTOB0039
      DAY=NN-NN/100*100                                                 OTOB0040
      NA=-1000                                                          OTOB0041
  19  IF(N1.EQ.3) READ(N1,END=20) NXZ,(IW(I),I=1,NU),IQ,IP,ID,LD,LZ,LF, OTOB0042
     1MP,MS,NA,NS,NT                                                    OTOB0043
        if(n1.eq.3.and.na.lt.0) goto 20
      IF(N1.EQ.2) READ(N1,END=20) NXZ,(IW(I),I=1,NU),IQ,IP,ID,LZ,MP,NA, OTOB0044
     1NS,NT                                                             OTOB0045
       IF(n1.eq.2.and.NA.LT.0) GOTO 71                                  
      IF(NA.LT.0) GOTO 20                                               OTOB0046
      READ(N1) ((A(I,J),I=1,26),J=1,NA)                                 OTOB0047
      READ(N1) ((S(I,J),I=1,2),J=1,NS),((P(I,J),I=1,2),J=1,NS)          OTOB0048
      READ(N1) ((TQ(I,J),I=1,6),J=1,NT),((QZ(I,J),I=1,4),J=1,NT)        OTOB0049
      READ(N1) ((TQC(I,J),I=1,4),J=1,NT)                                OTOB0050
      ppp=0
      do i=1,10
      if(zakv.eq.zakl(i)) ppp=1
      enddo
 8886  DO I=1,4
        IF(NXZ(I,1).NE.ZAKIP(I)) GOTO 8888
        ENDDO
      goto 19     
 8888 LFF=LF                                                            OTOB0051
      LFF=LF                                                            OTOB0051
      NXZ(5,1)=KUKU                                                     OTOB0052
      MF=0                                                              OTOB0053
      CIKL0=0                                                           OTOB0054
      CIKL1=0                                                           OTOB0055
      CIKL2=0                                                           OTOB0056
      IF(N1.EQ.3) GOTO 79                                               OTOB0057
      LF=LZ+1                                                           OTOB0058
      DO 78 J=1,NT                                                      OTOB0059
      AP3(J)=TQ(1,J)                                                    OTOB0060
      AP4(J)=LF-TQ(4,J)                                                 OTOB0061
      AP5(J)=LF-TQ(5,J)                                                 OTOB0062
      AP6(J)=LF-TQ(6,J)                                                 OTOB0063
      AP7(J)=TQC(2,J)                                                   OTOB0064
   78 CONTINUE                                                          OTOB0065
      GOTO 31                                                           OTOB0066
   79 DO 88 J=1,NT                                                      OTOB0067
      AP3(J)=TQ(1,J)                                                    OTOB0068
      AP4(J)=TQ(4,J)                                                    OTOB0069
      AP5(J)=TQ(5,J)                                                    OTOB0070
      AP6(J)=TQ(6,J)                                                    OTOB0071
      AP7(J)=TQC(2,J)                                                   OTOB0072
  88  CONTINUE                                                          OTOB0073
   31 NT2=NT                                                            OTOB0074
      IPPZ=0                                                            OTOB0075
      LZ1=0                                                             OTOB0076
      DO 133 J=1,NT                                                     OTOB0077
      IF(TQC(2,J).EQ.0) GOTO 133                                        OTOB0078
      IF(TQ(2,J).EQ.1512.OR.TQ(2,J).EQ.1513) IPPZ=1                     OTOB0079
      IF(TQ(2,J).EQ.1512.OR.TQ(2,J).EQ.1513) GOTO 133                   OTOB0080
      IF(TQ(4,J).GE.LF) MF=1                                            OTOB0081
      IF(LF-AP4(J).GT.LZ1) LZ1=LF-AP4(J)                                OTOB0082
  133 CONTINUE                                                          OTOB0083
      IF(IPPZ.EQ.1) LZ1=LZ1+30                                          OTOB0084
      DO 720 K=1,2                                                      OTOB0085
      DO 49 J3=2,NA                                                     OTOB0086
      J2=A(12,J3)+A(13,J3)-1                                            OTOB0087
      J1=A(12,J3)                                                       OTOB0088
      NPOS=J2-1                                                         OTOB0089
      DO 48 J6=J1,J2                                                    OTOB0090
      KJ=J2-J6                                                          OTOB0091
      J4=J1+KJ                                                          OTOB0092
      IF(J4.EQ.J2.AND.TQC(2,J4).NE.0) GOTO 22                           OTOB0093
      IF(TQC(2,J4).NE.0) GOTO 48                                        OTOB0094
      IF(KJ.EQ.0) GOTO 48                                               OTOB0095
      GOTO 700                                                          OTOB0096
  22  NOMP=A(14,J3)                                                     OTOB0097
      KOLP=A(15,J3)                                                     OTOB0098
      KON=NOMP+KOLP-1                                                   OTOB0099
      IF(NA.EQ.3.AND.A(9,2).EQ.A(9,3)) GOTO 48                          OTOB0100
      DO 710 J10=NOMP,KON                                               OTOB0101
      NSB=P(1,J10)                                                      OTOB0102
      IF(NSB.EQ.1) GOTO 710                                             OTOB0103
      NSB=A(12,NSB)                                                     OTOB0104
      IF(TQC(2,NSB).NE.0) GOTO 710                                      OTOB0105
      KJ=KJ+1                                                           OTOB0106
      GOTO 700                                                          OTOB0107
 710  CONTINUE                                                          OTOB0108
      GOTO 48                                                           OTOB0109
 700  KJ=KJ+1                                                           OTOB0110
      DO 51 JK=1,KJ                                                     OTOB0111
      J5=J1+JK-1                                                        OTOB0112
  51  TQC(2,J5)=0                                                       OTOB0113
      GOTO 49                                                           OTOB0114
  48  CONTINUE                                                          OTOB0115
  49  CONTINUE                                                          OTOB0116
 720  CONTINUE                                                          OTOB0117
      J3=A(10,1)                                                        OTOB0118
      J3=A(11,1)+J3-1                                                   OTOB0119
      J3=S(1,J3)                                                        OTOB0120
      J8=A(12,J3)+A(13,J3)-1                                            OTOB0121
      J1=A(12,J3)                                                       OTOB0122
      DO 150 J6=J1,J8                                                   OTOB0123
      KJ=J8-J6                                                          OTOB0124
      J4=J1+KJ                                                          OTOB0125
      IF(TQ(2,J4)-TQ(2,J4)/100*100.NE.83) GOTO 150                      OTOB0126
      R=(QZ(1,J4)+0.0)/U(2,TQ(1,J4))                                    OTOB0127
      J5=INT(R)                                                         OTOB0128
      J66=TQ(5,J4)-TQ(4,J4)                                             OTOB0129
      IF(N1.EQ.2) J66=TQ(4,J4)-TQ(5,J4)                                 OTOB0130
      J7=J66-J5                                                         OTOB0131
      GOTO 151                                                          OTOB0132
 150  CONTINUE                                                          OTOB0133
      J7=0                                                              OTOB0134
 151  CONTINUE                                                          OTOB0135
      LZ2=0                                                             OTOB0136
      NT1=NT                                                            OTOB0137
      DO 111 J=1,NT1                                                    OTOB0138
      IF(TQC(2,J).LE.0) GOTO 111                                        OTOB0139
      IF(J.EQ.1) GOTO 152                                               OTOB0140
      IF(J.GT.J4.AND.J.LE.J8) GOTO 152                                  OTOB0141
      AP4(J)=LF-AP4(J)-J7                                               OTOB0142
      IF(J.EQ.J4) GOTO 553                                              OTOB0143
      AP5(J)=LF-AP5(J)-J7                                               OTOB0144
      AP6(J)=LF-AP6(J)-J7                                               OTOB0145
      GOTO 53                                                           OTOB0146
  152 CONTINUE                                                          OTOB0147
      AP4(J)=LF-AP4(J)                                                  OTOB0148
  553 AP5(J)=LF-AP5(J)                                                  OTOB0149
      AP6(J)=LF-AP6(J)                                                  OTOB0150
      GOTO 53                                                           OTOB0151
  53  CONTINUE                                                          OTOB0152
      IF(AP4(J).GT.LZ2)  LZ2=AP4(J)                                     OTOB0153
  111 CONTINUE                                                          OTOB0154
      DO 90 J=1,NT                                                      OTOB0155
      PTQ(1,J)=TQ(1,J)                                                  OTOB0156
      PTQ(2,J)=TQ(2,J)                                                  OTOB0157
      PTQ(3,J)=TQ(3,J)                                                  OTOB0158
      PTQ(4,J)=AP4(J)                                                   OTOB0159
      PTQ(5,J)=AP5(J)                                                   OTOB0160
      PTQ(6,J)=AP6(J)                                                   OTOB0161
   90 CONTINUE                                                          OTOB0162
      PTQ(4,1)=0                                                        OTOB0163
      PTQ(5,1)=0                                                        OTOB0164
      PTQ(6,1)=0                                                        OTOB0165
      DO 95 I=1,NU                                                      OTOB0166
      U6(I)=U(6,I)                                                      OTOB0167
      U2(I)=U(2,I)                                                      OTOB0168
      U(6,I)=0                                                          OTOB0169
   95 CONTINUE                                                          OTOB0170
      MF=0                                                              OTOB0171
      DO 998 I=1,NT                                                     OTOB0172
      IF(TQC(2,I).EQ.0) GOTO 998                                        OTOB0173
      IF(PTQ(4,I).GE.LF) MF=1                                           OTOB0174
  998 CONTINUE                                                          OTOB0175
      IF(MF.EQ.1) GOTO 91                                               OTOB0176
      IF(A(10,1)+A(11,1)-1.GT.NS) GOTO 91                               OTOB0177
      IF(IP.LE.0) GOTO 91                                               OTOB0178
      IIP=0                                                             OTOB0179
      CIKL0=LZ2                                                         OTOB0180
      DO 96 I=1,NU                                                      OTOB0181
      IF(U(2,I).EQ.820) U(2,I)=U(2,I)*2                                 OTOB0182
   96 CONTINUE                                                          OTOB0183
      CALL PRIOR(A,PTQ,QZ,NT,NA,S,U,CIKL0,IIP,NU,TQC)                   OTOB0184
      CALL MEHDET(NA,NT,A,S,P,PTQ,TQC,ppp,QZ,PZP)                       OTOB0185
      CIKL0=0                                                           OTOB0186
      DO 900 J=1,NT                                                     OTOB0187
      IF(TQC(2,J).EQ.0) GOTO 900                                        OTOB0188
      IF(TQ(2,J).EQ.1512.OR.TQ(2,J).EQ.1513) GOTO 900                   OTOB0189
      IF(PTQ(4,J).GT.CIKL0) CIKL0=PTQ(4,J)                              OTOB0190
  900 CONTINUE                                                          OTOB0191
      IF(IPPZ.EQ.1) CIKL0=CIKL0+30                                      OTOB0192
      IF(IP.EQ.1) GOTO 91                                               OTOB0193
      DO 97 I=1,NU                                                      OTOB0194
   97 U(2,I)=U2(I)                                                      OTOB0195
      IIP=1                                                             OTOB0196
      CIKL1=LZ2                                                         OTOB0197
      DO 92 J=1,NT                                                      OTOB0198
      PTQ(4,J)=AP4(J)                                                   OTOB0199
      PTQ(5,J)=AP5(J)                                                   OTOB0200
      PTQ(6,J)=AP6(J)                                                   OTOB0201
   92 CONTINUE                                                          OTOB0202
      PTQ(4,1)=0                                                        OTOB0203
      PTQ(5,1)=0                                                        OTOB0204
      PTQ(6,1)=0                                                        OTOB0205
      CALL PRIOR(A,PTQ,QZ,NT,NA,S,U,CIKL1,IIP,NU,TQC)                   OTOB0206
      CALL MEHDET(NA,NT,A,S,P,PTQ,TQC,ppp,QZ,PZP)                       OTOB0207
       CIKL1=0                                                          OTOB0208
      DO 901 J=1,NT                                                     OTOB0209
      IF(TQC(2,J).EQ.0) GOTO 901                                        OTOB0210
      IF(TQ(2,J).EQ.1512.OR.TQ(2,J).EQ.1513) GOTO 901                   OTOB0211
      IF(PTQ(4,J).GT.CIKL1) CIKL1=PTQ(4,J)                              OTOB0212
  901 CONTINUE                                                          OTOB0213
      IF(IPPZ.EQ.1) CIKL1=CIKL1+30                                      OTOB0214
      IF(IP.EQ.2.OR.IP.EQ.3) GOTO 91                                    OTOB0215
      DO 80 I=1,NU                                                      OTOB0216
      U(6,I)=U6(I)                                                      OTOB0217
   80 U(2,I)=U2(I)                                                      OTOB0218
      IIP=2                                                             OTOB0219
      CIKL2=LZ2                                                         OTOB0220
      DO 81 J=1,NT                                                      OTOB0221
      PTQ(4,J)=AP4(J)                                                   OTOB0222
      PTQ(5,J)=AP5(J)                                                   OTOB0223
      PTQ(6,J)=AP6(J)                                                   OTOB0224
   81 CONTINUE                                                          OTOB0225
      PTQ(4,1)=0                                                        OTOB0226
      PTQ(5,1)=0                                                        OTOB0227
      PTQ(6,1)=0                                                        OTOB0228
      CALL PRIOR(A,PTQ,QZ,NT,NA,S,U,CIKL2,IIP,NU,TQC)                   OTOB0229
      CALL MEHDET(NA,NT,A,S,P,PTQ,TQC,ppp,QZ,PZP)                       OTOB0230
      CIKL2=0                                                           OTOB0231
      DO 82 J=1,NT                                                      OTOB0232
      IF(TQC(2,J).EQ.0) GOTO 82                                         OTOB0233
      IF(TQ(2,J).EQ.1512.OR.TQ(2,J).EQ.1513) GOTO 82                    OTOB0234
      IF(PTQ(4,J).GT.CIKL2) CIKL2=PTQ(4,J)                              OTOB0235
   82 CONTINUE                                                          OTOB0236
      IF(IPPZ.EQ.1) CIKL2=CIKL2+30                                      OTOB0237
   91 DO 98 I=1,NU                                                      OTOB0238
      U(2,I)=U2(I)                                                      OTOB0239
      U(6,I)=U6(I)                                                      OTOB0240
   98 CONTINUE                                                          OTOB0241
      NT=NT2                                                            OTOB0242
      NZIK=2                                                            OTOB0243
      N=A(10,1)                                                         OTOB0246
      N=S(1,N)                                                          OTOB0247
      N=A(12,N)+A(13,N)-1                                               OTOB0248
      NM=TQ(2,N)/100                                                    OTOB0249
      N=TQ(1,N)                                                         OTOB0250
      NZ=U(1,N)                                                         OTOB0251
      IOCT=0                                                            OTOB0252
      IIW=0                                                             OTOB0253
      DO 999 J=1,NT                                                     OTOB0254
      TQC(2,J)=AP7(J)                                                   OTOB0255
  999 CONTINUE                                                          OTOB0256
      DO 16 I=2,NT                                                      OTOB0257
      IF(TQC(2,I).EQ.0) GOTO 16                                         OTOB0258
      IOCT=IOCT+QZ(1,I)                                                 OTOB0259
  16  CONTINUE                                                          OTOB0260
      DO 15 I=1,NU                                                      OTOB0261
  15  IIW=IIW+IW(I)                                                     OTOB0262
      DO 23 I=1,5                                                       OTOB0263
  23  NZAK(I)=NXZ(I,1)                                                  OTOB0264
      MK(1)=NZ                                                          OTOB0265
      NOP(1)=NM                                                         OTOB0266
      SH(1)=ID/10                                                       OTOB0267
      KOL(1)=NT                                                         OTOB0268
      TR(1)=IOCT                                                        OTOB0269
      TR(1)=(TR(1)+50)/100                                              OTOB0270
      IF(N1.EQ.2) D1(1)=0                                               OTOB0271
      IF(N1.EQ.2) GOTO 7                                                OTOB0272
      LF1=LF                                                            OTOB0273
      LF=LF+LP                                                          OTOB0274
      IF(LF.GT.LP+LP) D1(1)=0                                           OTOB0275
      IF(LF.GT.LP+LP) GOTO 7                                            OTOB0276
      IDATA=DAY                                                         OTOB0277
      LF=KDPG(LF)/10                                                    OTOB0278
      IF(LF-LF/100*100.LT.MES) IDATA=IDATA+1                            OTOB0279
      IF(LF-LF/100*100.EQ.MES.AND.LF1.GT.25) IDATA=IDATA+1              OTOB0280
      D1(1)=LF*100+IDATA                                                OTOB0281
    7 D2(1)=0                                                           OTOB0282
    9 IF(N1.EQ.2) MK(2)=4                                               OTOB0283
      IF(N1.EQ.2) GOTO 74                                               OTOB0284
      IF(LFF.LE.NKP) MK(2)=7                                            OTOB0285
      IF(LFF.LE.NKP) GOTO 74                                            OTOB0286
      IF(NZ.EQ.15) GOTO 75                                              OTOB0287
      IF(TQ(2,2).NE.1512.AND.TQ(2,2).NE.1513) MK(2)=6                   OTOB0288
      IF(TQ(2,2).NE.1512.AND.TQ(2,2).NE.1513) GOTO 74                   OTOB0289
      IF(TQC(2,2).LE.0.AND.TQC(1,2).LT.0) MK(2)=6                       OTOB0290
      IF(TQC(2,2).LE.0.AND.TQC(1,2).LT.0) GOTO 74                       OTOB0291
      IF(TQC(2,2).GT.0) MK(2)=5                                         OTOB0292
      IF(TQC(2,2).GT.0) GOTO 74                                         OTOB0293
      KSCP=0                                                            OTOB0294
      DO 76 J=3,NT                                                      OTOB0295
      IF(TQC(2,J).EQ.0.AND.TQC(1,J).LE.0) GOTO 76                       OTOB0296
      IF(TQ(4,J).LE.LI/2) KSCP=1                                        OTOB0297
   76 CONTINUE                                                          OTOB0298
      IF(KSCP.EQ.0) MK(2)=5                                             OTOB0299
      IF(KSCP.EQ.1) MK(2)=6                                             OTOB0300
      GOTO 74                                                           OTOB0301
   75 IF(TQ(2,NT).NE.101) MK(2)=6                                       OTOB0302
      IF(TQ(2,NT).NE.101) GOTO 74                                       OTOB0303
      IF(TQC(2,NT).LE.0.AND.TQC(1,NT).LT.0) MK(2)=6                     OTOB0304
      IF(TQC(2,NT).LE.0.AND.TQC(1,NT).LT.0) GOTO 74                     OTOB0305
      IF(TQC(2,NT).GT.0) MK(2)=5                                        OTOB0306
      IF(TQC(2,NT).GT.0) GOTO 74                                        OTOB0307
      KSCP=0                                                            OTOB0308
      KTQP=NT-1                                                         OTOB0309
      N11=A(10,NA)                                                      OTOB0310
      IF(N11.EQ.0) IZIK=2                                               OTOB0311
      IF(N11.NE.0) IZIK=A(12,S(1,N11))+A(13,S(1,N11))                   OTOB0312
      DO 77 J=IZIK,KTQP                                                 OTOB0313
      IF(TQC(2,J).EQ.0.AND.TQC(1,J).LE.0) GOTO 77                       OTOB0314
      IF(TQ(4,J).LE.LI/2) KSCP=1                                        OTOB0315
   77 CONTINUE                                                          OTOB0316
      IF(KSCP.EQ.0) MK(2)=5                                             OTOB0317
      IF(KSCP.EQ.1) MK(2)=6                                             OTOB0318
   74 NOP(2)=CIKL2                                                      OTOB0319
      SH(2)=LZ2                                                         OTOB0320
      KOL(2)=LZ1                                                        OTOB0321
      TR(2)=IQ-QZ(2,1)                                                  OTOB0322
      IF(TR(2).LT.0) TR(2)=0                                            OTOB0323
      D1(2)=NT                                                          OTOB0324
      IF(LD-LD/1000*1000.LE.127) LD=LD/10                               BNOM0088
      D2(2)=LD                                                          OTOB0326
      IF(N1.EQ.2) D2(2)=0                                               OTOB0327
  62  DO 8 J=NZIK,NT1                                                   OTOB0328
      MK(J+1)=A(9,(A(1,(TQ(3,J)-TQ(3,J)/1000*1000))))                   OTOB0329
      NOP(J+1)=J-A(12,(A(1,(TQ(3,J)-TQ(3,J)/1000*1000))))+1             OTOB0330
      SH(J+1)=TQ(2,J)                                                   OTOB0331
      KOL(J+1)=TQC(2,J)                                                 OTOB0332
      TR(J+1)=QZ(1,J)                                                   OTOB0333
      ID1=TQC(1,J)+LP                                                   OTOB0334
      IF(TQC(2,J).NE.0) D2(J+1)=0                                       OTOB0335
      IF(TQC(2,J).NE.0) GOTO 100                                        OTOB0336
      IF(ID1.LE.0) D2(J+1)=999999                                       OTOB0337
      IF(ID1.LE.0) GOTO 100                                             OTOB0338
      IDATA=DAY                                                         OTOB0339
      ID1=KDPG(ID1)/10                                                  OTOB0340
      IF(ID1-ID1/100*100.GT.MES) IDATA=IDATA-1                          OTOB0341
      IF(ID1-ID1/100*100.EQ.MES.AND.TQC(1,J).LE.0) IDATA=IDATA-1        OTOB0342
      D2(J+1)=ID1*100+IDATA                                             OTOB0343
  100 IF(N1.EQ.2) D1(J+1)=TQ(5,J)                                       OTOB0344
      IF(N1.EQ.2.AND.TQC(2,J).EQ.0) D1(J+1)=0                           OTOB0345
      IF(N1.EQ.2) GOTO 8                                                OTOB0346
      ID2=TQ(5,J)+LP                                                    OTOB0347
      IF(TQ(5,J).LE.0) D1(J+1)=0                                        OTOB0348
      IF(TQ(5,J).LE.0) GOTO 8                                           OTOB0349
      IF(TQ(5,J).GT.2*LP) D1(J+1)=0                                     OTOB0350
      IF(TQ(5,J).GT.2*LP) GOTO 8                                        OTOB0351
      IF(TQC(1,J).LE.1.AND.TQC(2,J).EQ.0) D1(J+1)=0                     OTOB0352
      IF(TQC(1,J).LE.1.AND.TQC(2,J).EQ.0) GOTO 8                        OTOB0353
      IDATA=DAY                                                         OTOB0354
      ID2=KDPG(ID2)/10                                                  OTOB0355
      IF(ID2-ID2/100*100.LT.MES) IDATA=IDATA+1                          OTOB0356
      IF(ID2-ID2/100*100.EQ.MES.AND.TQ(5,J).GT.25) IDATA=IDATA+1        OTOB0357
      D1(J+1)=ID2*100+IDATA                                             OTOB0358
  8   CONTINUE                                                          OTOB0359
  1   FORMAT(5A2,I2,I2,I4,I3,I5,I6,I6)                                  OTOB0361
  3   NT2=NT1+1                                                         OTOB0362
      DO 40 I=1,NT2                                                     OTOB0363
      POLE(1,I)=MK(I)                                                   OTOB0364
      POLE(2,I)=NOP(I)                                                  OTOB0365
      POLE(3,I)=SH(I)                                                   OTOB0366
      POLE(4,I)=KOL(I)                                                  OTOB0367
      POLE(5,I)=TR(I)                                                   OTOB0368
      POLE(6,I)=D1(I)                                                   OTOB0369
      POLE(7,I)=D2(I)                                                   OTOB0370
   40 CONTINUE                                                          OTOB0371
      WRITE(N3,93)
   93 FORMAT(5A2,I2,I2,I4,I3,I5,I6,I6,I1,I3,I4,I3,I1,I5,I3,I3,I6)       OTOB0372
      WRITE(N3,93) NZAK,(POLE(I,1),I=1,7),POLE(1,2),POLE(2,2),POLE      OTOB0374
     2(3,2),POLE(4,2),IP,POLE(5,2),CIKL0,CIKL1,POLE(7,2)                
      DO J=3,NT2
   11 WRITE(5,1) NZAK,(POLE(I,J),I=1,7)   
      ENDDO     
   94 DO 6 I=1,500                                                      OTOB0389
      DO 30 J=1,7                                                       OTOB0390
  30  POLE(J,I)=PROB                                                    OTOB0391
  6   CONTINUE                                                          OTOB0392
      GOTO 19                                                           OTOB0398
  20  n1=2                                                              OTOB0400
      GOTO 19                                                           OTOB0401
   71 READ(8,70,END=72) ZAK,IP,IQ,NM,NA,ID,MDP,D2(1),KD21,D2(2),KD22,
     1PRIZ
   70 FORMAT(1X,4A2,32X,I1,I5,30X,I2,8X,I2,I5,I6,5X,I6,I2,I6,I2,I1,1X)
      IF(NM.EQ.50) NM=8                                                 OTOB0405
      IF(NM.EQ.60) NM=4                                                 OTOB0406
      IF(PRIZ.EQ.1) D1(1)=3                                             OTOB0407
      IF(PRIZ.EQ.0.AND.IP.EQ.9) D1(1)=1                                 OTOB0408
      IF(PRIZ.EQ.0.AND.IP.NE.9) D1(1)=2                                 OTOB0409
      D2(1)=D2(1)/100*100+KD21
      D2(2)=D2(2)/100*100+KD22
      IF(D2(2).EQ.0) D2(2)=MDP
      NZ=NA                                                             OTOB0410
      NT=2                                                              OTOB0411
   73 FORMAT(4A2,2X,I2,I2,I4,I3,11X,I6,I1,10X,I1,I5,6X,I6)              OTOB0412
      WRITE(N3,73) ZAK,NZ,NM,ID,NT,D2(1),D1(1),IP,IQ,D2(2)              OTOB0413
      GOTO 71                                                           OTOB0414
   72 STOP                                                              OTOB0415
      END                                                               OTOB0416
      SUBROUTINE MEHDET(NA,NT,A,S,P,TQ,TQC,ppp,QZ,PZP)                  ZAK20006
      INTEGER*2 A(26,200),S(2,200),P(2,200),TQ(6,500),TQC(4,500),
     1W1(100)/100*0/,  
     2W2(100)/100*0/,W3(50)/50*0/,W4(50)/50*0/                          ZAK20008
      INTEGER QZ(4,500),PZP(500)
      DATA NK/82/                                                       ZAK20009
      DO 11 I=1,NA                                                      ZAK20010
   11 A(16,I)=0                                                         ZAK20011
      IT=0                                                              ZAK20012
      DO 10 I=1,NT                                                      ZAK20013
      TQC(4,I)=0                                                        ZAK20014
      I1=TQ(2,I )-TQ(2,I )/100*100                                      ZAK20015
      IF(I1.LE.NK) GO TO 10                                             ZAK20016
      IF(TQ(4,I).GT.IT) IT=TQ(4,I)                                      ZAK20017
      TQC(4,I)=1                                                        ZAK20018
   10 CONTINUE                                                          ZAK20019
      IF(IT.LE.0) GO TO 24                                              ZAK20020
      IT=IT+1                                                           ZAK20021
      K=0                                                               ZAK20022
      DO 9 I=1,NA                                                       ZAK20023
      IF(A(11,I).GT.0) GO TO 9                                          ZAK20024
      CALL DERPOD(I,A,P,TQC,W1,W2,W3,TQ,K,QZ,PZP,PPP,NT)                ZAK20025
 8886 Continue                                                          OTOB0231
    9 CONTINUE                                                          ZAK20026
   15 CONTINUE                                                          ZAK20027
      IF(K.LE.0) GO TO 12                                               ZAK20028
      DO 14 I=1,K                                                       ZAK20029
   14 W4(I)=W3(I)                                                       ZAK20030
      K1=K                                                              ZAK20031
      K=0                                                               ZAK20032
      DO 13 I=1,K1                                                      ZAK20033
      J1=W4(I)                                                          ZAK20034
      CALL DERPOD(J1,A,P,TQC,W1,W2,W3,TQ,K,QZ,PZP,PPP,NT)               ZAK20035
 8887 Continue                                                          OTOB0231
   13 CONTINUE                                                          ZAK20036
      GO TO 15                                                          ZAK20037
   12 J0=1                                                              ZAK20038
      J=0                                                               ZAK20039
    2 J=J+1                                                             ZAK20040
      W1(J)=J0                                                          ZAK20041
      W2(J)=A(11,J0)                                                    ZAK20042
      I1=A(13,J0)                                                       ZAK20043
      IF(I1.LE.0) GO TO 3                                               ZAK20044
      IF(A(16,J0)-1) 5,3,6                                              ZAK20045
    6 I1=A(16,J0)-1                                                     ZAK20046
    5 I2=A(12,J0)+I1-1                                                  ZAK20047
      I3=W1(J-1)                                                        ZAK20048
      I3=A(12,I3)                                                       ZAK20049
      IF(TQ(6,I2).GT.TQ(4,I3)) GO TO 4                                  ZAK20050
      II=TQ(4,I3)-TQ(6,I2)+1                                            ZAK20051
      GO TO 8                                                           ZAK20052
    4 CONTINUE                                                          ZAK20053
      IF(TQ(6,I2).GE.IT) GO TO 3                                        ZAK20054
      II=IT-TQ(6,I2)                                                    ZAK20055
    8 DO 7 I=1,I1                                                       ZAK20056
      I2=A(12,J0)+I-1                                                   ZAK20057
      TQ(4,I2)=TQ(4,I2)+II                                              ZAK20058
      TQ(5,I2)=TQ(5,I2)+II                                              ZAK20059
    7 TQ(6,I2)=TQ(6,I2)+II                                              ZAK20060
      IF(A(16,J0).GT.0) GO TO 3                                         ZAK20061
      A(16,J0)=-1                                                       ZAK20062
    3 W2(J)=W2(J)-1                                                     ZAK20063
      IF(W2(J).LT.0) GO TO 1                                            ZAK20064
      J0=A(10,J0)+W2(J)                                                 ZAK20065
      J0=S(1,J0)                                                        ZAK20066
      GO TO 2                                                           ZAK20067
    1 J=J-1                                                             ZAK20068
      IF(J.LE.0) GO TO 24                                               ZAK20069
      J0=W1(J)                                                          ZAK20070
      GO TO 3                                                           ZAK20071
   24 DO 17 I=1,NA                                                      ZAK20072
   17 A(16,I)=-1                                                        ZAK20073
      RETURN                                                            ZAK20074
      END                                                               ZAK20075
      SUBROUTINE DERPOD(I,A,P,TQC,W1,W2,W3,TQ,K,QZ,PZP,PPP,NT)          ZAK60006
      INTEGER*2 A(26,200),P(2,200),TQC(4,500),W1(100),W2(100),W3(50),
     1TQ(6,500)
      integer QZ(4,500),PZP(500)       
      JJ=0                                                              ZAK60008
      J=0                                                               ZAK60009
      J0=I                                                              ZAK60010
   12 J=J+1                                                             ZAK60011
      IF(J.GT.100) STOP 100                                             ZAK60012
      W1(J)=J0                                                          ZAK60013
      W2(J)=A(15,J0)                                                    ZAK60014
      IF(JJ.LE.0) GO TO 13                                              ZAK60015
    2 A(16,J0)=1                                                        ZAK60016
      L=A(12,J0)                                                        ZAK60017
      TQC(4,L)=1                                                        ZAK60018
      IF(TQ(3,L).LT.1000) GO TO 15                                      ZAK60019
      I1=TQ(3,L)/1000                                                   ZAK60020
      I2=TQ(3,L)-I1*1000                                                ZAK60021
      I1=I1+1                                                           ZAK60022
      DO 1 L=1,I1                                                       ZAK60023
      J1=I2+L-1                                                         ZAK60024
      J1=A(1,J1)                                                        ZAK60025
      IF(A(16,J1).GT.0) GO TO 1                                         ZAK60026
      K=K+1                                                             ZAK60027
      W3(K)=J1                                                          ZAK60028
    1 A(16,J1)=1                                                        ZAK60029
      GO TO 15                                                          ZAK60030
 8885 FORMAT(1X,2I6)
   13 I1=A(13,J0)                                                       ZAK60031
      IF(I1.LE.0) GO TO 15                                              ZAK60032
      DO 14 L=1,I1                                                      ZAK60033
      I2=A(12,J0)+L-1                                                   ZAK60034
      IF(TQC(4,I2).LE.0) GO TO 14                                       ZAK60035
      A(16,J0)=L                                                        ZAK60036
      JJ=J                                                              ZAK60037
      GO TO 15                                                          ZAK60038
   14 CONTINUE                                                          ZAK60039
   15 W2(J)=W2(J)-1                                                     ZAK60040
      IF(W2(J).LT.0) GO TO 16                                           ZAK60041
      J0=W1(J)                                                          ZAK60042
      J0=A(14,J0)+W2(J)                                                 ZAK60043
      J0=P(1,J0)                                                        ZAK60044
      IF(A(16,J0).LE.0) GO TO 12                                        ZAK60045
      IF(A(16,J0).GT.1.AND.JJ.GT.0) GO TO 2                             ZAK60046
      GO TO 15                                                          ZAK60047
   16 J=J-1                                                             ZAK60048
      IF(J.LE.0) RETURN                                                 ZAK60049
      IF(JJ.GT.J) JJ=0                                                  ZAK60050
      GO TO 15                                                          ZAK60051
      END                                                               ZAK60052
 
