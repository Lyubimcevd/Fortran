      INTEGER*2 A(26,200),S(2,200),P(2,200),TQ(6,500),TQC(4,500),       PLEK0001
     1U(20,100),NXZ(10,4),AZ(8,4000),ZOP(2,8000),ZP(2,300),KDP(180)     PLEK0002
      INTEGER ZG(100,60),UC(100)                                        PLEK0003
      INTEGER QZ(4,500),IW(100),UW(2,200)                               PLEK0004
      INTEGER*2 KDPP(180),ZAKP(4,100)/400*'99'/                         PLEK0005
      INTEGER*2 IC(2,65),PIC(65),KPRP(1)                                PLEK0006
      character*1 prd/' '/,py/' '/
      DATA LIMZ/4000/,LMZP/300/,LIMO/8000/                              PLEK0007
      DATA NF1/4/,NF2/3/,ND/10/                                         PLEK0008
      DATA NZ,JO,JD,NPZ/4*0/                                            PLEK0009
      open(10,file='F:\ASUIPW\tek_INF\work.dat',access='direct',recl=120
     18)
    2 FORMAT (10I8)                                                     PLEK0011
   30 FORMAT(4A2,72X)                                                   PLEK0012
    1 FORMAT(A2,A2,I2,7X)                                               PLEK0013
      open(unit=nf1,file='F:\ASUIPW\tek_INF\u.dat',form='unformatted')
      READ (NF1) NW,NU                                                  PLEK0014
      READ (NF1) ((UW(I,J),I=1,2),J=1,NW),((U(I,J),I=1,20),J=1,NU)      PLEK0015
      REWIND NF1                                                        PLEK0016
      I=0                                                               PLEK0017
      DO 4 J=1,NU                                                       PLEK0018
      IF(U(7,J).LE.0) GO TO 4                                           PLEK0019
      I=I+1                                                             PLEK0020
      U(7,J)=I                                                          PLEK0021
      UC(I)=(U(3,J)*U(4,J)+50)/100                                      PLEK0022
    4 CONTINUE                                                          PLEK0023
      NUC=I                                                             PLEK0024
      jw=1                                                              PLEK0025
      ld=0                                                              PLEK0026
      open(666, file = 'tmp.txt')
      open(5,file='F:\ASUIPW\tek_INF\prizp.dat')
      do 7777 i=1,65
      READ (5,1) IC(1,I),IC(2,I),PIC(I)                                 PLEK0027
 7777 continue
      read(666,777) nn
  777 format(i8)
      read(666,777) nk
  781 CALL KALDNI(NK,0,KDP)                                             PLEK0030
      NK1=KDP(1)                                                        PLEK0031
      CALL KALDNI(NN,90,KDP)                                            PLEK0032
      DO 5 J=91,180                                                     PLEK0033
      IF(NK1.EQ.KDP(J)) GOTO 6                                          PLEK0034
    5 CONTINUE                                                          PLEK0035
    6 LP=J+LD-90                                                        PLEK0036
      CALL KALDNI(NN,LP,KDP)                                            PLEK0037
      open(unit=2,file='F:\ASUIPW\tek_INF\plan.dat',form='unformatted')
      open(unit=nf2,file='F:\ASUIPW\tek_INF\plan1.dat',form='unformatted
     1')
      open(unit=1,file='F:\ASUIPW\tek_INF\portfel.dat',form='unformatted
     1')
      open(unit=8,file='F:\ASUIPW\tek_INF\portfel1.dat',form='unformatte
     1d')
      IF(JW.LE.0) GOTO 7                                                PLEK0038
      READ(02) I,J,K                                                    PLEK0039
      I1=I+I                                                            PLEK0040
      READ(02) (KDPP(M),M=1,I1)                                         PLEK0041
      CALL KALDNI(K,0,KPRP)                                             PLEK0042
      DO 10 M=1,I1                                                      PLEK0043
      IF(KDPP(M).EQ.KPRP(1)) GOTO 11                                    PLEK0044
   10 CONTINUE                                                          PLEK0045
   11 KPRP=M-I                                                          PLEK0046
      open(9,file='F:\ASUIPW\tek_INF\progz.dat')
      READ(9,30,END=31) ((ZAKP(I,J),I=1,4),J=1,100)                     PLEK0050
   31 CALL INPACK(ND,2,NU,NUC,LP,NZ,NPZ,JO,JD,LIMZ,LIMO,LMZP,AZ,ZOP     PLEK0051
     1,ZG,ZP,U,NXZ,IW,A,S,P,TQ,QZ,TQC,KDP,K5,K55,K,KPRP(1),IC,PIC,ZAKP) PLEK0052
    7 CONTINUE                                                          PLEK0053
      CALL INPACK(ND,1,NU,NUC,LP,NZ,NPZ,JO,JD,LIMZ,LIMO,LMZP,AZ,ZOP     PLEK0054
     1,ZG,ZP,U,NXZ,IW,A,S,P,TQ,QZ,TQC,KDP,K5,K55,K,KPRP(1),IC,PIC,ZAKP) PLEK0055
      CALL PERPLN(NZ,NPZ,LP,LD,NUC,JO,ZG,UC,AZ,ZOP)                     PLEK0056
      CALL OBPLAN(ND,LP,LD,NZ,NPZ,NUC,AZ,ZP,ZG,UC)                      PLEK0057
      CALL PLTPGN(2,1,8,NF2,9,NU,NUC,LP,NN,NK,NZ,NPZ,A,S,P,TQ,QZ,TQC,NX PLEK0058
     1Z,IW,KDP(1),AZ,ZG,U,K5,K55)                                       PLEK0059
      STOP                                                              PLEK0060
      END                                                               PLEK0061
      SUBROUTINE INPACK(ND,NF1,NU,NUC,LP,NZ,NPZ,JO,JD,LIMZ,LIMO,LMZP,AZ PLEK0062
     1,ZOP,ZG,ZP,U,NXZ,IW,A,S,P,TQ,QZ,TQC,KDP,K5,K55,KPP,KPRP,IC,PIC,   PLEK0063
     2ZAKP)                                                             PLEK0064
      INTEGER*2 A(26,1),S(2,1),P(2,1),TQ(6,1),TQC(4,1),NXZ(10,4),       PLEK0065
     2U(20,1),AZ(8,1),ZP(2,1),ZOP(2,1)                                  PLEK0066
     3,KDP(1),ZAKP(4,1)                                                 PLEK0067
      INTEGER*2 CD/'CO'/                                                PLEK0068
      INTEGER*2 MET/'ÑÏ'/                                               PLEK0069
      INTEGER*2 MET1/'  '/                                              PLEK0070
      INTEGER QZ(4,1),IW(NU),W(20),ZG(100,1)                            PLEK0071
      INTEGER*2 CON,C                                                   PLEK0072
      INTEGER*2 IC(2,65),PIC(65),ZAK(2)                                 PLEK0073
      LOGICAL*1 XVOST(2),IZD(2),D/' '/                                  PLEK0074
      INTEGER*2 NOMZ                                                    PLEK0075
      INTEGER*2 XVOST2,D2/'  '/                                         PLEK0076
      INTEGER*2 PTQ(500),RAZ                                            PLEK0077
      EQUIVALENCE (NOMZ,XVOST(1)),(XVOST(2),XVOST2),(ZAK(2),IZD(1))     PLEK0078
      DATA NSLES/83/,C/'99'/                                            PLEK0079
   76 FORMAT (6(I4,4X))                                                 PLEK0080
      IF(NF1.EQ.2) NF=6                                                 PLEK0081
      IF(NF1.EQ.1) NF=5                                                 PLEK0082
      K5=0                                                              PLEK0083
      K55=0                                                             PLEK0084
      LP1=LP+LP                                                         PLEK0085
      DO 1 J=1,LP                                                       PLEK0086
      DO 1 I=1,NUC                                                      PLEK0087
    1 ZG(I,J)=0                                                         PLEK0088
      DO 2 K=1,2500                                                     PLEK0089
      IF(NF1.NE.2) GOTO 35                                              PLEK0090
      READ(NF1) NXZ,IW,IQ,IP,ID,LD,LZ,LF,MP,MS,NA,NS,NT                 PLEK0091
      IF(LF.LE.KPRP) IPRZ=0                                             PLEK0092
      IF(LF.GT.KPRP) IPRZ=1                                             PLEK0093
      GOTO 34                                                           PLEK0094
   35  IF(NF1.EQ.1) READ(NF1) NXZ,IW,IQ,IP,ID,LZ,MP,NA,NS,NT            PLEK0095
   34 IF(NA.LE.-1000) GOTO4                                             PLEK0096
      READ (NF1) ((A(I,J),I=1,26),J=1,NA)                               PLEK0097
      READ (NF1) ((S(I,J),I=1,2),J=1,NS),((P(I,J),I=1,2),J=1,NS)        PLEK0098
      READ (NF1) ((TQ(I,J),I=1,6),J=1,NT),((QZ(I,J),I=1,4),J=1,NT)      PLEK0099
      READ (NF1) ((TQC(I,J),I=1,4),J=1,NT)                              PLEK0100
      PRG=0                                                             PLEK0101
      IF(ZAKP(1,1).EQ.C) GOTO 34444                                     PLEK0102
      DO 32222 J=1,100                                                  PLEK0103
      IF(ZAKP(1,J).EQ.C) GOTO 34444                                     PLEK0104
      DO 33333 I=1,4                                                    PLEK0105
      IF(NXZ(I,1).NE.ZAKP(I,J)) GOTO 32222                              PLEK0106
33333 CONTINUE                                                          PLEK0107
      PRG=1                                                             PLEK0108
      GOTO 34444                                                        PLEK0109
32222 CONTINUE                                                          PLEK0110
34444 CON=99                                                            PLEK0111
      N=A(10,1)                                                         PLEK0112
      N=S(1,N)                                                          PLEK0113
      N=A(12,N)+A(13,N)-1                                               PLEK0114
      N=TQ(1,N)                                                         PLEK0115
      N=U(1,N)                                                          PLEK0116
      IF(NF.EQ.5.AND.N.EQ.17) K5=K5+1                                   PLEK0117
      IF(NF.EQ.5.AND.N.EQ.15) K55=K55+1                                 PLEK0118
      IF(NF.EQ.5.AND.IP.EQ.9) GOTO 2                                    PLEK0119
      XVOST2=D2                                                         PLEK0120
      ZAK(1)=NXZ(1,1)                                                   PLEK0121
      ZAK(2)=NXZ(2,1)                                                   PLEK0122
      IZD(2)=D                                                          PLEK0123
      NOMZ=NXZ(4,1)                                                     PLEK0124
      IF(XVOST2.EQ.D2) IZD(1)=D                                         PLEK0125
      DO 601 I=1,65                                                     PLEK0126
      IF(IC(1,I).EQ.ZAK(1).AND.IC(2,I).EQ.ZAK(2)) CON=PIC(I)            PLEK0127
  601 CONTINUE                                                          PLEK0128
      IF(NXZ(5,1).EQ.CD.AND.NF.EQ.6) GOTO 2                             PLEK0129
      IF(N.EQ.99) GOTO 2                                                PLEK0130
  600 CONTINUE                                                          PLEK0131
      J1=A(10,1)                                                        PLEK0132
      J2=A(11,1)+J1-1                                                   PLEK0133
      DO 19 J3=J1,J2                                                    PLEK0134
      J=S(1,J3)                                                         PLEK0135
      DO 20 I=1,10                                                      PLEK0136
      IF(A(I+16,J).NE.NXZ(I,2)) GOTO 19                                 PLEK0137
   20 CONTINUE                                                          PLEK0138
      GOTO 21                                                           PLEK0139
   19 CONTINUE                                                          PLEK0140
   21 CONTINUE                                                          PLEK0141
      DO 720 KOL=1,2                                                    PLEK0142
      DO 49 J3=2,NA                                                     PLEK0143
      J2=A(12,J3)+A(13,J3)-1                                            PLEK0144
      J1=A(12,J3)                                                       PLEK0145
      NPOS=J2-1                                                         PLEK0146
      DO 50 J6=J1,J2                                                    PLEK0147
      KJ=J2-J6                                                          PLEK0148
      J4=J1+KJ                                                          PLEK0149
      IF(J4.EQ.J2.AND.TQC(2,J4).NE.0)                                   PLEK0150
     1GOTO 22                                                           PLEK0151
      IF(TQC(2,J4).NE.0) GOTO50                                         PLEK0152
      IF(KJ.EQ.0) GOTO50                                                PLEK0153
      GOTO 700                                                          PLEK0154
   22 NOMP=A(14,J3)                                                     PLEK0155
      KOLP=A(15,J3)                                                     PLEK0156
      KON=NOMP+KOLP-1                                                   PLEK0157
      IF(NA.EQ.3.AND.A(9,2).EQ.A(9,3)) GOTO 50                          PLEK0158
      DO 710 J10=NOMP,KON                                               PLEK0159
      NSB=P(1,J10)                                                      PLEK0160
      IF(NSB.EQ.1) GOTO 710                                             PLEK0161
      NSB=A(12,NSB)                                                     PLEK0162
      IF(TQC(2,NSB).NE.0) GOTO 710                                      PLEK0163
      KJ=KJ+1                                                           PLEK0164
      GOTO 700                                                          PLEK0165
  710 CONTINUE                                                          PLEK0166
      GOTO 50                                                           PLEK0167
  700 KJ=KJ+1                                                           PLEK0168
      DO 51 JK=1,KJ                                                     PLEK0169
      J5=J1+JK-1                                                        PLEK0170
  51  TQC(2,J5)=0                                                       PLEK0171
      GOTO 49                                                           PLEK0172
   50 CONTINUE                                                          PLEK0173
   49 CONTINUE                                                          PLEK0174
  720 CONTINUE                                                          PLEK0175
      J3=A(10,1)                                                        PLEK0176
      J3=A(11,1)+J3-1                                                   PLEK0177
      J3=S(1,J3)                                                        PLEK0178
      J8=A(12,J3)+A(13,J3)-1                                            PLEK0179
      J1=A(12,J3)                                                       PLEK0180
      DO 150 J6=J1,J8                                                   PLEK0181
      KJ=J8-J6                                                          PLEK0182
      J4=J1+KJ                                                          PLEK0183
      IF(TQ(2,J4)-TQ(2,J4)/100*100.NE.83) GOTO 150                      PLEK0184
      R=(QZ(1,J4)+0.0)/U(2,TQ(1,J4))                                    PLEK0185
      J5=INT(R)                                                         PLEK0186
      IF(R-J5.LE.0.0) J5=J5-1                                           PLEK0187
      IF(NF.EQ.5) J66=TQ(4,J4)-TQ(5,J4)                                 PLEK0188
      IF(NF.EQ.6) J66=TQ(5,J4)-TQ(4,J4)                                 PLEK0189
      J7=J66-J5                                                         PLEK0190
      GOTO 151                                                          PLEK0191
 150  CONTINUE                                                          PLEK0192
      J7=0                                                              PLEK0193
  151 CONTINUE                                                          PLEK0194
      J=A(12,J)+A(13,J)-1                                               PLEK0195
      J=TQ(1,J)                                                         PLEK0196
      NC=U(1,J)                                                         PLEK0197
      IF(NXZ(5,1).EQ.MET) GOTO 14                                       PLEK0198
      IF(TQC(1,1).LT.0.AND.IQ.EQ.0) GOTO 14                             PLEK0199
      IF(TQC(1,1).LT.0) TQC(2,1)=0                                      PLEK0200
      IF((IQ-QZ(2,1)-TQC(2,1)).GT.0) GOTO 14                            PLEK0201
      GO TO 2                                                           PLEK0202
  140 J1=A(10,1)                                                        PLEK0203
      J2=A(11,1)+J1-1                                                   PLEK0204
      DO 13 J3=J1,J2                                                    PLEK0205
      J=S(1,J3)                                                         PLEK0206
      J=A(12,J)+A(13,J)-1                                               PLEK0207
      IF(TQC(2,J).GT.0)GOTO 14                                          PLEK0208
   13 CONTINUE                                                          PLEK0209
      GOTO2                                                             PLEK0210
   14 LZ=0                                                              PLEK0211
      NT1=NT                                                            PLEK0212
      NT=0                                                              PLEK0213
      DO 3 J=2,NT1                                                      PLEK0214
      IF(TQC(2,J).LE.0) GO TO 3                                         PLEK0215
      NT=NT+1                                                           PLEK0216
      IF(NF.EQ.5) GOTO 555                                              PLEK0217
      IF(J.EQ.1) GOTO 152                                               PLEK0218
      IF(J.GT.J4.AND.J.LE.J8) GOTO 152                                  PLEK0219
      TQ(4,J)=LF-TQ(4,J)-J7                                             PLEK0220
      IF(J.EQ.J4) GOTO 553                                              PLEK0221
      TQ(5,J)=LF-TQ(5,J)-J7                                             PLEK0222
      TQ(6,J)=LF-TQ(6,J)-J7                                             PLEK0223
      GOTO 53                                                           PLEK0224
  152 CONTINUE                                                          PLEK0225
      TQ(4,J)=LF-TQ(4,J)                                                PLEK0226
  553 TQ(5,J)=LF-TQ(5,J)                                                PLEK0227
      TQ(6,J)=LF-TQ(6,J)                                                PLEK0228
      IF(NF.EQ.6) GOTO 53                                               PLEK0229
  555 IF(J.EQ.1) GOTO 53                                                PLEK0230
      IF(J.GT.J4.AND.J.LE.J8) GOTO 53                                   PLEK0231
      TQ(4,J)=TQ(4,J)-J7                                                PLEK0232
      IF(J.EQ.J4) GOTO 53                                               PLEK0233
      TQ(5,J)=TQ(5,J)-J7                                                PLEK0234
      TQ(6,J)=TQ(6,J)-J7                                                PLEK0235
   53 IF(TQ(4,J).GT.LZ) LZ=TQ(4,J)                                      PLEK0236
    3 CONTINUE                                                          PLEK0237
      LZ=LZ+1                                                           PLEK0238
      IF(LZ-1.EQ.0) GOTO 602                                            PLEK0239
      DO 68 I=1,NT1                                                     PLEK0240
   68 PTQ(I)=0                                                          PLEK0241
      NSBOR=A(10,1)                                                     PLEK0242
      KSBOR=NSBOR+A(11,1)-1                                             PLEK0243
      DO 60 I=NSBOR,KSBOR                                               PLEK0244
      PTQ(A(12,S(1,I)))=1                                               PLEK0245
   60 CONTINUE                                                          PLEK0246
      MIN=32000                                                         PLEK0247
      MAX=0                                                             PLEK0248
      DO 61 I=2,NA                                                      PLEK0249
      NMK=A(12,I)                                                       PLEK0250
      KMK=NMK+A(13,I)-1                                                 PLEK0251
      IF(PTQ(NMK).EQ.1) GOTO 61                                         PLEK0252
      KR=0                                                              PLEK0253
      DO 62 J=NMK,KMK                                                   PLEK0254
      IF(KR.EQ.1) GOTO 63                                               PLEK0255
      KOD=TQ(2,J)-TQ(2,J)/100*100                                       PLEK0256
      IF(KOD.EQ.NSLES) KR=1                                             PLEK0257
      IF(KOD.EQ.NSLES) GOTO 63                                          PLEK0258
      IF(TQC(2,J).EQ.0) GOTO 62                                         PLEK0259
      IF(TQ(6,J).LT.MIN) MIN=TQ(6,J)                                    PLEK0260
      GOTO 62                                                           PLEK0261
   63 IF(TQC(2,J).EQ.0) GOTO 62                                         PLEK0262
      IF(TQ(4,J).GT.MAX) MAX=TQ(4,J)                                    PLEK0263
      GOTO 61                                                           PLEK0264
   62 CONTINUE                                                          PLEK0265
   61 CONTINUE                                                          PLEK0266
      IF(MIN.EQ.32000.OR.MIN.LT.0) GOTO 602                             PLEK0267
      IF(MAX.NE.0) GOTO 64                                              PLEK0268
      DO 91 I=NSBOR,KSBOR                                               PLEK0269
      NMK=A(12,S(1,I))                                                  PLEK0270
      KMK=NMK+A(13,S(1,I))-1                                            PLEK0271
      DO 92 J=NMK,KMK                                                   PLEK0272
      IF(TQC(2,J).EQ.0) GOTO 92                                         PLEK0273
      IF(TQ(4,J).GT.MAX) MAX=TQ(4,J)                                    PLEK0274
      GOTO 91                                                           PLEK0275
   92 CONTINUE                                                          PLEK0276
   91 CONTINUE                                                          PLEK0277
   64 RAZ=MIN-MAX                                                       PLEK0278
      IF(RAZ.LE.1) GOTO 602                                             PLEK0279
      RAZ=RAZ-1                                                         PLEK0280
      DO 65 I=2,NA                                                      PLEK0281
      NMK=A(12,I)                                                       PLEK0282
      KMK=NMK+A(13,I)-1                                                 PLEK0283
      IF(PTQ(NMK).EQ.1) GOTO 65                                         PLEK0284
      PTQ(NMK)=1                                                        PLEK0285
      DO 66 J=NMK,KMK                                                   PLEK0286
      KOD=TQ(2,J)-TQ(2,J)/100*100                                       PLEK0287
      IF(KOD.EQ.NSLES) GOTO 65                                          PLEK0288
      IF(TQC(2,J).EQ.0) GOTO 66                                         PLEK0289
      DO 67 I2=4,6                                                      PLEK0290
   67 TQ(I2,J)=TQ(I2,J)-RAZ                                             PLEK0291
   66 CONTINUE                                                          PLEK0292
   65 CONTINUE                                                          PLEK0293
  602 LZ=0                                                              PLEK0294
      DO 78 J=2,NT1                                                     PLEK0295
      IF(TQC(2,J).EQ.0) GOTO 78                                         PLEK0296
      IF(TQ(4,J).GT.LZ) LZ=TQ(4,J)                                      PLEK0297
   78 CONTINUE                                                          PLEK0298
      LZ=LZ+1                                                           PLEK0299
      IF(NF.EQ.5) GOTO 12                                               PLEK0300
      NPZ=NPZ+1                                                         PLEK0301
   12 DO 5 J1=1,NT1                                                     PLEK0302
      IF(TQC(2,J1).LE.0) GOTO 5                                         PLEK0303
      I2=LZ-TQ(5,J1)                                                    PLEK0304
      I1=LZ-TQ(4,J1)                                                    PLEK0305
      IF(I1.GT.LP) GOTO 5                                               PLEK0306
      IF(I2.GT.LP) I2=LP+1                                              PLEK0307
      I3=TQ(1,J1)                                                       PLEK0308
      I4=U(7,I3)                                                        PLEK0309
      IF(I4.LE.0) GO TO 5                                               PLEK0310
      I1=LZ-TQ(4,J1)                                                    PLEK0311
      IF(I1.GE.I2) GO TO 15                                             PLEK0312
      I=I2-1                                                            PLEK0313
      DO 6 J=I1,I                                                       PLEK0314
      IF(QZ(4,J1).LT.1000000.OR.IP.NE.0.OR.U(2,I3).NE.820) ZG(I4,J)=ZG  PLEK0315
     1(I4,J)+U(2,I3)                                                    PLEK0316
      IF(QZ(4,J1).GE.1000000.AND.IP.EQ.0.AND.U(2,I3).EQ.820) ZG(I4,J)=ZGPLEK0317
     1(I4,J)+U(2,I3)*2                                                  PLEK0318
    6 CONTINUE                                                          PLEK0319
      IF(I2.EQ.LP+1) GOTO 5                                             PLEK0320
   15 IF(QZ(4,J1).LT.1000000.OR.IP.NE.0.OR.U(2,I3).NE.820) ZG(I4,I2)=ZG PLEK0321
     1(I4,I2)+QZ(1,J1)-U(2,I3)*(I2-I1)                                  PLEK0322
      IF(QZ(4,J1).GE.1000000.AND.IP.EQ.0.AND.U(2,I3).EQ.820) ZG(I4,I2)= PLEK0323
     1ZG(I4,I2)+QZ(1,J1)-U(2,I3)*2*(I2-I1)                              PLEK0324
    5 CONTINUE                                                          PLEK0325
      IU=0                                                              PLEK0326
      I1=0                                                              PLEK0327
      DO 7 I=1,NUC                                                      PLEK0328
      I1=I1+1                                                           PLEK0329
      I2=I1                                                             PLEK0330
      DO 8 J=1,LP                                                       PLEK0331
      IF(ZG(I,J).LE.0) GO TO 8                                          PLEK0332
      I1=I1+1                                                           PLEK0333
      IF(I1.GT.LMZP) STOP 111                                           PLEK0334
      ZP(1,I1)=J                                                        PLEK0335
      ZP(2,I1)=ZG(I,J)                                                  PLEK0336
    8 ZG(I,J)=0                                                         PLEK0337
      IF(I2.GE.I1) GO TO 9                                              PLEK0338
      ZP(1,I2)=I                                                        PLEK0339
      ZP(2,I2)=I1-I2                                                    PLEK0340
      IU=IU+1                                                           PLEK0341
      GO TO 7                                                           PLEK0342
    9 I1=I1-1                                                           PLEK0343
    7 CONTINUE                                                          PLEK0344
      NZ=NZ+1                                                           PLEK0345
      IF(NZ.GT.LIMZ) STOP 112                                           PLEK0346
      AZ(1,NZ)=K                                                        PLEK0347
      AZ(2,NZ)=LZ                                                       PLEK0348
      AZ(8,NZ)=CON                                                      PLEK0349
      MP1=MP-MP/10000*10000                                             PLEK0350
      KPP1=KPP-KPP/10000*10000                                          PLEK0351
      IF(MP1.LT.KPP1) J=0                                               PLEK0352
      IF(MP1.LT.KPP1) GOTO 17                                           PLEK0353
      CALL KALDNI(MP,0,NXZ)                                             PLEK0354
      DO 16 J=1,LP1                                                     PLEK0355
      IF(KDP(J).EQ.NXZ(1,1)) GOTO 17                                    PLEK0356
   16 CONTINUE                                                          PLEK0357
   17 AZ(3,NZ)=J-LP                                                     PLEK0358
      IF(IP.EQ.0) GOTO 80                                               PLEK0359
      IF(NF.EQ.6.AND.IPRZ.EQ.0) AZ(3,NZ)=1                              PLEK0360
      IF(NF.EQ.6.AND.IPRZ.EQ.1) AZ(3,NZ)=2                              PLEK0361
   80 AZ(4,NZ)=IP                                                       PLEK0362
      IF(PRG.EQ.1) AZ(4,NZ)=8                                           PLEK0363
      AZ(6,NZ)=IU                                                       PLEK0364
      IF(AZ(4,NZ).NE.0.AND.AZ(4,NZ).NE.8) GOTO 10                       PLEK0365
      AZ(5,NZ)=JO+1                                                     PLEK0366
      DO 11 J=1,I1                                                      PLEK0367
      JO=JO+1                                                           PLEK0368
      IF(JO.GT.LIMO) STOP 113                                           PLEK0369
      ZOP(1,JO)=ZP(1,J)                                                 PLEK0370
   11 ZOP(2,JO)=ZP(2,J)                                                 PLEK0371
      GO TO 2                                                           PLEK0372
   10 JD=JD+1                                                           PLEK0373
      AZ(5,NZ)=JD                                                       PLEK0374
      WRITE (ND,rec=JD) I1,((ZP(I,J),I=1,2),J=1,I1)                     PLEK0375
    2 CONTINUE                                                          PLEK0376
    4 REWIND NF1                                                        PLEK0377
      RETURN                                                            PLEK0378
      END                                                               PLEK0379
      SUBROUTINE PERPLN(NZ,NPZ,LP,LD,NUC,JO,ZG,UC,AZ,ZOP)               PLEK0380
      INTEGER*2 WPR(5000)                                               PLEK0381
      INTEGER*2 AZ(8,1),ZOP(2,1)                                        PLEK0382
      INTEGER ZG(100,1),UC(1)                                           PLEK0383
      REAL MINMAX,MAX                                                   PLEK0384
      CONTINUE                                                          PLEK0385
      CONTINUE                                                          PLEK0386
      CONTINUE                                                          PLEK0387
      CONTINUE                                                          PLEK0388
      CONTINUE                                                          PLEK0389
      MINMAX=10000.0                                                    PLEK0390
      DO 20 K=1,NZ                                                      PLEK0391
      WPR(K)=AZ(8,K)                                                    PLEK0392
   20 CONTINUE                                                          PLEK0393
      DO 10 I=1,NUC                                                     PLEK0394
      DO 10 J=1,LP                                                      PLEK0395
   10 ZG(I,J)=0                                                         PLEK0396
      NV=100000/NZ                                                      PLEK0397
      DO 8 KV=1,NV                                                      PLEK0398
      DO 1 K=1,NZ                                                       PLEK0399
      IF(AZ(4,K).NE.0.AND.AZ(4,K).NE.8) GO TO 1                         PLEK0400
      CALL genslpl(W)                                                   PLEK0401
      NW=AZ(2,K)                                                        PLEK0402
      IF(K.GT.NPZ)  NW=NW+LD                                            PLEK0403
      N=AZ(3,K)-NW                                                      PLEK0404
      IF(N.LT.0) N=0                                                    PLEK0405
      ID=INT(N*W)                                                       PLEK0406
      AZ(8,K)=ID+NW                                                     PLEK0407
      J2=AZ(5,K)-1                                                      PLEK0408
      J1=AZ(6,K)                                                        PLEK0409
      DO 2 I=1,J1                                                       PLEK0410
      J2=J2+1                                                           PLEK0411
      J=ZOP(1,J2)                                                       PLEK0412
      J3=ZOP(2,J2)                                                      PLEK0413
      DO 14 I1=1,J3                                                     PLEK0414
      J2=J2+1                                                           PLEK0415
      J4=ZOP(1,J2)+ID                                                   PLEK0416
      IF(J4.LE.0.OR.J4.GT.LP) GOTO 14                                   PLEK0417
      KZG=ZG(J,J4)+ZOP(2,J2)                                            PLEK0418
      ZG(J,J4)=ZG(J,J4)+ZOP(2,J2)                                       PLEK0419
   14 CONTINUE                                                          PLEK0420
    2 CONTINUE                                                          PLEK0421
    1 CONTINUE                                                          PLEK0422
      MAX=0.0                                                           PLEK0423
      DO 3 I=1,NUC                                                      PLEK0424
      IF(UC(I).EQ.0) R=0.0                                              PLEK0425
      IF(UC(I).EQ.0) PRINT 12,I                                         PLEK0426
      IF(UC(I).EQ.0) GOTO 30                                            PLEK0427
      R=1.0/UC(I)                                                       PLEK0428
   30 DO 3 J=1,LP                                                       PLEK0429
      D1=ZG(I,J)*R                                                      PLEK0430
      IF(D1.GT.MAX) MAX=D1                                              PLEK0431
    3 ZG(I,J)=0                                                         PLEK0432
      IF(MAX.GE.MINMAX) GOTO 8                                          PLEK0433
      MINMAX=MAX                                                        PLEK0434
      DO 5 I=1,NZ                                                       PLEK0435
    5 AZ(7,I)=AZ(8,I)                                                   PLEK0436
      IF(MINMAX.LE.1.00) GOTO 6                                         PLEK0437
    8 CONTINUE                                                          PLEK0438
    6 DO 4 K=1,NZ                                                       PLEK0439
      IF(AZ(4,K).GT.0) GO TO 4                                          PLEK0440
      J2=AZ(5,K)-1                                                      PLEK0441
      J1=AZ(6,K)                                                        PLEK0442
      DO 9 I=1,J1                                                       PLEK0443
      J2=J2+1                                                           PLEK0444
      J=ZOP(1,J2)                                                       PLEK0445
      J3=ZOP(2,J2)                                                      PLEK0446
      DO 15 L=1,J3                                                      PLEK0447
      J2=J2+1                                                           PLEK0448
      J4=ZOP(1,J2)+AZ(7,K)-AZ(2,K)                                      PLEK0449
   12 FORMAT (1X,8I8)                                                   PLEK0450
      IF(J4.LE.0.OR.J4.GT.LP) GOTO 15                                   PLEK0452
      KZG=ZG(J,J4)+ZOP(2,J2)                                            PLEK0453
      ZG(J,J4)=ZG(J,J4)+ZOP(2,J2)                                       PLEK0454
   15 CONTINUE                                                          PLEK0455
    9 CONTINUE                                                          PLEK0456
      AZ(8,K)=WPR(K)                                                    PLEK0457
    4 CONTINUE                                                          PLEK0458
         RETURN                                                         PLEK0459
      END                                                               PLEK0460
      SUBROUTINE OBPLAN(ND,LP,LD,NZ,NPZ,NUC,AZ,ZP,ZG,UC)                PLEK0461
      INTEGER*2 AZ(8,1),ZP(2,1)                                         PLEK0462
      INTEGER ZG(100,1),UC(1)                                           PLEK0463
      INTEGER*2 WPR(5000)                                               PLEK0464
   20 FORMAT(3I3)                                                       PLEK0465
      DO 10 J=1,NZ                                                      PLEK0466
      WPR(J)=AZ(8,J)                                                    PLEK0467
      AZ(8,J)=0                                                         PLEK0468
      IF(AZ(4,J).LE.0.OR.AZ(4,J).EQ.8) AZ(8,J)=10000                    PLEK0469
   10 CONTINUE                                                          PLEK0470
      DO 8 N=1,NZ                                                       PLEK0471
      I6=10000                                                          PLEK0472
      DO 9 I=1,NZ                                                       PLEK0473
      IF(AZ(8,I).GT.0) GOTO 9                                           PLEK0474
      K6=AZ(4,I)                                                        PLEK0475
      K2=WPR(I)                                                         PLEK0476
      K4=AZ(3,I)                                                        PLEK0477
      K3=0                                                              PLEK0478
      K1=AZ(2,I)                                                        PLEK0479
      IF(I.GT.NPZ) K3=1                                                 PLEK0480
      IF(I.GT.NPZ) K1=AZ(3,I)                                           PLEK0481
      IF(I.GT.NPZ) K4=AZ(2,I)                                           PLEK0482
      IF(K6-I6) 11,35,9                                                 PLEK0483
   35 IF(K3-I3) 11,12,9                                                 PLEK0484
  12  IF(K2-I2) 11,13,9                                                 PLEK0485
  13  IF(K4-I4) 11,14,9                                                 PLEK0486
  14  IF(K1-I1) 11,9,9                                                  PLEK0487
   11 I1=K1                                                             PLEK0488
      I2=K2                                                             PLEK0489
      I3=K3                                                             PLEK0490
      I4=K4                                                             PLEK0491
      I6=K6                                                             PLEK0492
      I0=I                                                              PLEK0493
    9 CONTINUE                                                          PLEK0494
      IF(I1.GT.9999) GOTO 15                                            PLEK0495
      I=I0                                                              PLEK0496
   24 CONTINUE                                                          PLEK0497
      AZ(8,I)=10000                                                     PLEK0498
      I2=AZ(5,I)                                                        PLEK0499
      READ (ND,rec=I2) J5,((ZP(I1,J),I1=1,2),J=1,J5)                    PLEK0500
      I1=AZ(6,I)                                                        PLEK0501
      J5=0                                                              PLEK0502
      IF(I.GT.NPZ) J5=LD                                                PLEK0503
    5 I4=0                                                              PLEK0504
      DO 2 J1=1,I1                                                      PLEK0505
      I4=I4+1                                                           PLEK0506
      J=ZP(1,I4)                                                        PLEK0507
      J2=ZP(2,I4)                                                       PLEK0508
      DO 3 J3=1,J2                                                      PLEK0509
      I4=I4+1                                                           PLEK0510
      J4=ZP(1,I4)+J5                                                    PLEK0511
      IF(J4.GT.LP) GO TO 3                                              PLEK0512
      IF(ZG(J,J4)+ZP(2,I4).LE.UC(J)) GO TO 3                            PLEK0513
      J5=J5+1                                                           PLEK0514
      GO TO 5                                                           PLEK0515
    3 CONTINUE                                                          PLEK0516
    2 CONTINUE                                                          PLEK0517
      AZ(7,I)=0                                                         PLEK0518
      IF(J5.GE.LP) GOTO 8                                               PLEK0519
      I4=0                                                              PLEK0520
      DO 6 J1=1,I1                                                      PLEK0521
      I4=I4+1                                                           PLEK0522
      J=ZP(1,I4)                                                        PLEK0523
      J2=ZP(2,I4)                                                       PLEK0524
      DO 7 J3=1,J2                                                      PLEK0525
      I4=I4+1                                                           PLEK0526
      J4=ZP(1,I4)+J5                                                    PLEK0527
      IF(J4.GT.LP) GO TO 7                                              PLEK0528
      ZG(J,J4)=ZG(J,J4)+ZP(2,I4)                                        PLEK0529
    7 CONTINUE                                                          PLEK0530
    6 CONTINUE                                                          PLEK0531
      AZ(7,I)=AZ(2,I)+J5                                                PLEK0532
      GOTO 8                                                            PLEK0533
   25 AZ(7,I)=0                                                         PLEK0534
      AZ(8,I)=10000                                                     PLEK0535
    8 CONTINUE                                                          PLEK0536
   15 CONTINUE                                                          PLEK0537
      RETURN                                                            PLEK0538
      END                                                               PLEK0539
      SUBROUTINE KALDNI(ND,LP,KDP)                                      PLEK0540
      INTEGER*2 KM(13)/0 ,31,28,31,30,31,30,31,31,30,31,30,31/,         PLEK0541
     1KDP(1  ),R/'R '/                                                  PLEK0542
      INTEGER*2 NDN(7)/'R ','R ','R ','R ','R ','N ','N '/              PLEK0543
      INTEGER*2 NDP(9)/ 101, 201, 803, 105, 205, 905, 710, 711, 811/    PLEK0544
      ID=ND/1000000                                                     PLEK0545
      M=ND/10000-ID*100                                                 PLEK0546
      IG=ND-ID*1000000-M*10000                                          PLEK0547
    1 FORMAT(20X,20HHEBEPHO ÇAÄAHA ÄATA:,I9)                            PLEK0548
      IF(ID*(13-M)*IG.GT.0) GO TO 14                                    PLEK0549
      GO TO 15                                                          PLEK0550
   14 CONTINUE                                                          PLEK0551
      IG1=IG/4                                                          PLEK0552
      IF(IG.EQ.IG1*4) KM(3)=29                                          PLEK0553
      M1=0                                                              PLEK0554
      DO 2 I=1,M                                                        PLEK0555
    2 M1=M1+KM(I)                                                       PLEK0556
      M2=(IG-1977)*365+(IG-1976)/4+M1+ID-2                              PLEK0557
      IF(IG.NE.IG1*4) GO TO 3                                           PLEK0558
      M2=M2-1                                                           PLEK0559
    3 IN=M2-(M2/7)*7                                                    PLEK0560
      IF(IN.EQ.0) IN=7                                                  PLEK0561
      LP1=LP*2                                                          PLEK0562
      IF(LP.NE.0) GO TO 12                                              PLEK0563
      LP1=1                                                             PLEK0564
      GO TO 13                                                          PLEK0565
   12 ID1=ID                                                            PLEK0566
      M1=M                                                              PLEK0567
      IN1=IN                                                            PLEK0568
      K=LP+2                                                            PLEK0569
   11 IF(NDN(IN1).NE.R) GO TO 8                                         PLEK0570
      DO 10 I=1,8                                                       PLEK0571
      M2=ID1*100+M1                                                     PLEK0572
      IF(M2.EQ.NDP(I)) GO TO 8                                          PLEK0573
   10 CONTINUE                                                          PLEK0574
      IF(K.EQ.LP+2.AND.ID.NE.ID1) K=K-1                                 PLEK0575
      K=K-1                                                             PLEK0576
      KDP(K)=ID1*1000+M1*10+IN1                                         PLEK0577
    8 ID1=ID1-1                                                         PLEK0578
      IF(ID1.GT.0) GO TO 9                                              PLEK0579
      M1=M1-1                                                           PLEK0580
      IF(M1.EQ.0) M1=12                                                 PLEK0581
      ID1=KM(M1+1)                                                      PLEK0582
    9 IN1=IN1-1                                                         PLEK0583
      IF(IN1.EQ.0) IN1=7                                                PLEK0584
      IF(K.GT.1) GO TO 11                                               PLEK0585
   13 K=LP                                                              PLEK0586
    7 IF(NDN(IN).NE.R) GO TO 4                                          PLEK0587
      DO 6 I=1,8                                                        PLEK0588
      M1=ID*100+M                                                       PLEK0589
      IF(M1.EQ.NDP(I)) GO TO 4                                          PLEK0590
    6 CONTINUE                                                          PLEK0591
      K=K+1                                                             PLEK0592
      KDP(K)=ID*1000+M*10+IN                                            PLEK0593
    4 ID=ID+1                                                           PLEK0594
      IF(ID.LE.KM(M+1)) GO TO 5                                         PLEK0595
      ID=1                                                              PLEK0596
      M=M+1                                                             PLEK0597
      IF(M.EQ.13) M=1                                                   PLEK0598
    5 IN=IN+1                                                           PLEK0599
      IF(IN.EQ.8) IN=1                                                  PLEK0600
      IF(K.LT.LP1) GO TO 7                                              PLEK0601
      KM(3)=28                                                          PLEK0602
   15 RETURN                                                            PLEK0603
      END                                                               PLEK0604
      SUBROUTINE PLTPGN(N1,N2,N3,N4,N5,NU,NUC,LP,NN,NK,NZ,NP,A,S,P,TQ,QZPLEK0605
     1,TQC,NXZ,IW,KDP,AZ,ZG,U,K5,K55)                                   PLEK0606
      INTEGER*2 A(26,200),S(2,200),P(2,200),TQ(6,500),TQC(4,500),NXZ(10 PLEK0607
     1,4),AZ(8,4000),ZG1(100,60),KDP(300)                               PLEK0608
      INTEGER*2 CD/'CO'/                                                PLEK0609
      INTEGER*2 KDPS(300),KLD(1)                                        PLEK0610
      INTEGER*2 PORT/' Ï'/,U(20,1)                                      PLEK0611
      INTEGER*2 MET/'ÑÏ'/                                               PLEK0612
      INTEGER*2 MET1/'  '/                                              PLEK0613
      INTEGER F(100,3)/300*0/,ZG(100,1)                                 PLEK0614
      INTEGER*4 QZ(4,500),IW(100)                                       PLEK0615
      INTEGER*2 PTQ(500),RAZ                                            PLEK0616
  150 FORMAT(//42X,'CBOÄKA O HAËÈ×ÈÈ ÇAKAÇOB HA ',I8,41X//1X,127('-')/1XPLEK0617
     1,'!Ö !KOËÈ× ÇAKAÇOB !KOËÈ× BÛÏOËHEH!KOËÈ× ÇAKAÇOB !KOËÈ× ÏEPEØEÄØÈPLEK0618
     2X  !KOËÈ×ECTBO  ÇAKAÇOB B ÏOPTÔEËE !KOËÈ× ÇAKAÇOB !OCTA-!KOËÈ×!'/1PLEK0619
     3X,'!  !B ÏËAHE ÏPEÄ  !HÛX ÇAKAÇOB   !B ÏËAHE TEKÓ- !ÇAKAÇOB ÈÇ ÏËAPLEK0620
     4HA  !',31X,'!BKËÞ× B ÏËAH  !ËOCÜ !BÛÏOË!'/1X,'!E ! MECßÖA       ! PLEK0621
     5B ÏËAHE      !ÙEÃO MECßÖA   !ÏPEÄÛÄÓÙEÃO MECßÖA!',10X,'ÖEXA',17X,'PLEK0622
     6!ÈÇ ÏOPTÔEËß   !B    !HEH- !'/1X,'!  !',110('-'),'!ÏOPT-!HÛX B!'/1PLEK0623
     7X,'!X ! ÇA-! ÇA-! BÛ-! ÇA-! ÇA-! BÛ-! ÇA-! ÇA-! BÛ-!KOË-!ÇAÄEË !BÛPLEK0624
     8ÏÓCK!ÁEÇ !ÏPÈ-!CÏEÖ!C ÇAÃOTOBK !ÈTOÃ! ÇA-! ÇA-! BÛ-!ÔEËE !ÏOPT-!'/PLEK0625
     91X,'!  !ÏÓCK!ÄEË !ÏÓCK!ÏÓCK!ÄEË !ÏÓCK!ÏÓCK!ÄEË !ÏÓCK!BO  ! H/×  ! PLEK0626
     AH/×  !ÇA- !OCTA!ÇA- !',11('-'),'!KOË-!ÏÓCK!ÄEË !ÏÓCK!ÖEXA !ÔEËE !'PLEK0627
     B/1X,'!  !',10('    !'),2('      !'),'ÃOT !HOB !KAÇÛ!KOË ! H/×  !BOPLEK0628
     C  !',3('    !'),'     !ÖEXA !'/1X,127('-'))                       PLEK0629
  151 FORMAT(1H ,'!17!')                                                PLEK0630
  152 FORMAT(1H ,'!15!')                                                PLEK0631
  153 FORMAT(1H+,4X,10(I4,1X),I6,1X,I6,1X,4(I4,1X),I6,1X,4(I4,1X),2(I5,1PLEK0632
     1X))                                                               PLEK0633
  625 FORMAT(1H+,7X,I5,10X,I5,10X,I5,61X,I5)                            PLEK0634
   53 FORMAT(10X,'B ÏOPTÔEËE CTAËO',I5,' ÇAKAÇOB'//)                    PLEK0635
  530 FORMAT(10X,'B ÏOPTÔEËE ÁÛËO',I5,' ÇAKAÇOB')                       PLEK0636
 2000 FORMAT(10X,'B ÏËAHE ÁÛËO',I5,' ÇAKAÇOB')                          PLEK0637
 2001 FORMAT(10X,'B ÏËAHE CTAËO',I5,' ÇAKAÇOB'/////)                    PLEK0638
  531 FORMAT(//////15X,'ÏPOTOKOË PAC×ETA ÏËAHA',1X,I8)                  PLEK0639
  200 FORMAT (10I8)                                                     PLEK0640
  300 FORMAT(1X,120(1H-))                                               PLEK0641
  301 FORMAT(1H )                                                       PLEK0642
      DATA K100/0/,K101/0/,K102/0/,K103/0/,K104/0/,K105/0/,K106/0/,K107/PLEK0643
     10/,K108/0/,K109/0/,K110/0/,K111/0/,K112/0/,K113/0/,K114/0/,K115/0/PLEK0644
     2,K116/0/,K117/0/,K118/0/,K119/0/,K120/0/,K121/0/,K122/0/          PLEK0645
      DATA K500/0/,K501/0/,K502/0/,K503/0/,K504/0/,K505/0/,K506/0/,K507/PLEK0646
     10/,K508/0/,K509/0/,K510/0/,K511/0/,K512/0/,K513/0/,K514/0/,K515/0/PLEK0647
     2,K516/0/,K517/0/,K518/0/,K519/0/,K520/0/,K521/0/,K522/0/          PLEK0648
      DATA KIT1/0/,KIT2/0/,KIT3/0/,KIT4/0/,KIT5/0/,KIT6/0/,KIT7/0/,KIT8/PLEK0649
     10/,KOLPLS/0/,KOLPLT/0/                                            PLEK0650
      DATA NSLES/83/                                                    PLEK0651
      ij=-1
      NQ=0                                                              PLEK0653
      KZI=0                                                             PLEK0654
      NI=0                                                              PLEK0655
      K=0                                                               PLEK0656
      MS=0                                                              PLEK0657
      LP1=LP+LP                                                         PLEK0658
      WRITE(N4) LP,NN,NK                                                PLEK0659
      WRITE(N4) (KDP(I),I=1,LP1)                                        PLEK0660
      NKPT=NK                                                           PLEK0661
      CALL KALDNI(NKPT,0,KLD)                                           PLEK0662
      DO 604 L=LP,LP1                                                   PLEK0663
      IF(NKPT.EQ.KDP(L)) GOTO 605                                       PLEK0664
  604 CONTINUE                                                          PLEK0665
  605 NKPT=L-LP                                                         PLEK0666
  800 FORMAT (I8)                                                       PLEK0667
      NF=N2                                                             PLEK0669
      IF(NP.LE.0) GO TO 100                                             PLEK0670
      NF=N1                                                             PLEK0671
      K=0                                                               PLEK0672
   60 READ(N1) J,I,I                                                    PLEK0673
      KOH=J                                                             PLEK0674
      LP2=J*2                                                           PLEK0675
      READ (N1)(KDPS(I),I=1,LP2)                                        PLEK0676
      NKPS=I                                                            PLEK0677
      CALL KALDNI(NKPS,0,KLD)                                           PLEK0678
      DO 601 I1=J,LP2                                                   PLEK0679
      IF(NKPS.EQ.KDPS(I1)) GOTO 603                                     PLEK0680
  601 CONTINUE                                                          PLEK0681
  603 NKPS=I1-J                                                         PLEK0682
    3 LU=J                                                              PLEK0684
  100 NJ=NZ+2                                                           PLEK0685
      MW=0                                                              PLEK0686
      DO 1 L=1,NJ                                                       PLEK0687
      M=L+MW                                                            PLEK0688
      IF(NF.EQ.N1) GO TO 15                                             PLEK0689
   14 READ(NF) NXZ,(IW(I),I=1,NU),IQ,IP,ID,LZ,MP,NA,NS,NT               PLEK0690
      KZI=KZI+1                                                         PLEK0691
      GO TO 18                                                          PLEK0692
   15 READ(NF) NXZ,(IW(I),I=1,NU),IQ,IP,ID,LD,LZ,LF,MP,MS,NA,NS,NT      PLEK0693
      KOLPLS=KOLPLS+1                                                   PLEK0694
   18 IF(NA.LE.-1000) GO TO 25                                          PLEK0695
      READ(NF) ((A(I,J),I=1,26),J=1,NA)                                 PLEK0696
      READ(NF) ((S(I,J),I=1,2),J=1,NS),((P(I,J),I=1,2),J=1,NS)          PLEK0697
       READ(NF) ((TQ(I,J),I=1,6),J=1,NT),((QZ(I,J),I=1,4),J=1,NT)       PLEK0698
      READ(NF) ((TQC(I,J),I=1,4),J=1,NT)                                PLEK0699
      K=K+1                                                             PLEK0700
      N=A(10,1)                                                         PLEK0701
      N=S(1,N)                                                          PLEK0702
      N=A(12,N)+A(13,N)-1                                               PLEK0703
      N=TQ(1,N)                                                         PLEK0704
      N=U(1,N)                                                          PLEK0705
      IF(NF.EQ.N1)  GOTO 154                                            PLEK0706
      IF(N.EQ.17) K117=K117+1                                           PLEK0707
      IF(N.EQ.15) K517=K517+1                                           PLEK0708
      IF(NXZ(5,1).EQ.MET.AND.N.EQ.17) K114=K114+1                       PLEK0709
      IF(NXZ(5,1).EQ.MET.AND.N.EQ.15) K514=K514+1                       PLEK0710
 9000 IF(IP.EQ.9.AND.N.EQ.17) K113=K113+1                               PLEK0711
      IF(IP.EQ.9.AND.N.EQ.15) K513=K513+1                               PLEK0712
      IF(IP.EQ.9.AND.IQ-QZ(2,1)-TQC(2,1).LE.0) GOTO 4                   PLEK0713
      IF(N.EQ.99.AND.IQ-QZ(2,1)-TQC(2,1).LE.0) GOTO 4                   PLEK0714
      IF(IP.EQ.9) GOTO 900                                              PLEK0715
      IF(N.EQ.99) GOTO 900                                              PLEK0716
      IF(N.EQ.15) GOTO 619                                              PLEK0717
      IF(A(9,NA).NE.99) GOTO 6222                                       PLEK0728
      IF(TQC(2,NT).LE.0) GOTO 6222                                      PLEK0729
      K112=K112+1                                                       PLEK0730
 6222 IF(TQ(2,2).NE.512.AND.TQ(2,2).NE.513) GOTO 620                    PLEK0718
      IF(TQC(2,2).LE.0) GOTO 620                                        PLEK0719
      K112=K112+1                                                       PLEK0720
      GOTO 155                                                          PLEK0721
  620 K115=K115+1                                                       PLEK0722
      DO 621 I=1,NT                                                     PLEK0723
      IF(TQC(2,I).LE.0) GOTO 621                                        PLEK0724
      K116=K116+QZ(1,I)                                                 PLEK0725
  621 CONTINUE                                                          PLEK0726
      GOTO 155                                                          PLEK0727
  619 IF(A(9,NA).NE.99) GOTO 622                                        PLEK0728
      IF(TQC(2,NT).LE.0) GOTO 622                                       PLEK0729
      K512=K512+1                                                       PLEK0730
      GOTO 155                                                          PLEK0731
  622 K515=K515+1                                                       PLEK0732
      DO 623 I=1,NT                                                     PLEK0733
      IF(TQC(2,I).LE.0) GOTO 623                                        PLEK0734
      K516=K516+QZ(1,I)                                                 PLEK0735
  623 CONTINUE                                                          PLEK0736
      GOTO 155                                                          PLEK0737
  154 IF(N.EQ.15) GOTO 600                                              PLEK0738
      KIT1=KIT1+1                                                       PLEK0739
      IF(LF.LE.NKPS) K102=K102+1                                        PLEK0740
      IF(LF.LE.NKPS) GOTO 155                                           PLEK0741
      IF(TQ(2,2).NE.512.AND.TQ(2,2).NE.513) GOTO 55555                  PLEK0743
      IF(TQC(2,2).LE.0.AND.TQC(1,2).LT.0) K101=K101+1                   PLEK0744
      IF(TQC(2,2).LE.0.AND.TQC(1,2).LT.0) GOTO 155                      PLEK0745
      IF(TQC(2,2).GT.0) K100=K100+1                                     PLEK0746
      IF(TQC(2,2).GT.0) GOTO 155                                        PLEK0747
      KSCP=0                                                            PLEK0748
      DO 650 JJ=3,NT                                                    PLEK0749
      IF(TQC(2,JJ).EQ.0.AND.TQC(1,JJ).LE.0) GOTO 650                    PLEK0750
      IF(TQ(4,JJ).LE.KOH) KSCP=1                                        PLEK0751
  650 CONTINUE                                                          PLEK0752
      IF(KSCP.EQ.0) K100=K100+1                                         PLEK0753
      IF(KSCP.EQ.1) K101=K101+1                                         PLEK0754
      GOTO 155                                                          PLEK0755
  600 KIT5=KIT5+1                                                       PLEK0756
      IF(LF.LE.NKPS) K502=K502+1                                        PLEK0757
      IF(LF.LE.NKPS) GOTO 155                                           PLEK0758
55555 IF(TQ(2,NT).NE.501) K101=K101+1                                   PLEK0759
      IF(TQ(2,NT).NE.501) GOTO 155                                      PLEK0760
      IF(TQC(2,NT).LE.0.AND.TQC(1,NT).LT.0) K101=K101+1                 PLEK0761
      IF(TQC(2,NT).LE.0.AND.TQC(1,NT).LT.0) GOTO 155                    PLEK0762
      IF(TQC(2,NT).GT.0) K100=K100+1                                    PLEK0763
      IF(TQC(2,NT).GT.0) GOTO 155                                       PLEK0764
      KSCP=0                                                            PLEK0765
      KTQP=NT-1                                                         PLEK0766
      N11=A(10,NA)                                                      PLEK0767
      IF(N11.EQ.0) IZIK=2                                               PLEK0768
      IF(N11.NE.0) IZIK=A(12,S(1,N11))+A(13,S(1,N11))                   PLEK0769
      DO 651 JJ=IZIK,KTQP                                               PLEK0770
      IF(TQC(2,JJ).EQ.0.AND.TQC(1,JJ).LE.0) GOTO 651                    PLEK0771
      IF(TQ(4,JJ).LE.KOH) KSCP=1                                        PLEK0772
  651 CONTINUE                                                          PLEK0773
      IF(KSCP.EQ.0) K100=K100+1                                         PLEK0774
      IF(KSCP.EQ.1) K101=K101+1                                         PLEK0775
  155 CONTINUE                                                          PLEK0776
      DO 720 KOL=1,2                                                    PLEK0777
      DO 49 J3=2,NA                                                     PLEK0778
      J2=A(12,J3)+A(13,J3)-1                                            PLEK0779
      J1=A(12,J3)                                                       PLEK0780
      NPOS=J2-1                                                         PLEK0781
      DO 54 J6=J1,J2                                                    PLEK0782
      KJ=J2-J6                                                          PLEK0783
      J4=J1+KJ                                                          PLEK0784
      IF(TQC(2,J4).NE.0.AND.J4.EQ.J2) GOTO 157                          PLEK0785
      IF(TQC(2,J4).NE.0) GOTO 54                                        PLEK0786
      IF(KJ.EQ.0) GOTO 54                                               PLEK0787
      GOTO 700                                                          PLEK0788
  157 NOMP=A(14,J3)                                                     PLEK0789
      KOLP=A(15,J3)                                                     PLEK0790
      KON=NOMP+KOLP-1                                                   PLEK0791
      IF(NA.EQ.3.AND.A(9,2).EQ.A(9,3)) GOTO 54                          PLEK0792
      DO 710 J10=NOMP,KON                                               PLEK0793
      NSB=P(1,J10)                                                      PLEK0794
      IF(NSB.EQ.1) GOTO 710                                             PLEK0795
      NSB=A(12,NSB)                                                     PLEK0796
      IF(TQC(2,NSB).NE.0) GOTO 710                                      PLEK0797
      KJ=KJ+1                                                           PLEK0798
      GOTO 700                                                          PLEK0799
  710 CONTINUE                                                          PLEK0800
      GOTO 54                                                           PLEK0801
  700 KJ=KJ+1                                                           PLEK0802
      DO 55 JK=1,KJ                                                     PLEK0803
      J5=J1+JK-1                                                        PLEK0804
      IF(TQC(2,J5).EQ.0) GOTO 55                                        PLEK0805
      TQC(2,J5)=0                                                       PLEK0806
      TQC(3,J5)=0                                                       PLEK0807
      TQC(1,J5)=KOH                                                     PLEK0808
   55 CONTINUE                                                          PLEK0809
      GOTO 49                                                           PLEK0810
   54 CONTINUE                                                          PLEK0811
   49 CONTINUE                                                          PLEK0812
  720 CONTINUE                                                          PLEK0813
      J3=A(10,1)                                                        PLEK0814
      J3=A(11,1)+J3-1                                                   PLEK0815
      J3=S(1,J3)                                                        PLEK0816
      J8=A(12,J3)+A(13,J3)-1                                            PLEK0817
      J1=A(12,J3)                                                       PLEK0818
      DO 1500 J6=J1,J8                                                  PLEK0819
      KJ=J8-J6                                                          PLEK0820
      J4=J1+KJ                                                          PLEK0821
      IF(TQ(2,J4)-TQ(2,J4)/100*100.NE.83) GOTO 1500                     PLEK0822
      R=(QZ(1,J4)+0.0)/U(2,TQ(1,J4))                                    PLEK0823
      J5=INT(R)                                                         PLEK0824
      IF(R-J5.LE.0.0) J5=J5-1                                           PLEK0825
      IF(NF.EQ.N2) J66=TQ(4,J4)-TQ(5,J4)                                PLEK0826
      IF(NF.EQ.N1) J66=TQ(5,J4)-TQ(4,J4)                                PLEK0827
      J7=J66-J5                                                         PLEK0828
      GOTO 1501                                                         PLEK0829
 1500 CONTINUE                                                          PLEK0830
      J7=0                                                              PLEK0831
 1501 NT1=NT                                                            PLEK0832
      NT2=0                                                             PLEK0833
      DO 1502 J=1,NT1                                                   PLEK0834
      NT2=NT2+1                                                         PLEK0835
      IF(NF.EQ.N2) GOTO 555                                             PLEK0836
      IF(J.EQ.1) GOTO 1520                                              PLEK0837
      IF(J.GT.J4.AND.J.LE.J8) GOTO 1520                                 PLEK0838
      TQ(4,NT2)=LF-TQ(4,J)-J7                                           PLEK0839
      IF(J.EQ.J4) GOTO 553                                              PLEK0840
      TQ(5,NT2)=LF-TQ(5,J)-J7                                           PLEK0841
      TQ(6,NT2)=LF-TQ(6,J)-J7                                           PLEK0842
      GOTO1502                                                          PLEK0843
 1520 CONTINUE                                                          PLEK0844
      TQ(4,NT2)=LF-TQ(4,J)                                              PLEK0845
  553 TQ(5,NT2)=LF-TQ(5,J)                                              PLEK0846
      TQ(6,NT2)=LF-TQ(6,J)                                              PLEK0847
      IF(NF.EQ.N1) GOTO1502                                             PLEK0848
  555 IF(J.EQ.1) GOTO 550                                               PLEK0849
      IF(J.GT.J4.AND.J.LE.J8) GOTO 550                                  PLEK0850
      TQ(4,NT2)=TQ(4,J)-J7                                              PLEK0851
      IF(J.EQ.J4) GOTO 533                                              PLEK0852
      TQ(5,NT2)=TQ(5,J)-J7                                              PLEK0853
      TQ(6,NT2)=TQ(6,J)-J7                                              PLEK0854
      GOTO 1502                                                         PLEK0855
  550 TQ(4,NT2)=TQ(4,J)                                                 PLEK0856
  533 TQ(5,NT2)=TQ(5,J)                                                 PLEK0857
      TQ(6,NT2)=TQ(6,J)                                                 PLEK0858
 1502 CONTINUE                                                          PLEK0859
      LZ=0                                                              PLEK0860
      DO 68 I=2,NT                                                      PLEK0861
      IF(TQC(2,I).EQ.0) GOTO 68                                         PLEK0862
      IF(TQ(4,I).GT.LZ) LZ=TQ(4,I)                                      PLEK0863
   68 PTQ(I)=0                                                          PLEK0864
      IF(LZ.EQ.0) LZ=LZ+1                                               PLEK0865
      IF(LZ.EQ.0) GOTO 888                                              PLEK0866
      NSBOR=A(10,1)                                                     PLEK0867
      KSBOR=NSBOR+A(11,1)-1                                             PLEK0868
      DO 70 I=NSBOR,KSBOR                                               PLEK0869
      PTQ(A(12,S(1,I)))=1                                               PLEK0870
   70 CONTINUE                                                          PLEK0871
      MIN=32000                                                         PLEK0872
      MAX=0                                                             PLEK0873
      DO 61 I=2,NA                                                      PLEK0874
      NMK=A(12,I)                                                       PLEK0875
      KMK=NMK+A(13,I)-1                                                 PLEK0876
      IF(PTQ(NMK).EQ.1) GOTO 61                                         PLEK0877
      KR=0                                                              PLEK0878
      DO 62 J=NMK,KMK                                                   PLEK0879
      IF(KR.EQ.1) GOTO 63                                               PLEK0880
      KOD=TQ(2,J)-TQ(2,J)/100*100                                       PLEK0881
      IF(KOD.EQ.NSLES) KR=1                                             PLEK0882
      IF(KOD.EQ.NSLES) GOTO 63                                          PLEK0883
      IF(TQC(2,J).EQ.0) GOTO 62                                         PLEK0884
      IF(TQ(6,J).LT.MIN) MIN=TQ(6,J)                                    PLEK0885
      GOTO 62                                                           PLEK0886
   63 IF(TQC(2,J).EQ.0) GOTO 62                                         PLEK0887
      IF(TQ(4,J).GT.MAX) MAX=TQ(4,J)                                    PLEK0888
      GOTO 61                                                           PLEK0889
   62 CONTINUE                                                          PLEK0890
   61 CONTINUE                                                          PLEK0891
      IF(MIN.EQ.32000.OR.MIN.LT.0) GOTO 7778                            PLEK0892
      IF(MAX.NE.0) GOTO 64                                              PLEK0893
      DO 91 I=NSBOR,KSBOR                                               PLEK0894
      NMK=A(12,S(1,I))                                                  PLEK0895
      KMK=NMK+A(13,S(1,I))-1                                            PLEK0896
      DO 92 J=NMK,KMK                                                   PLEK0897
      IF(TQC(2,J).EQ.0) GOTO 92                                         PLEK0898
      IF(TQ(4,J).GT.MAX) MAX=TQ(4,J)                                    PLEK0899
      GOTO 91                                                           PLEK0900
   92 CONTINUE                                                          PLEK0901
   91 CONTINUE                                                          PLEK0902
   64 RAZ=MIN-MAX                                                       PLEK0903
      IF(RAZ.LE.1) GOTO 7778                                            PLEK0904
      RAZ=RAZ-1                                                         PLEK0905
      DO 65 I=2,NA                                                      PLEK0906
      NMK=A(12,I)                                                       PLEK0907
      KMK=NMK+A(13,I)-1                                                 PLEK0908
      IF(PTQ(NMK).EQ.1) GOTO 65                                         PLEK0909
      PTQ(NMK)=1                                                        PLEK0910
      DO 66 J=NMK,KMK                                                   PLEK0911
      KOD=TQ(2,J)-TQ(2,J)/100*100                                       PLEK0912
      IF(KOD.EQ.NSLES) GOTO 65                                          PLEK0913
      IF(TQC(2,J).EQ.0) GOTO 66                                         PLEK0914
      DO 67 I2=4,6                                                      PLEK0915
   67 TQ(I2,J)=TQ(I2,J)-RAZ                                             PLEK0916
   66 CONTINUE                                                          PLEK0917
   65 CONTINUE                                                          PLEK0918
 7778 LZ=0                                                              PLEK0919
      DO 78 J=2,NT                                                      PLEK0920
      IF(TQC(2,J).EQ.0) GOTO78                                          PLEK0921
      IF(TQ(4,J).GT.LZ) LZ=TQ(4,J)                                      PLEK0922
   78 CONTINUE                                                          PLEK0923
      LZ=LZ+1                                                           PLEK0924
  888 IF(NF.EQ.N2) GOTO 1000                                            PLEK0925
      IF(K.LT.AZ(1,L).OR.K.GT.AZ(1,NP)) GO TO 4                         PLEK0926
 1000 J1=A(10,1)                                                        PLEK0927
      J2=A(11,1)+J1-1                                                   PLEK0928
      IF(NXZ(5,1).EQ.CD.AND.NF.EQ.N1) GOTO 900                          PLEK0929
      IF(NXZ(5,1).EQ.MET) GOTO 19                                       PLEK0930
      IF(TQC(1,1).LT.0.AND.IQ.EQ.0) GOTO 19                             PLEK0931
      IF(TQC(1,1).LT.0) TQC(2,1)=0                                      PLEK0932
      IF((IQ-QZ(2,1)-TQC(2,1)).GT.0) GOTO 19                            PLEK0933
   90 CONTINUE                                                          PLEK0934
      IF(NF.EQ.N2) GOTO4                                                PLEK0935
      GO TO 27                                                          PLEK0936
  190 DO 9 J3=J1,J2                                                     PLEK0937
      J=S(1,J3)                                                         PLEK0938
      J=A(12,J)+A(13,J)-1                                               PLEK0939
      IF (TQC(2,J).GT.0) GO TO 19                                       PLEK0940
    9 CONTINUE                                                          PLEK0941
      IF (NF.EQ.N2) GO TO 4                                             PLEK0942
      GO TO 27                                                          PLEK0943
  19  IF(NF.EQ.N2) GOTO10                                               PLEK0944
      IF(AZ(7,L).LE.0) GOTO13                                           PLEK0945
      LS=AZ(7,L)-LF                                                     PLEK0946
      DO 5 I=1,NT                                                       PLEK0947
      TQC(1,I)=TQC(1,I)-LU                                              PLEK0948
      LW=-LU                                                            PLEK0949
      IF(TQC(2,I).GT.0) LW=0                                            PLEK0950
      DO 7 J=4,6                                                        PLEK0951
    7 TQ(J,I)=AZ(7,L)-TQ(J,I)-LW                                        PLEK0952
    5 CONTINUE                                                          PLEK0953
      GO TO 6                                                           PLEK0954
   10 LF=AZ(7,M)                                                        PLEK0955
      IF(LF.LE.0) GOTO 13                                               PLEK0956
      DO 12 I=1,NT                                                      PLEK0957
      TQC(1,I)=TQC(1,I)-LU                                              PLEK0958
      LW=-LU                                                            PLEK0959
      IF(TQC(2,I).GT.0) LW=0                                            PLEK0960
      DO 12 J=4,6                                                       PLEK0961
   12 TQ(J,I)=LF-TQ(J,I)-LW                                             PLEK0962
      LD=LF-AZ(2,M)+LP+1                                                PLEK0963
      LD=KDP(LD)/10*100+((NN-NN/10000*10000)-(NN-NN/10000*10000)/100
     1*100)                                                             PLEK0964
    6 LF=AZ(7,M)                                                        PLEK0965
      LZ=AZ(2,M)                                                        PLEK0966
      IF(MS.EQ.-1.AND.AZ(7,M).GT.LP) GOTO 1600                          PLEK0967
      MS=0                                                              PLEK0968
 1600 CONTINUE                                                          PLEK0969
      IF(NXZ(5,1).NE.MET) QZ(2,1)=QZ(2,1)+TQC(2,1)                      PLEK0970
      TQC(2,1)=0                                                        PLEK0971
      LZ=0                                                              PLEK0972
      IPPZ=0                                                            PLEK0973
      DO 133 J=1,NT                                                     PLEK0974
      IF(NXZ(5,1).EQ.MET) QZ(3,J)=0                                     PLEK0975
      IF(QZ(4,J).EQ.99999) QZ(4,J)=0                                    PLEK0976
      IF(TQC(2,J).EQ.0) GOTO 133                                        PLEK0977
      IF(TQ(2,J).EQ.512.OR.TQ(2,J).EQ.513) IPPZ=1                       PLEK0978
      IF(TQ(2,J).EQ.512.OR.TQ(2,J).EQ.513) GOTO 133                     PLEK0979
      IF(LF-TQ(4,J).GT.LZ) LZ=LF-TQ(4,J)                                PLEK0980
  133 CONTINUE                                                          PLEK0981
      IF(IPPZ.EQ.1) LZ=LZ+30                                            PLEK0982
      IF(LZ.EQ.0) LF=1                                                  PLEK0983
      WRITE(N4) NXZ,(IW(I),I=1,NU),IQ,IP,ID,LD,LZ,LF,MP,MS,NA,NS,NT     PLEK1199
      WRITE(N4) ((A(I,J),I=1,26),J=1,NA)                                PLEK0985
      WRITE (N4) ((S(I,J),I=1,2),J=1,NS),((P(I,J),I=1,2),J=1,NS)        PLEK0986
      WRITE (N4) ((TQ(I,J),I=1,6),J=1,NT),((QZ(I,J),I=1,4),J=1,NT)      PLEK0987
      WRITE (N4) ((TQC(I,J),I=1,4),J=1,NT)                              PLEK0988
      KOLPLT=KOLPLT+1                                                   PLEK0989
      IF(NF.EQ.N1) GOTO 609                                             PLEK0990
      IF(N.EQ.15) GOTO 606                                              PLEK0991
      KIT3=KIT3+1                                                       PLEK0992
      KIT4=KIT4+1                                                       PLEK0993
      IF(LF.LE.NKPT) K108=K108+1                                        PLEK0994
      IF(LF.LE.NKPT) K120=K120+1                                        PLEK0995
      IF(LF.LE.NKPT) GOTO 1                                             PLEK0996
      IF(TQ(2,2).NE.512.AND.TQ(2,2).NE.513) GOTO 55556                  PLEK0999
      IF(TQC(2,2).LE.0.AND.TQC(1,2).LT.0) K107=K107+1                   PLEK1000
      IF(TQC(2,2).LE.0.AND.TQC(1,2).LT.0) K119=K119+1                   PLEK1001
      IF(TQC(2,2).LE.0.AND.TQC(1,2).LT.0) GOTO 1                        PLEK1002
      IF(TQC(2,2).GT.0) K106=K106+1                                     PLEK1003
      IF(TQC(2,2).GT.0) K118=K118+1                                     PLEK1004
      IF(TQC(2,2).GT.0) GOTO 1                                          PLEK1005
      KSCP=0                                                            PLEK1006
      DO 652 JJ=3,NT                                                    PLEK1007
      IF(TQC(2,JJ).EQ.0.AND.TQC(1,JJ).LE.0) GOTO 652                    PLEK1008
      IF(TQ(4,JJ).LE.LP) KSCP=1                                         PLEK1009
  652 CONTINUE                                                          PLEK1010
      IF(KSCP.EQ.0) K106=K106+1                                         PLEK1011
      IF(KSCP.EQ.0) K118=K118+1                                         PLEK1012
      IF(KSCP.EQ.1) K107=K107+1                                         PLEK1013
      IF(KSCP.EQ.1) K119=K119+1                                         PLEK1014
      GOTO 1                                                            PLEK1015
  606 KIT7=KIT7+1                                                       PLEK1016
      KIT8=KIT8+1                                                       PLEK1017
      IF(LF.LE.NKPT) K508=K508+1                                        PLEK1018
      IF(LF.LE.NKPT) K520=K520+1                                        PLEK1019
      IF(LF.LE.NKPT) GOTO 1                                             PLEK1020
55556 IF(TQ(2,NT).NE.501) K107=K107+1                                   PLEK1021
      IF(TQ(2,NT).NE.501) K119=K119+1                                   PLEK1022
      IF(TQ(2,NT).NE.501) GOTO 1                                        PLEK1023
      IF(TQC(2,NT).LE.0.AND.TQC(1,NT).LT.0) K107=K107+1                 PLEK1024
      IF(TQC(2,NT).LE.0.AND.TQC(1,NT).LT.0) K119=K119+1                 PLEK1025
      IF(TQC(2,NT).LE.0.AND.TQC(1,NT).LT.0) GOTO 1                      PLEK1026
      IF(TQC(2,NT).GT.0) K106=K106+1                                    PLEK1027
      IF(TQC(2,NT).GT.0) K118=K118+1                                    PLEK1028
      IF(TQC(2,NT).GT.0) GOTO 1                                         PLEK1029
      KSCP=0                                                            PLEK1030
      KTQP=NT-1                                                         PLEK1031
      N11=A(10,NA)                                                      PLEK1032
      IF(N11.EQ.0) IZIK=2                                               PLEK1033
      IF(N11.NE.0) IZIK=A(12,S(1,N11))+A(13,S(1,N11))                   PLEK1034
      DO 653 JJ=IZIK,KTQP                                               PLEK1035
      IF(TQC(2,JJ).EQ.0.AND.TQC(1,JJ).LE.0) GOTO 653                    PLEK1036
      IF(TQ(4,JJ).LE.LP) KSCP=1                                         PLEK1037
  653 CONTINUE                                                          PLEK1038
      IF(KSCP.EQ.0) K106=K106+1                                         PLEK1039
      IF(KSCP.EQ.0) K118=K118+1                                         PLEK1040
      IF(KSCP.EQ.1) K107=K107+1                                         PLEK1041
      IF(KSCP.EQ.1) K119=K119+1                                         PLEK1042
      GOTO 1                                                            PLEK1043
  609 IF(N.EQ.15) GOTO 610                                              PLEK1044
      KIT3=KIT3+1                                                       PLEK1045
      K109=K109+1                                                       PLEK1046
      IF(LF.LE.NKPT) GOTO 611                                           PLEK1047
      IF(TQ(2,2).NE.512.AND.TQ(2,2).NE.513) GOTO 55557                  PLEK1048
      IF(TQC(2,2).LE.0.AND.TQC(1,2).LT.0) GOTO 654                      PLEK1049
      IF(TQC(2,2).GT.0) K106=K106+1                                     PLEK1050
      IF(TQC(2,2).GT.0) GOTO 1                                          PLEK1051
      KSCP=0                                                            PLEK1052
      DO 612 JJ=3,NT                                                    PLEK1053
      IF(TQC(2,JJ).EQ.0.AND.TQC(1,JJ).LE.0) GOTO 612                    PLEK1054
      IF(TQ(4,JJ).LE.LP) KSCP=1                                         PLEK1055
  612 CONTINUE                                                          PLEK1056
      IF(KSCP.EQ.0) K106=K106+1                                         PLEK1057
      IF(KSCP.EQ.0) GOTO 1                                              PLEK1058
      IF(KSCP.EQ.1) GOTO 654                                            PLEK1059
  611 K108=K108+1                                                       PLEK1060
      DO 613 I=1,NT                                                     PLEK1061
      IF(TQC(2,I).LE.0) GOTO 613                                        PLEK1062
      K111=K111+QZ(1,I)                                                 PLEK1063
  613 CONTINUE                                                          PLEK1064
      GOTO 1                                                            PLEK1065
  654 K107=K107+1                                                       PLEK1066
      DO 614 I=1,NT                                                     PLEK1067
      IF(TQC(2,I).LE.0) GOTO 614                                        PLEK1068
      K110=K110+QZ(1,I)                                                 PLEK1069
  614 CONTINUE                                                          PLEK1070
      GOTO 1                                                            PLEK1071
  610 KIT7=KIT7+1                                                       PLEK1072
      K509=K509+1                                                       PLEK1073
      IF(LF.LE.NKPT) GOTO 615                                           PLEK1074
55557 IF(TQ(2,NT).NE.501) GOTO 616                                      PLEK1075
      IF(TQC(2,NT).LE.0.AND.TQC(1,NT).LT.0) GOTO 616                    PLEK1076
      IF(TQC(2,NT).GT.0) K106=K106+1                                    PLEK1077
      IF(TQC(2,NT).GT.0) GOTO 1                                         PLEK1078
      KSCP=0                                                            PLEK1079
      KTQP=NT-1                                                         PLEK1080
      N11=A(10,NA)                                                      PLEK1081
      IF(N11.EQ.0) IZIK=2                                               PLEK1082
      IF(N11.NE.0) IZIK=A(12,S(1,N11))+A(13,S(1,N11))                   PLEK1083
      DO 655 JJ=IZIK,KTQP                                               PLEK1084
      IF(TQC(2,JJ).EQ.0.AND.TQC(1,JJ).LE.0) GOTO 655                    PLEK1085
      IF(TQ(4,JJ).LE.LP) KSCP=1                                         PLEK1086
  655 CONTINUE                                                          PLEK1087
      IF(KSCP.EQ.0) K106=K106+1                                         PLEK1088
      IF(KSCP.EQ.0) GOTO 1                                              PLEK1089
      IF(KSCP.EQ.1) GOTO 616                                            PLEK1090
  615 K508=K508+1                                                       PLEK1091
      DO 617 I=1,NT                                                     PLEK1092
      IF(TQC(2,I).LE.0) GOTO 617                                        PLEK1093
      K111=K111+QZ(1,I)                                                 PLEK1094
  617 CONTINUE                                                          PLEK1095
      GO TO 1                                                           PLEK1096
  616 K107=K107+1                                                       PLEK1097
      DO 618 I=1,NT                                                     PLEK1098
      IF(TQC(2,I).LE.0) GOTO 618                                        PLEK1099
      K110=K110+QZ(1,I)                                                 PLEK1100
  618 CONTINUE                                                          PLEK1101
      GOTO 1                                                            PLEK1102
   27 NI=NI+1                                                           PLEK1103
    4 IF(IJ.LT.0) GOTO 131                                              PLEK1104
  131 IF(NF.EQ.N1) GOTO 156                                             PLEK1110
      IF(N.EQ.17) K122=K122+1                                           PLEK1111
      IF(N.EQ.15) K522=K522+1                                           PLEK1112
      IF(IP.EQ.9.OR.N.EQ.99) GOTO 14                                    PLEK1113
      GOTO 103                                                          PLEK1114
  156 IF(N.EQ.15) GOTO 607                                              PLEK1115
      KIT2=KIT2+1                                                       PLEK1116
      IF(LF.LE.NKPS) K105=K105+1                                        PLEK1117
      IF(LF.LE.NKPS) GOTO 103                                           PLEK1118
      IF(TQ(2,2).NE.512.AND.TQ(2,2).NE.513) GOTO 55558                  PLEK1120
      IF(TQC(2,2).LE.0.AND.TQC(1,2).LT.0) K104=K104+1                   PLEK1121
      IF(TQC(2,2).LE.0.AND.TQC(1,2).LT.0) GOTO 103                      PLEK1122
      IF(TQC(2,2).GT.0) K103=K103+1                                     PLEK1123
      IF(TQC(2,2).GT.0) GOTO 103                                        PLEK1124
      KSCP=0                                                            PLEK1125
      DO 656 JJ=3,NT                                                    PLEK1126
      IF(TQC(2,JJ).EQ.0.AND.TQC(1,JJ).LE.0) GOTO 656                    PLEK1127
      IF(TQ(4,JJ).LE.KOH) KSCP=1                                        PLEK1128
  656 CONTINUE                                                          PLEK1129
      IF(KSCP.EQ.0) K103=K103+1                                         PLEK1130
      IF(KSCP.EQ.1) K104=K104+1                                         PLEK1131
      GOTO 103                                                          PLEK1132
  607 KIT6=KIT6+1                                                       PLEK1133
      IF(LF.LE.NKPS) K505=K505+1                                        PLEK1134
      IF(LF.LE.NKPS) GOTO 103                                           PLEK1135
55558 IF(TQ(2,NT).NE.501) K104=K104+1                                   PLEK1136
      IF(TQ(2,NT).NE.501) GOTO 103                                      PLEK1137
      IF(TQC(2,NT).LE.0.AND.TQC(1,NT).LT.0) K104=K104+1                 PLEK1138
      IF(TQC(2,NT).LE.0.AND.TQC(1,NT).LT.0) GOTO 103                    PLEK1139
      IF(TQC(2,NT).GT.0) K103=K103+1                                    PLEK1140
      IF(TQC(2,NT).GT.0) GOTO 103                                       PLEK1141
      KSCP=0                                                            PLEK1142
      KTQP=NT-1                                                         PLEK1143
      N11=A(10,NA)                                                      PLEK1144
      IF(N11.EQ.0) IZIK=2                                               PLEK1145
      IF(N11.NE.0) IZIK=A(12,S(1,N11))+A(13,S(1,N11))                   PLEK1146
      DO 657 JJ=IZIK,KTQP                                               PLEK1147
      IF(TQC(2,JJ).EQ.0.AND.TQC(1,JJ).LE.0) GOTO 657                    PLEK1148
      IF(TQ(4,JJ).LE.KOH) KSCP=1                                        PLEK1149
  657 CONTINUE                                                          PLEK1150
      IF(KSCP.EQ.0) K103=K103+1                                         PLEK1151
      IF(KSCP.EQ.1) K104=K104+1                                         PLEK1152
  103 IF(NF.EQ.N2) GOTO 102                                             PLEK1153
  101 IF(K-AZ(1,L)) 15,1,15                                             PLEK1154
  102 IF(K-AZ(1,M)) 14,1,14                                             PLEK1155
   13 IF(NXZ(5,1).NE.MET) QZ(2,1)=QZ(2,1)+TQC(2,1)                      PLEK1156
      TQC(2,1)=0                                                        PLEK1157
      DO 120 I=1,NT                                                     PLEK1158
      IF(TQC(2,I).EQ.0) TQC(1,I)=TQC(1,I)-LU                            PLEK1159
  120 CONTINUE                                                          PLEK1160
      WRITE(N3) NXZ,(IW(I),I=1,NU),IQ,IP,ID,LZ,MP,NA,NS,NT              PLEK1161
       WRITE (N3) ((A(I,J),I=1,26),J=1,NA)                              PLEK1162
      WRITE (N3) ((S(I,J),I=1,2),J=1,NS),((P(I,J),I=1,2),J=1,NS)        PLEK1163
      WRITE (N3) ((TQ(I,J),I=1,6),J=1,NT),((QZ(I,J),I=1,4),J=1,NT)      PLEK1164
      WRITE (N3) ((TQC(I,J),I=1,4),J=1,NT)                              PLEK1165
      NQ=NQ+1                                                           PLEK1166
      IF(N.EQ.17) K121=K121+1                                           PLEK1167
      IF(N.EQ.15) K521=K521+1                                           PLEK1168
      GOTO 1                                                            PLEK1169
  900 IF(NF.EQ.N1) GOTO 27                                              PLEK1170
      IF(NXZ(5,1).NE.MET) QZ(2,1)=QZ(2,1)+TQC(2,1)                      PLEK1171
      TQC(2,1)=0                                                        PLEK1172
      WRITE(N3) NXZ,(IW(I),I=1,NU),IQ,IP,ID,LZ,MP,NA,NS,NT              PLEK1173
      WRITE(N3) ((A(I,J),I=1,26),J=1,NA)                                PLEK1174
      WRITE(N3) ((S(I,J),I=1,2),J=1,NS),((P(I,J),I=1,2),J=1,NS)         PLEK1175
      WRITE(N3) ((TQ(I,J),I=1,6),J=1,NT),((QZ(I,J),I=1,4),J=1,NT)       PLEK1176
      WRITE(N3) ((TQC(I,J),I=1,4),J=1,NT)                               PLEK1177
      NQ=NQ+1                                                           PLEK1178
      IF(N.EQ.17) K121=K121+1                                           PLEK1179
      IF(N.EQ.15) K521=K521+1                                           PLEK1180
      GOTO 14                                                           PLEK1181
   25 REWIND NF                                                         PLEK1182
      IF(NF.EQ.N2) GO TO 16                                             PLEK1183
      NI=NP-NI                                                          PLEK1184
      K=0                                                               PLEK1185
      NF=N2                                                             PLEK1186
      MS=0                                                              PLEK1187
      MW=-1                                                             PLEK1188
    1 CONTINUE                                                          PLEK1189
   16 K=NZ-NP                                                           PLEK1190
      NZ1=K-NQ                                                          PLEK1191
      NA=-1000                                                          PLEK1192
      IF(IJ.LT.0) GOTO 132                                              PLEK1193
 132  DO 500 J=1,LP                                                     PLEK1196
      DO 500 I=1,NUC                                                    PLEK1197
  500 ZG1(I,J)=(ZG(I,J)+5)/10                                           PLEK1198
      WRITE(N4) NXZ,(IW(I),I=1,NU),IQ,IP,ID,LD,LZ,LF,MP,MS,NA,NS,NT     PLEK1199
      WRITE(N4) ((ZG1(I,J),I=1,NUC),J=1,LP),((F(I,J),I=1,NUC),J=1,3)    PLEK1200
      REWIND N4                                                         PLEK1201
      WRITE (N3) NXZ,(IW(I),I=1,NU),IQ,IP,ID,LZ,MP,NA,NS,NT             PLEK1202
       REWIND N3                                                        PLEK1203
      WRITE (*,531) NN                                                  PLEK1204
      KZI=KZI-1                                                         PLEK1205
      KOLPLS=KOLPLS-1                                                   PLEK1206
      open(12,file='prn')
      WRITE (*,530) KZI                                                 PLEK1207
      WRITE (*,53) NQ                                                   PLEK1208
      WRITE (*,2000) KOLPLS                                             PLEK1209
      WRITE (*,2001) KOLPLT                                             PLEK1210
      WRITE (*,300)                                                     PLEK1211
      pause'çàïèøè êîëè÷åñòâî çàêàçîâ,âêëþ÷è ïå÷àòü è íàæìè êë."ENTER"'
      DO 302 J=1,10                                                     PLEK1212
  302 WRITE (12,301)                                                    PLEK1213
      K110=K110/100                                                     PLEK1214
      K510=K510/100                                                     PLEK1215
      K111=K111/100                                                     PLEK1216
      K511=K511/100                                                     PLEK1217
      K116=K116/100                                                     PLEK1218
      K516=K516/100                                                     PLEK1219
      DO 305 I=1,1                                                      PLEK1220
      WRITE(12,150) NN                                                  PLEK1221
      WRITE(12,151)                                                     PLEK1222
      WRITE(12,153) K100,K101,K102,K103,K104,K105,K106,K107,K108,K109,K1PLEK1223
     110,K111,K112,K113,K114,K115,K116,K117,K118,K119,K120,K121,K122    PLEK1224
      WRITE(12,151)                                                     PLEK1225
      WRITE(12,625) KIT1,KIT2,KIT3,KIT4                                 PLEK1226
      DO 303 J=1,10                                                     PLEK1232
  303 WRITE (12,301)                                                    PLEK1233
      WRITE (12,300)                                                    PLEK1234
  305 CONTINUE                                                          PLEK1235
      RETURN                                                            PLEK1236
      END                                                               PLEK1237
