      INTEGER*2 FU(10)/'0 ','1 ','2 ','3 ','4 ','5 ','6 ','7 ','8 ','9 'TOBT0004
     */                                                                 TOBT0005
      LOGICAL*1 D/' '/                                                  TOBT0006
      INTEGER*2 MIN(500),MB/'Ñ '/,PROB/'  '/                            TOBT0007
      INTEGER*2 KLAS(2,500),KLD(1),FAMP(5,500)                          TOBT0008
      INTEGER NTF,NTP,NOMTF,TTF                                         TOBT0009
      INTEGER*2 NM,NZ                                                   TOBT0010
      INTEGER KON/'CO  '/                                               TOBT0011
      INTEGER*2 A(26,200),S(2,200),P(2,200),TQ(6,500),TQC(4,500),NXZ(10,TOBT0012
     *4),KDP(180),U(20,100),NIN(5,500),DET(8,500),IZ(500),KOL(500)      TOBT0013
      INTEGER F(100,3)                                                  TOBT0014
      INTEGER*2 ZG(100,60)                                              TOBT0015
      INTEGER QZ(4,500),IW(100),UW(2,200),DA(500)                       TOBT0016
      INTEGER*2 NPR(500),PR,PRI                                         TOBT0017
      INTEGER NAC(500)                                                  TOBT0018
      INTEGER*2 C/'FF'/,KOH1/'CO'/,Z10/'10'/,Z15/'15'/                  TOBT0019
      INTEGER*2 ZAK(4,500)/2000*'FF'/                                   TOBT0020
      INTEGER*2 CP/'ÑÏ'/                                                TOBT0021
      INTEGER*2 MET/'1 '/                                               TOBT0022
      INTEGER*2 GR(1100),KLASIN(2,1100),PROZ(1100),PROZS(1100)          TOBT0023
      character*5 zakzao/'20460'/
      character*5 zakaz
      equivalence(nxz(1,1),zakaz)
      integer*2 dummy2
      integer*4 dummy4,dummy
  900 FORMAT(3A2,I2,3X,I2)                                              TOBT0024
    1 FORMAT(A4,I6,I2,5A2,2A2,I5,4A2,8A2,A2,5A2)                        TOBT0025
  202 FORMAT(1X,A4,I6,I2,5A2,I4,4A2,8A2)                                TOBT0026
      open(14,file='F:\ASUIPW\tek_INF\klasin.dat')
      open(4,file='F:\ASUIPW\tek_INF\u.dat',form='unformatted')
      open(unit=1,file='F:\ASUIPW\tek_INF\plan.dat',form='unformatted')
      open(unit=3,file='F:\ASUIPW\tek_INF\plan1.dat',form='unformatted')
      open(unit=2,file='F:\ASUIPW\tek_INF\portfel.dat',
     1form='unformatted')
      open(unit=8,file='F:\ASUIPW\tek_INF\portfel1.dat',
     1form='unformatted') 
      open(9,file='F:\ASUIPW\tek_INF\ixakt.dat')
      open(10,file='F:\ASUIPW\tek_INF\zakrzak.dat')
      open(12,file='F:\ASUIPW\tek_INF\zakrzak5.dat')
      open(11,file='F:\ASUIPW\tek_INF\cbodtob.dat')
      DO 901 J=1,1100                                                   TOBT0027
      READ(14,900) GR(J),(KLASIN(I,J),I=1,2),PROZ(J),PROZS(J)           TOBT0028
      IF(GR(J).EQ.KOH1) GOTO 902                                        TOBT0029
  901 CONTINUE                                                          TOBT0030
  902 KKLAS=J-1                                                         TOBT0031
      READ(4) NW,NU                                                     TOBT0032
      READ(4) ((UW(I,J),I=1,2),J=1,NW),((U(I,J),I=1,20),J=1,NU)         TOBT0033
      REWIND 4                                                          TOBT0034
      LJ=1                                                              TOBT0035
      LU=1                                                              TOBT0036
      LI=1                                                              TOBT0037
      NUC=0                                                             TOBT0038
      DO 90 I=1,NU                                                      TOBT0039
      IF(U(7,I).LE.0) GOTO 90                                           TOBT0040
      NUC=NUC+1                                                         TOBT0041
   90 CONTINUE                                                          TOBT0042
      PR=1                                                              TOBT0043
      NTP=0                                                             TOBT0044
      NTF=0                                                             TOBT0045
      NTPD=0                                                            TOBT0046
      NOMTF=0                                                           TOBT0047
      TTF=0                                                             TOBT0048
      NSTF=0                                                            TOBT0049
      NSOMTF=0                                                          TOBT0050
      ND=1                                                              TOBT0051
      ND1=3                                                             TOBT0052
      READ(ND) LP,NN,NK                                                 TOBT0053
      LP1=LP*2                                                          TOBT0054
      READ(ND) (KDP(I),I=1,LP1)                                         TOBT0055
      WRITE(ND1) LP,NN,NK                                               TOBT0056
      WRITE(ND1) (KDP(I),I=1,LP1)                                       TOBT0057
      open(666,file = 'tmp.txt')
      read(666,777) itek
  777 format(i8)
      IDATA=NK                                                          TOBT0059
      IDAT2=IDATA                                                       TOBT0060
      IDATA=IDATA/10000                                                 TOBT0061
      IDAT2=IDAT2-IDATA*10000                                           TOBT0062
      IDAT=IDATA/100*100                                                TOBT0063
      IDAT1=IDAT/100                                                    TOBT0064
      IDATA=IDATA-IDAT                                                  TOBT0065
      CALL KALDNI(NK,0,KLD)                                             TOBT0066
      DO 700 K=LP,LP1                                                   TOBT0067
      IF(KDP(K).EQ.KLD(1)) GOTO 401                                     TOBT0068
  700 CONTINUE                                                          TOBT0069
  401 NK=K-LP                                                           TOBT0070
      CALL KALDNI(ITEK,0,KLD)                                           TOBT0071
      DO 701 K=LP,LP1                                                   TOBT0072
      IF(KDP(K).EQ.KLD(1)) GOTO 410                                     TOBT0073
  701 CONTINUE                                                          TOBT0074
  410 ITEK=K-LP                                                         TOBT0075
      DO 200 J=1,500                                                    TOBT0076
      READ(9,1,end=2) NAC(J),DA(J),IZ(J),(NIN(I,J),I=1,5),(KLAS(I,J),I=1TOBT0077
     1,2),KOL(J),(ZAK(I,J),I=1,4),(DET(I,J),I=1,8),MIN(J),(FAMP(I,J),I=1TOBT0078
     2,5)
      IF(NAC(J).EQ.KON) GOTO 2                                          TOBT0079
  200 CONTINUE                                                          TOBT0080
    2 CONTINUE                                                          TOBT0081
      KST=J-1                                                           TOBT0082
      DO 70 I=1,KST                                                     TOBT0083
      NPR(I)=0                                                          TOBT0084
   70 CONTINUE                                                          TOBT0085
   21 IF(ND.NE.1) GOTO 3                                                TOBT0086
      READ(ND) NXZ,(IW(I),I=1,NU),IQ,IP,ID,LD,LZ,LF,MP,MS,NA,NS,NT      TOBT0087
      IF(NA.LT.0) GOTO 4                                                TOBT0088
      GOTO 5                                                            TOBT0089
    3 READ(ND) NXZ,(IW(I),I=1,NU),IQ,IP,ID,LZ,MP,NA,NS,NT               TOBT0090
      IF(NA.LT.0) GOTO 6                                                TOBT0091
    5 READ(ND) ((A(I,J),I=1,26),J=1,NA)                                 TOBT0092
      READ(ND) ((S(I,J),I=1,2),J=1,NS),((P(I,J),I=1,2),J=1,NS)          TOBT0093
      READ(ND) ((TQ(I,J),I=1,6),J=1,NT),((QZ(I,J),I=1,4),J=1,NT)        TOBT0094
      READ(ND) ((TQC(I,J),I=1,4),J=1,NT)                                TOBT0095
      LF1=LF                                                            TOBT0096
      IF(NXZ(5,1).EQ.CP) LF1=NK                                         TOBT0097
      JIK=A(10,1)                                                       TOBT0098
      JIK=S(1,JIK)                                                      TOBT0099
      JIK=A(12,JIK)+A(13,JIK)-1                                         TOBT0100
      NM=TQ(2,JIK)/100                                                  TOBT0101
      JIK=TQ(1,JIK)                                                     TOBT0102
      NZ=U(1,JIK)                                                       TOBT0103
      IF(NZ.EQ.99) NZ=17                                                TOBT0104
      MP1=MP/10000                                                        TOBT0106
      MP2=MP-MP1*10000                                                    TOBT0107
      MP1=MP1-MP1/100*100                                               TOBT0108
      IF(TQC(1,1).LT.0) TQC(2,1)=0                                      TOBT0109
      DO 7 J=1,KST                                                      TOBT0110
      DO 8 I=1,4                                                        TOBT0111
      IF(NXZ(I,1).NE.ZAK(I,J)) GOTO 7                                   TOBT0112
    8 CONTINUE                                                          TOBT0113
      IF(NPR(J).EQ.1) GOTO 30                                           TOBT0114
 8000 NPR(J)=1                                                          TOBT0115
      IF(TQC(1,1).EQ.IDATA) GOTO 20                                     TOBT0116
      TQC(1,1)=IDATA                                                    TOBT0117
      TQC(2,1)=KOL(J)                                                   TOBT0118
      GOTO 222                                                          TOBT0119
   20 IF(MIN(J).EQ.MB) TQC(2,1)=TQC(2,1)-KOL(J)                         TOBT0120
      IF(MIN(J).EQ.PROB) TQC(2,1)=TQC(2,1)+KOL(J)                       TOBT0121
  222 IF(ND.NE.1) GOTO 23                                               TOBT0122
      IF(MS.EQ.-1) GOTO 24                                              TOBT0123
      IF(LF.LE.NK) GOTO 24                                              TOBT0124
      IF(QZ(2,1)+TQC(2,1).LT.IQ) GOTO 25                                TOBT0125
      PRI=3                                                             TOBT0126
      GOTO 26                                                           TOBT0127
   25 PRI=4                                                             TOBT0128
      GOTO 26                                                           TOBT0129
   24 IF(QZ(2,1)+TQC(2,1).LT.IQ) GOTO 27                                TOBT0130
      PRI=1                                                             TOBT0131
      GOTO 26                                                           TOBT0132
   27 PRI=2                                                             TOBT0133
   26 CONTINUE                                                          TOBT0134
      IF(MIN(J).EQ.MB) KOL(J)=-KOL(J)                                   TOBT0135
      IF(IP.GE.2.or.zakaz.eq.zakzao) GOTO 335                                              TOBT0136
      IF(NXZ(5,1).EQ.CP) GOTO 335                                       TOBT0137
      IF(MP2.NE.IDAT2) GOTO 335                                         TOBT0138
      IF(MP1.NE.IDATA) GOTO 335                                         TOBT0139
  336 CONTINUE
C  336 IF(IP.EQ.0) NTF=ID*15                                             TOBT0140
C      IF(IP.EQ.1) NTF=ID*13                                             TOBT0141
C      GOTO 337                                                          TOBT0142
  335 NTF=ID*10                                                         TOBT0143
  337 NTF=NTF*KOL(J)/IQ                                                 TOBT0144
      NTF=NTF/10                                                        TOBT0145
  500 DO 501 K=1,KKLAS                                                  TOBT0146
      IF(KLASIN(1,K).NE.NXZ(2,2).OR.KLASIN(2,K).NE.NXZ(3,2)) GOTO 501   TOBT0147
      KPROZ=PROZ(K)                                                     TOBT0148
      KPROZS=PROZS(K)                                                   TOBT0149
      GOTO 502                                                          TOBT0151
  501 CONTINUE                                                          TOBT0152
      KPROZ=0                                                           TOBT0153
      KPROZS=0                                                          TOBT0154
  502 NSTF=NTF-NTF*KPROZ/1000                                           TOBT0155
      NTF=NTF*100/(100-KPROZS)                                          TOBT0156
      TTF=0                                                             TOBT0157
      IF(NXZ(5,1).EQ.CP) GOTO 8485                                      TOBT0158
 8484 DO 5454 I=1,NU                                                    TOBT0159
      TTF=TTF+IW(I)                                                     TOBT0160
 5454 CONTINUE                                                          TOBT0161
      GOTO 5456                                                         TOBT0162
 8485 DO 5455 I=2,NT                                                    TOBT0163
      TTF=TTF+QZ(1,I)                                                   TOBT0164
 5455 CONTINUE                                                          TOBT0165
 5456 CONTINUE                                                          TOBT0166
      IF(IQ.EQ.0) GOTO 1030                                             TOBT0167
      TTF=TTF*KOL(J)/IQ                                                 TOBT0168
 1030 TTF=TTF/10                                                        TOBT0169
      IQP=IQ-QZ(2,1)                                                    TOBT0170
      DO 300 K=1,10                                                     TOBT0171
      IF(NXZ(4,1).EQ.FU(K)) GOTO 301                                    TOBT0172
  300 CONTINUE                                                          TOBT0173
      WRITE(10,50) NAC(J),DA(J),IZ(J),(NIN(I,J),I=1,5),KOL(J),(ZAK(I,   TOBT0174
     *J),I=1,4),PROB,(DET(I,J),I=1,8),PRI,TTF,NM,(NXZ(I,2),I=2,3),NTF   TOBT0175
   50 FORMAT(A4,I6,I2,5A2,I5,5A2,8A2,I1,I6,I2,2A2,I6)                   TOBT0176
      WRITE(12,55) NAC(J),DA(J),IZ(J),(NIN(I,J),I=1,5),KOL(J),(ZAK(I,J),TOBT0177
     *I=1,4),PROB,(DET(I,J),I=1,8),PRI,NM,(NXZ(I,2),I=1,9),(NXZ(I,4),I=6TOBT0178
     *,9),IQP,(FAMP(I,J),I=1,5),IP                                      TOBT0179
   55 FORMAT(A4,I6,I2,5A2,I5,5A2,8A2,I1,I2,9A2,4A2,I5,5A2,I1)           TOBT0180
      GOTO 22                                                           TOBT0181
  301 WRITE(10,51) NAC(J),DA(J),IZ(J),(NIN(I,J),I=1,5),KOL(J),D,(ZAK(   TOBT0182
     *I,J),I=1,4),D,(DET(I,J),I=1,8),PRI,TTF,NM,(NXZ(I,2),I=2,3),NTF    TOBT0183
   51 FORMAT(A4,I6,I2,5A2,I5,A1,4A2,A1,8A2,I1,I6,I2,2A2,I6)             TOBT0184
      WRITE(12,56) NAC(J),DA(J),IZ(J),(NIN(I,J),I=1,5),KOL(J),D,(ZAK(   TOBT0185
     *I,J),I=1,4),D,(DET(I,J),I=1,8),PRI,NM,(NXZ(I,2),I=1,9),(NXZ(I,4),ITOBT0186
     *=6,9),IQP,(FAMP(I,J),I=1,5),IP                                    TOBT0187
   56 FORMAT(A4,I6,I2,5A2,I5,A1,4A2,A1,8A2,I1,I2,9A2,4A2,I5,5A2,I1)     TOBT0188
   22 IF(TQC(2,1).GT.0) GOTO 7                                          TOBT0189
      NTP=0                                                             TOBT0190
      NTF=0                                                             TOBT0191
      NTPD=0                                                            TOBT0192
      NOMTF=0                                                           TOBT0193
      TTF=0                                                             TOBT0194
      NSTF=0                                                            TOBT0195
      NSOMTF=0                                                          TOBT0196
      GOTO 7                                                            TOBT0197
   23 IF(QZ(2,1 )+TQC(2,1).LT.IQ) GOTO 33                               TOBT0198
      PRI=5                                                             TOBT0199
      GOTO 26                                                           TOBT0200
   33 PRI=6                                                             TOBT0201
      GOTO 26                                                           TOBT0202
   7  CONTINUE                                                          TOBT0203
   36 IF(IP.GE.2.or.ZAKAZ.EQ.ZAKZAO) GOTO 30                                               TOBT0204
      IF(NXZ(5,1).EQ.CP) GOTO 30                                        TOBT0205
      IF(MP2.NE.IDAT2) GOTO 30                                          TOBT0206
      IF(MP1.NE.IDATA) GOTO 30                                          TOBT0207
  333 CONTINUE                                                          TOBT0208
C      IF(IP.EQ.0) ID1=ID*15                                             TOBT0209
C      IF(IP.EQ.1) ID1=ID*13                                             TOBT0210
C      ID1=ID1/10                                                        TOBT0211
C      GOTO 31                                                           TOBT0212
   30 ID1=ID                                                            TOBT0213
   31 CONTINUE                                                          TOBT0214
      IF(ND.NE.1) GOTO 71                                               TOBT0215
      WRITE(ND1) NXZ,(IW(I),I=1,NU),IQ,IP,ID,LD,LZ,LF,MP,MS,NA,NS,NT    TOBT0216
      GOTO 72                                                           TOBT0217
   71 WRITE(ND1) NXZ,(IW(I),I=1,NU),IQ,IP,ID,LZ,MP,NA,NS,NT             TOBT0218
   72 WRITE(ND1) ((A(I,JI),I=1,26),JI=1,NA)                             TOBT0219
      WRITE(ND1) ((S(I,JI),I=1,2),JI=1,NS),((P(I,JI),I=1,2),JI=1,NS)    TOBT0220
      WRITE(ND1) ((TQ(I,JI),I=1,6),JI=1,NT),((QZ(I,JI),I=1,4),JI=1,NT)  TOBT0221
      WRITE(ND1) ((TQC(I,JI),I=1,4),JI=1,NT)                            TOBT0222
      IF(NXZ(1,1).EQ.MET) GOTO 94                                       TOBT0223
   91 IF(ND.NE.1) GOTO 52                                               TOBT0224
      IF(MS.EQ.-1) GOTO 800                                             TOBT0225
      IF(LF.GT.NK) GOTO 52                                              TOBT0226
  800 NTP=ID*10                                                         TOBT0227
      IF(IQ.EQ.0) GOTO 101                                              TOBT0228
      NTP=NTP*QZ(2,1)/IQ                                                TOBT0229
      NTP=NTP/10                                                        TOBT0230
      NTP=ID-NTP                                                        TOBT0231
      IF(NTP.LE.0) NTP=0                                                TOBT0232
      IF(TQC(2,1).EQ.0)   GOTO 53                                       TOBT0233
      GOTO 52                                                           TOBT0234
  101 NTP=ID                                                            TOBT0235
      IF(TQC(2,1).EQ.0) GOTO 53                                         TOBT0236
   52 IF(TQC(2,1).EQ.0) GOTO 94                                         TOBT0237
      NSTF=ID                                                           TOBT0238
      NTF=ID1                                                           TOBT0239
      NTFN=ID1                                                          TOBT0240
      IF(IQ.EQ.0) GOTO 84                                               TOBT0241
      NSTF=NSTF*TQC(2,1)/IQ                                             TOBT0242
      NTF=NTF*TQC(2,1)/IQ                                               TOBT0243
   84 IF(NXZ(5,1).EQ.CP) GOTO 8486                                      TOBT0244
      DO 54 I=1,NU                                                      TOBT0245
      TTF=TTF+IW(I)                                                     TOBT0246
   54 CONTINUE                                                          TOBT0247
      GOTO 5457                                                         TOBT0248
 8486 DO 5458 I=2,NT                                                    TOBT0249
      TTF=TTF+QZ(1,I)                                                   TOBT0250
 5458 CONTINUE                                                          TOBT0251
 5457 CONTINUE                                                          TOBT0252
      TTF=TTF/10                                                        TOBT0253
      IF(IQ.EQ.0) GOTO 103                                              TOBT0254
      TTF=TTF*TQC(2,1)/IQ                                               TOBT0255
  103 IF(NXZ(1,1).EQ.1) GOTO 53                                         TOBT0256
      IF(ND.NE.1) GOTO 53                                               TOBT0257
      IF(MS.EQ.-1) GOTO 801                                             TOBT0258
      IF(LF.GT.NK) GOTO 53                                              TOBT0259
  801 IF(TQC(2,1).LE.IQ-QZ(2,1)) NOMTF=(NTFN*TQC(2,1)*10/IQ+5)/10       TOBT0260
      IF(TQC(2,1).GT.IQ-QZ(2,1)) NOMTF=(NTFN*(IQ-QZ(2,1))*10/IQ+5)/10   TOBT0261
  53  IF(NM.EQ.5.OR.NM.EQ.6) NMOB=5                                     TOBT0267
      IF(NM.EQ.1) NMOB=1                                                TOBT0268
      IF(NM.EQ.2) NMOB=2                                                TOBT0269
      IF(NM.EQ.3) NMOB=3                                                TOBT0270
      IF(NM.EQ.4) NMOB=4                                                TOBT0271
  533 DO 903 J=1,KKLAS                                                  TOBT0273
      IF(KLASIN(1,J).NE.NXZ(2,2).OR.KLASIN(2,J).NE.NXZ(3,2)) GOTO 903   TOBT0274
      KPROZ=PROZ(J)                                                     TOBT0275
      KPROZS=PROZS(J)                                                   TOBT0276
      IF(NZ.EQ.10.AND.NM.EQ.40) KPROZS=0                                TOBT0277
      GOTO 904                                                          TOBT0278
  903 CONTINUE                                                          TOBT0279
      KPROZ=0                                                           TOBT0280
      KPROZS=0                                                          TOBT0281
  904 NSTF=NSTF*100/(100-KPROZS)                                        TOBT0282
      NSOMTF=NOMTF-NOMTF*KPROZS/1000                                    TOBT0283
      NTF=NTF*100/(100-KPROZS)                                          TOBT0284
      NOMTF=NOMTF*100/(100-KPROZS)                                      TOBT0285
      NTP=NTP-NTP*KPROZ/1000                                            TOBT0286
      IF(LF1.LE.ITEK) NTPD=NTP                                          TOBT0287
      IF(NXZ(5,1).EQ.CP) IP=3                                           TOBT0288
      WRITE(11,60) NZ,IP,(NXZ(I,1),I=1,4),NXZ(5,1),NTP,NTF,NOMTF,TTF,   TOBT0289
     *QZ(2,1),TQC(2,1),NMOB,NSTF,NSOMTF,NTPD                            TOBT0290
   60 FORMAT(2I2,4A2,A2,6I6,I1,3I6)                                     TOBT0291
   94 PR=1                                                              TOBT0292
      NTP=0                                                             TOBT0293
      NTF=0                                                             TOBT0294
      NTPD=0                                                            TOBT0295
      NOMTF=0                                                           TOBT0296
      TTF=0                                                             TOBT0297
      NSTF=0                                                            TOBT0298
      NSOMTF=0                                                          TOBT0299
      NM=0                                                              TOBT0300
      GOTO 21                                                           TOBT0301
    4 WRITE(ND1) NXZ,(IW(I),I=1,NU),IQ,IP,ID,LD,LZ,LF,MP,MS,NA,NS,NT    TOBT0302
      READ(ND) ((ZG(I,J),I=1,NUC),J=1,LP),((F(I,J),I=1,NUC),J=1,3)      TOBT0303
      WRITE(ND1) ((ZG(I,J),I=1,NUC),J=1,LP),((F(I,J),I=1,NUC),J=1,3)    TOBT0304
      ND=2                                                              TOBT0305
      ND1=8                                                             TOBT0306
      GOTO 21                                                           TOBT0307
    6 WRITE(ND1) NXZ,(IW(I),I=1,NU),IQ,IP,ID,LZ,MP,NA,NS,NT             TOBT0308
      DO 80 J=1,KST                                                     TOBT0309
      IF(NPR(J).EQ.1) GOTO 80                                           TOBT0310
      PRI=7                                                             TOBT0311
      DO 302 K=1,10                                                     TOBT0312
      IF(ZAK(4,J).EQ.FU(K)) GOTO 303                                    TOBT0313
  302 CONTINUE                                                          TOBT0314
      WRITE(10,50) NAC(J),DA(J),IZ(J),(NIN(I,J),I=1,5),KOL(J),(ZAK(I,   TOBT0315
     *J),I=1,5),(DET(I,J),I=1,8),PRI,NTF,NM,(KLAS(I,J),I=1,2),NTF       TOBT0316
      GOTO 80                                                           TOBT0317
  303 WRITE(10,51) NAC(J),DA(J),IZ(J),(NIN(I,J),I=1,5),KOL(J),D,(ZAK(   TOBT0318
     *I,J),I=1,4),D,(DET(I,J),I=1,8),PRI,NTF,NM,(KLAS(I,J),I=1,2),NTF   TOBT0319
   80 CONTINUE                                                          TOBT0320
   81 REWIND 2                                                          TOBT0321
      stop
      END                                                               TOBT0322
 
