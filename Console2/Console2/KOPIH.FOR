      INTEGER*2 A(26,200),S(2,200),P(2,200),TQ(6,500),TQC(4,500),NXZ(10 OPIH0001
     1,4),ZG1(100,60),KDP(180),U(20,100),IM(10,400)                     OPIH0002
      INTEGER*2 TQ1(6,500),U6(100),U2(100),KLD(1)                       OPIH0003
      INTEGER*2 DEP,MAS,NOMZ(4),VIZM,IPRIT,BMAS,SMAS,GRAFS,GRAF         OPIH0004
      INTEGER*2 IPROB/'  '/                                             OPIH0005
      INTEGER*2 MET/'—œ'/                                               OPIH0006
      INTEGER NPROB/'    '/,ZG(100,60)/6000*0/                          OPIH0007
      INTEGER IDAT/0/,PROBEL                                            OPIH0008
      INTEGER F(100,3)                                                  OPIH0009
      INTEGER  QZ(4,500),IW(100),UW,UPR,IUP,IAN                         OPIH0010
      INTEGER IZ(18,400)/7200*0/,PLS/'+   '/                            OPIH0011
      INTEGER*2 CD/'CO'/,PR/' œ'/,HPR/'  '/                             OPIH0012
      INTEGER*2 NPR/'  '/                                               OPIH0013
      INTEGER*2 SZ1/'1 '/,SZ2/'2 '/                                     OPIH0014
      INTEGER*2 XVOST2,D/'  '/                                          OPIH0015
      LOGICAL*1 ZAK(8),XVOST(2)                                         OPIH0016
      EQUIVALENCE (NOMZ(1),ZAK(1)),(XVOST(1),XVOST2)                    OPIH0017
      integer*2 dummy2
      integer*4 dummy4,dummy
      DATA N0/4/,N1/2/,N2/1/,N3/3/,N4/8/,N5/9/                          OPIH0018
      DATA L/66/                                                        OPIH0019
    1 FORMAT(10A2,3A1,2I6,I1,I6)                                        OPIH0020
   66 FORMAT(1H )                                                       OPIH0021
   45 FORMAT(10I8)                                                      OPIH0022
   71 FORMAT(10X,'B œOPT‘EÀE CTAÀO ',I4,' «AKA«OB'//)                   OPIH0023
 7113 FORMAT(10X,'B œÀAHE CTAÀO    ',I4,' «AKA«OB'//////)               OPIH0024
 7112 FORMAT(10X,'B œÀAHE ¡€ÀO   ',I4,' «AKA«OB')                       OPIH0025
 7110 FORMAT(/////////15X,'œPOTOKOÀ KOPPEK÷»»',1X,I6)                   OPIH0026
 7111 FORMAT(10X,'B œOPT‘EÀE ¡€ÀO ',I4,' «AKA«OB')                      OPIH0027
      open(10,file='F:\ASUIPW\tek_INF\korplpf.dat',access='direct',
     1recl=42,FORM='formatted') 
      open(12,file='F:\ASUIPW\tek_INF\pehkor.dat')
      KZI=0                                                             OPIH0030
      NZAP=1                                                            OPIH0031
      MPZ=0                                                             OPIH0032
      KOLPZI=0                                                          OPIH0033
      KOLPZR=0                                                          OPIH0034
      KZ=0                                                              OPIH0035
      IA=1                                                              OPIH0036
      NUC=0                                                             OPIH0037
      nr=0
      open(666,file = 'tmp.txt')
      read(666,777) nd
  777 format(i6)
      ND=ND+10000                                                       OPIH0041
 2000 MEC=ND/100-ND/10000*100                                           OPIH0042
   2  READ(10,1,end = 10) (IM(I,IA),I=1,10),IAN,IPR,IUP,IZ(14,IA),      OPIH0043
     *iz(15,ia),IZ(16,                                                  OPIH0044
     1IA),IZ(17,IA)                                                     OPIH0045
      IZ(11,IA)=IAN                                                     OPIH0047
      IZ(12,IA)=IPR                                                     OPIH0048
      IZ(13,IA)=IUP                                                     OPIH0049
      IA=IA+1                                                           OPIH0050
      GO TO 2                                                           OPIH0051
   10 IA=IA-1                                                           OPIH0052
      IF(IA.LE.0) GO TO 49                                              OPIH0053
      DO 3 K=1,IA                                                       OPIH0054
      IZ(18,K)=0                                                        OPIH0055
      IF(IZ(11,K).EQ.PLS.AND.IZ(12,K).EQ.PLS) IZ(11,K)=0                OPIH0056
      IF(IZ(13,K).NE.PLS) GO TO 4                                       OPIH0057
      IZ(11,K)=0                                                        OPIH0058
      IZ(12,K)=0                                                        OPIH0059
      IZ(14,K)=0                                                        OPIH0060
      IZ(15,K)=0                                                        OPIH0061
      IF(IZ(13,K).EQ.PLS) IZ(14,K)=ND                                   OPIH0062
    4 IF(IZ(14,K).GT.0.AND.IZ(15,K).GT.0) IZ(14,K)=0                    OPIH0063
      IF(IZ(14,K)+IZ(15,K).LE.0) GO TO 3                                OPIH0064
      IZ(11,K)=0                                                        OPIH0065
      IZ(12,K)=0                                                        OPIH0066
    3 CONTINUE                                                          OPIH0067
      open(unit=n0,file='F:\ASUIPW\tek_INF\u.dat',form='unformatted')
      READ(N0) NW,NU                                                    OPIH0068
      READ(N0)((UW     ,I=1,2),J=1,NW),((U(I,J),I=1,20),J=1,NU)         OPIH0069
      DO 3000 I=1,NU                                                    OPIH0070
      U6(I)=U(6,I)                                                      OPIH0071
 3000 U2(I)=U(2,I)                                                      OPIH0072
      open(unit=n1,file='F:\ASUIPW\tek_INF\plan.dat',form='unformatted')
      open(unit=n3,file='F:\ASUIPW\tek_INF\plan1.dat',
     1form='unformatted')
      open(unit=n2,file='F:\ASUIPW\tek_INF\portfel.dat',
     1form='unformatted')
      open(unit=n4,file='F:\ASUIPW\tek_INF\portfel1.dat',
     1form='unformatted')
      READ(N1) LP,NN,NK                                                 OPIH0073
      NI=NN/100*100                                                     OPIH0074
      NI=NN-NI                                                          OPIH0075
      LP1=2*LP                                                          OPIH0076
      READ(N1) (KDP(I),I=1,LP1)                                         OPIH0077
      CALL KALDNI(ND,0,KLD)                                             OPIH0078
      DO 9010 I1=LP,LP1                                                 OPIH0079
      IF(KLD(1).EQ.KDP(I1)) GOTO 9020                                   OPIH0080
 9010 CONTINUE                                                          OPIH0081
 9020 NDR=I1-LP                                                         OPIH0082
      CALL KALDNI(NK,0,KLD)                                             OPIH0083
      DO 901 I1=LP,LP1                                                  OPIH0084
      IF (KLD(1).EQ.KDP(I1)) GOTO 902                                   OPIH0085
  901 CONTINUE                                                          OPIH0086
  902 NKP=I1-LP                                                         OPIH0087
      DO 16 I=1,NU                                                      OPIH0088
      IF(U(7,I).LE.0) GO TO 16                                          OPIH0089
      NUC=NUC+1                                                         OPIH0090
      U(7,I)=NUC                                                        OPIH0091
   16 CONTINUE                                                          OPIH0092
      DO 17 I=1,NUC                                                     OPIH0093
      DO 17 J=1,LP                                                      OPIH0094
   17 ZG(I,J)=0                                                         OPIH0095
      IA=IA+1                                                           OPIH0096
      IF(NR.NE.0) GOTO 39                                               OPIH0097
   27 READ(N1) NXZ,(IW(I),I=1,NU),IQ,IP,ID,LD,LZ,LF,MP,MS,NA,NS,NT      OPIH0098
      IF(NA.LE.-1000) GO TO 28                                          OPIH0099
      READ(N1) ((A(I,J),I=1,26),J=1,NA)                                 OPIH0100
      READ(N1) ((S(I,J),I=1,2),J=1,NS),((P(I,J),I=1,2),J=1,NS)          OPIH0101
      READ(N1) ((TQ(I,J),I=1,6),J=1,NT),((QZ(I,J),I=1,4),J=1,NT)        OPIH0102
      READ(N1) ((TQC(I,J),I=1,4),J=1,NT)                                OPIH0103
      GOTO 27                                                           OPIH0104
 8500 IF(IP.GE.4) GOTO 1900                                             OPIH0105
      IF(LZ.EQ.0) GOTO 1900                                             OPIH0106
      N=A(10,1)                                                         OPIH0107
      N=S(1,N)                                                          OPIH0108
      N=A(12,N)+A(13,N)-1                                               OPIH0109
      N=TQ(1,N)                                                         OPIH0110
      DEP=U(1,N)                                                        OPIH0111
      IF(DEP.EQ.15) GOTO 1410                                           OPIH0112
      IF(TQ(2,2).NE.1512.AND.TQ(2,2).NE.1513) GOTO 1900                 OPIH0113
      IF(TQC(2,2).LE.0.AND.TQC(1,2).LT.0) GOTO 1900                     OPIH0114
      IF(TQC(2,2).GT.0) GOTO 1900                                       OPIH0115
      KSCP=0                                                            OPIH0116
      DO 8002 J=3,NT                                                    OPIH0117
      IF(TQC(2,J).EQ.0.AND.TQC(1,J).LE.0) GOTO 8002                     OPIH0118
      IF(TQ(4,J).LE.LP) KSCP=1                                          OPIH0119
 8002 CONTINUE                                                          OPIH0120
      IF(KSCP.EQ.1) GOTO 1900                                           OPIH0121
      GOTO 8004                                                         OPIH0122
 1410 IF(TQ(2,NT).NE.101) GOTO 1900                                     OPIH0123
      IF(TQC(2,NT).LE.0.AND.TQC(1,NT).LT.0) GOTO 1900                   OPIH0124
      IF(TQC(2,NT).GT.0) GOTO 1900                                      OPIH0125
      KSCP=0                                                            OPIH0126
      KTQP=NT-1                                                         OPIH0127
      N11=A(10,NA)                                                      OPIH0128
      IF(N11.EQ.0) NZIK=2                                               OPIH0129
      IF(N11.NE.0) NZIK=A(12,S(1,N11))+A(13,S(1,N11))                   OPIH0130
      DO 8005 J=NZIK,KTQP                                               OPIH0131
      IF(TQC(2,J).EQ.0.AND.TQC(1,J).LE.0) GOTO 8005                     OPIH0132
      IF(TQ(4,J).LE.LP) KSCP=1                                          OPIH0133
 8005 CONTINUE                                                          OPIH0134
      IF(KSCP.EQ.1) GOTO 1900                                           OPIH0135
 8004 GOTO 9999                                                         OPIH0136
  28  READ(N1) ((ZG1(I,J),I=1,NUC),J=1,LP),((F(I,J),I=1,NUC),J=1,3)     OPIH0137
      REWIND N1                                                         OPIH0138
      DO 44444 J=1,LP                                                   OPIH0139
      DO 44444 I=1,NUC                                                  OPIH0140
44444 ZG(I,J)=ZG1(I,J)*10                                               OPIH0141
      IM(1,IA)=CD                                                       OPIH0142
      IA=IA-1                                                           OPIH0143
      WRITE(N3) LP,NN,NK                                                OPIH0144
      WRITE(N3) (KDP(I),I=1,LP1)                                        OPIH0145
      II=0                                                              OPIH0148
      READ(N1) LP,NN,NK                                                 OPIH0149
      READ(N1) (KDP(I),I=1,LP1)                                         OPIH0150
   14 READ(N1) NXZ,(IW(I),I=1,NU),IQ,IP,ID,LD,LZ,LF,MP,MS,NA,NS,NT      OPIH0151
      IF(NA.LE.-1000) GO TO 18                                          OPIH0152
      READ(N1) ((A(I,J),I=1,26),J=1,NA)                                 OPIH0153
      READ(N1) ((S(I,J),I=1,2),J=1,NS),((P(I,J),I=1,2),J=1,NS)          OPIH0154
      READ(N1) ((TQ(I,J),I=1,6),J=1,NT),((QZ(I,J),I=1,4),J=1,NT)        OPIH0155
      READ(N1) ((TQC(I,J),I=1,4),J=1,NT)                                OPIH0156
      KOLPZI=KOLPZI+1                                                   OPIH0157
      PPPP=0                                                            OPIH0158
      IPRNZ=0                                                           OPIH0159
      DO 6 K=1,IA                                                       OPIH0160
      IF(IZ(18,K).GE.1) GOTO 6                                          OPIH0161
      DO 7 J=1,4                                                        OPIH0162
      IF(NXZ(J,1).NE.IM(J,K)) GO TO 6                                   OPIH0163
    7 CONTINUE                                                          OPIH0164
      IZ(18,K)=1                                                        OPIH0165
      IF(TQC(1,1).NE.MEC) GOTO 9999                                     OPIH0166
      IF(TQC(2,1).EQ.0) GOTO 9999                                       OPIH0167
      IF(IZ(11,K).EQ.PLS.OR.IZ(12,K).EQ.PLS.OR.IZ(16,K).NE.0.OR.        OPIH0168
     1IZ(17,K).NE.0) IZ(18,K)=0                                         OPIH0169
      IF(IZ(11,K).EQ.PLS.OR.IZ(12,K).EQ.PLS.OR.IZ(16,K).NE.0.OR.        OPIH0170
     1IZ(17,K).NE.0) GOTO 1900                                          OPIH0171
 9999 CONTINUE                                                          OPIH0172
      GRAF=0                                                            OPIH0173
      GRAFS=0                                                           OPIH0174
      IDAT=0                                                            OPIH0175
      IPRIT=0                                                           OPIH0176
      VIZM=0                                                            OPIH0177
      BMAS=IPROB                                                        OPIH0178
      SMAS=IPROB                                                        OPIH0179
      DO 1400 J=1,4                                                     OPIH0180
      NOMZ(J)=NXZ(J,1)                                                  OPIH0181
 1400 CONTINUE                                                          OPIH0182
      XVOST2=D                                                          OPIH0183
      XVOST(2)=ZAK(8)                                                   OPIH0184
      IF(XVOST2.NE.D) GOTO 8888                                         OPIH0185
      DO 8887 J=1,7                                                     OPIH0186
 8887 ZAK(9-J)=ZAK(8-J)                                                 OPIH0187
      XVOST(2)=ZAK(2)                                                   OPIH0188
      NOMZ(1)=D                                                         OPIH0189
      ZAK(2)=XVOST(2)                                                   OPIH0190
 8888 N=A(10,1)                                                         OPIH0191
      N=S(1,N)                                                          OPIH0192
      N=A(12,N)+A(13,N)-1                                               OPIH0193
      MAS=TQ(2,N)/100                                                   OPIH0194
      N=TQ(1,N)                                                         OPIH0195
      DEP=U(1,N)                                                        OPIH0196
      IF(LF.LE.NKP.OR.MS.EQ.-1) GRAFS=3                                 OPIH0197
      IF(LF.LE.NKP.OR.MS.EQ.-1) GOTO 1405                               OPIH0198
      IF(DEP.EQ.15) GO TO 1406                                          OPIH0199
      IF(TQ(2,2).NE.1512.AND.TQ(2,2).NE.1513) GRAFS=2                   OPIH0200
      IF(TQ(2,2).NE.1512.AND.TQ(2,2).NE.1513) GOTO 1405                 OPIH0201
      IF(TQC(2,2).LE.0.AND.TQC(1,2).LT.0) GRAFS=2                       OPIH0202
      IF(TQC(2,2).LE.0.AND.TQC(1,2).LT.0) GOTO 1405                     OPIH0203
      IF(TQC(2,2).GT.0) GRAFS=1                                         OPIH0204
      IF(TQC(2,2).GT.0) GOTO 1405                                       OPIH0205
      KSCP=0                                                            OPIH0206
      DO 8000 J=3,NT                                                    OPIH0207
      IF(TQC(2,J).EQ.0.AND.TQC(1,J).LE.0) GOTO 8000                     OPIH0208
      IF(TQ(4,J).LE.LP) KSCP=1                                          OPIH0209
 8000 CONTINUE                                                          OPIH0210
      IF(KSCP.EQ.0) GRAFS=1                                             OPIH0211
      IF(KSCP.EQ.1) GRAFS=2                                             OPIH0212
      GO TO 1405                                                        OPIH0213
 1406 IF(TQ(2,NT).NE.101) GRAFS=2                                       OPIH0214
      IF(TQ(2,NT).NE.101) GOTO 1405                                     OPIH0215
      IF(TQC(2,NT).LE.0.AND.TQC(1,NT).LT.0) GRAFS=2                     OPIH0216
      IF(TQC(2,NT).LE.0.AND.TQC(1,NT).LT.0) GOTO 1405                   OPIH0217
      IF(TQC(2,NT).GT.0) GRAFS=1                                        OPIH0218
      IF(TQC(2,NT).GT.0) GOTO 1405                                      OPIH0219
      KSCP=0                                                            OPIH0220
      KTQP=NT-1                                                         OPIH0221
      N11=A(10,NA)                                                      OPIH0222
      IF(N11.EQ.0) NZIK=2                                               OPIH0223
      IF(N11.NE.0) NZIK=A(12,S(1,N11))+A(13,S(1,N11))                   OPIH0224
      DO 8001 J=NZIK,KTQP                                               OPIH0225
      IF(TQC(2,J).EQ.0.AND.TQC(1,J).LE.0) GOTO 8001                     OPIH0226
      IF(TQ(4,J).LE.LP) KSCP=1                                          OPIH0227
 8001 CONTINUE                                                          OPIH0228
      IF(KSCP.EQ.0) GRAFS=1                                             OPIH0229
      IF(KSCP.EQ.1) GRAFS=2                                             OPIH0230
 1405 IF(IPRNZ.EQ.1.AND.IP.EQ.0) GOTO 11                                OPIH0231
      IF(IPRNZ.EQ.1) GOTO 15                                            OPIH0232
      IP1=IP                                                            OPIH0233
      IPP=0                                                             OPIH0234
      IF(IZ(16,K).GT.0) IP=IZ(16,K)                                     OPIH0235
      IF(IZ(16,K).EQ.8) IP=0                                            OPIH0236
      IF(IZ(16,K).GT.0) IPRIT=IZ(16,K)                                  OPIH0237
      IF(IZ(16,K).GT.0) VIZM=0                                          OPIH0238
      IF(IZ(16,K).EQ.0) GOTO 23                                         OPIH0239
      IF(TQ(5,A(12,S(1,A(10,1)))+A(13,S(1,A(10,1)))-1).EQ.1) GOTO 23    OPIH0240
      IF(QZ(4,A(12,S(1,A(10,1)))+A(13,S(1,A(10,1)))-1).LT.1000000.AND.IPOPIH0241
     1.GT.3) GOTO 23                                                    OPIH0242
      IF(IP1.GT.3.AND.IP.GT.3) GOTO 23                                  OPIH0243
      IF(QZ(4,A(12,S(1,A(10,1)))+A(13,S(1,A(10,1)))-1).LT.1000000.AND.IPOPIH0244
     11.EQ.0.AND.IP.GE.3) GOTO 23                                       OPIH0245
      IF(TQC(2,A(12,S(1,A(10,1)))+A(13,S(1,A(10,1)))-1).EQ.0) GOTO 23   OPIH0246
      IF(A(10,1).GE.NA) GOTO 23                                         OPIH0247
      IF(NXZ(5,1).EQ.MET.AND.IZ(12,K).EQ.PLS) GOTO 23                   OPIH0248
      IF(IP.EQ.2.AND.IP1.EQ.3) GOTO 23                                  OPIH0249
      IF(IP.EQ.3.AND.IP1.EQ.2) GOTO 23                                  OPIH0250
      LZ1=LZ                                                            OPIH0251
      TQ1(1,1)=TQ(1,1)                                                  OPIH0252
      TQ1(2,1)=TQ(2,1)                                                  OPIH0253
      TQ1(3,1)=TQ(3,1)                                                  OPIH0254
      TQ1(4,1)=0                                                        OPIH0255
      TQ1(5,1)=0                                                        OPIH0256
      TQ1(6,1)=0                                                        OPIH0257
      DO 7009 J=2,NT                                                    OPIH0258
      TQ1(1,J)=TQ(1,J)                                                  OPIH0259
      TQ1(2,J)=TQ(2,J)                                                  OPIH0260
      TQ1(3,J)=TQ(3,J)                                                  OPIH0261
      TQ1(4,J)=LF-TQ(4,J)                                               OPIH0262
      TQ1(5,J)=LF-TQ(5,J)                                               OPIH0263
      TQ1(6,J)=LF-TQ(6,J)                                               OPIH0264
      IF(TQC(2,J).EQ.0) TQ1(4,J)=LF-1                                   OPIH0265
      IF(TQC(2,J).EQ.0) TQ1(5,J)=LF-1                                   OPIH0266
      IF(TQC(2,J).EQ.0) TQ1(6,J)=LF-1                                   OPIH0267
 7009 CONTINUE                                                          OPIH0268
      ASSIGN 502 TO MM                                                  OPIH0269
  505 DO 7000 J=2,NT                                                    OPIH0270
      IF(QZ(4,J).LT.1000000) GOTO 7001                                  OPIH0271
      TQ1(4,J)=QZ(4,J)/1000000                                          OPIH0272
      TQ1(5,J)=QZ(4,J)/1000-TQ1(4,J)*1000                               OPIH0273
      TQ1(6,J)=QZ(4,J)-TQ1(4,J)*1000000-TQ1(5,J)*1000                   OPIH0274
      GOTO 7000                                                         OPIH0275
 7001 IF(TQC(2,J).EQ.0) GOTO 7000                                       OPIH0276
      QZ(4,J)=TQ1(4,J)*1000000+TQ1(5,J)*1000+TQ1(6,J)                   OPIH0277
 7000 CONTINUE                                                          OPIH0278
      ASSIGN 5000 TO MMM                                                OPIH0279
      GOTO 5001                                                         OPIH0280
 5000 IF(II.EQ.0) CALL PODZAG(U,TQ,QZ,ZG,NT,LP,1,TQC,IP1)               OPIH0281
      IF(IP.LE.3) GOTO 7003                                             OPIH0282
      DO 2222 J=2,NT                                                    OPIH0283
      R=(QZ(1,J)+0.0)/U(2,TQ(1,J))                                      OPIH0284
      J5=INT(R)                                                         OPIH0285
      IF(R-J5.LE.0.0) J5=J5-1                                           OPIH0286
      IF(TQ1(4,J).LT.TQ1(5,J)+J5) GOTO 23                               OPIH0287
 2222 CONTINUE                                                          OPIH0288
      GOTO 7002                                                         OPIH0289
 7003 DO 501 J=1,NU                                                     OPIH0290
      IF(IP.EQ.0.OR.IP.EQ.1) U(6,J)=0                                   OPIH0291
      IF(IP.EQ.0.AND.U(2,J).EQ.820) U(2,J)=U(2,J)*2                     OPIH0292
  501 CONTINUE                                                          OPIH0293
      IF(IP.EQ.3) IPP=3                                                 OPIH0294
      IF(IP.EQ.3) IP=2                                                  OPIH0295
      CALL PRIOR(A,TQ1,QZ,NT,NA,S,U,LZ,IP,NU,TQC)                       OPIH0296
      IF(IPP.EQ.3) IP=3                                                 OPIH0297
      IF(IP.GT.0) CALL SGRLZE(LZ,NA,NT,NU,A,S,P,TQ1,QZ,TQC,U,IP,IW)     OPIH0298
      CALL MEHDET(NA,NT,A,S,P,TQ1,TQC)                                  OPIH0299
 7002 CALL MEHDET(NA,NT,A,S,P,TQ1,TQC)                                  OPIH0300
      IF(IP.GT.0) CALL SGRLZE(LZ,NA,NT,NU,A,S,P,TQ1,QZ,TQC,U,IP,IW)     OPIH0301
      DO 500 J=1,NU                                                     OPIH0302
      U(2,J)=U2(J)                                                      OPIH0303
  500 U(6,J)=U6(J)                                                      OPIH0304
      DO 7004 J=2,NT                                                    OPIH0305
      IF(TQC(2,J).EQ.0) GOTO 7004                                       OPIH0306
      TQ(4,J)=TQ1(4,J)                                                  OPIH0307
      TQ(5,J)=TQ1(5,J)                                                  OPIH0308
      TQ(6,J)=TQ1(6,J)                                                  OPIH0309
 7004 CONTINUE                                                          OPIH0310
      GOTO MM,(502,13)                                                  OPIH0311
  502 DO 503 J=2,NT                                                     OPIH0312
      IF(TQC(2,J).EQ.0) GOTO 503                                        OPIH0313
      TQ(4,J)=LF-TQ(4,J)                                                OPIH0314
      TQ(5,J)=LF-TQ(5,J)                                                OPIH0315
  503 TQ(6,J)=LF-TQ(6,J)                                                OPIH0316
      CALL PODZAG(U,TQ,QZ,ZG,NT,LP,0,TQC,IP)                            OPIH0317
      IF(IZ(14,K)+IZ(15,K).EQ.0) IZ(14,K)=LF-LZ1                        OPIH0318
      IF(IZ(14,K)+IZ(15,K).EQ.0) IZ(14,K)=2                             OPIH0319
   23 CONTINUE                                                          OPIH0320
      IF(IZ(17,K).GT.0) MP=IZ(17,K)                                     OPIH0321
      IF(IZ(17,K).GT.0) IDAT=IZ(17,K)                                   OPIH0322
      IF(IZ(17,K).GT.0) VIZM=0                                          OPIH0323
      IF(IZ(13,K).NE.PLS) GOTO 8                                        OPIH0324
      MS=-1                                                             OPIH0325
      GOTO 44                                                           OPIH0326
    8 IF(IZ(12,K).NE.PLS) GO TO 43                                      OPIH0327
      IF(GRAFS.EQ.1) VIZM=7                                             OPIH0328
      IF(GRAFS.EQ.2) VIZM=6                                             OPIH0329
      IF(GRAFS.EQ.3) VIZM=5                                             OPIH0330
      MS=-2                                                             OPIH0331
      IF(NXZ(5,1).EQ.MET) GOTO 4300                                     OPIH0332
      CALL PODZAG(U,TQ,QZ,ZG,NT,LP,1,TQC,IP)                            OPIH0333
 4300 DO 100 J=1,NT                                                     OPIH0334
      IF(TQC(2,J).EQ.0) GOTO 100                                        OPIH0335
      TQ(4,J)=LF-TQ(4,J)                                                OPIH0336
      TQ(5,J)=LF-TQ(5,J)                                                OPIH0337
      TQ(6,J)=LF-TQ(6,J)                                                OPIH0338
  100 CONTINUE                                                          OPIH0339
      WRITE(N4) NXZ,(IW(I),I=1,NU),IQ,IP,ID,LZ,MP,NA,NS,NT              OPIH0340
      WRITE(N4) ((A(I,J),I=1,26),J=1,NA)                                OPIH0341
      WRITE(N4) ((S(I,J),I=1,2),J=1,NS),((P(I,J),I=1,2),J=1,NS)         OPIH0342
      WRITE(N4) ((TQ(I,J),I=1,6),J=1,NT),((QZ(I,J),I=1,4),J=1,NT)       OPIH0343
      WRITE(N4) ((TQC(I,J),I=1,4),J=1,NT)                               OPIH0344
      WRITE(12,1407) DEP,MAS,(NOMZ(J),J=1,4),VIZM,IPRIT,                OPIH0345
     *IDAT,BMAS,SMAS                                                    OPIH0346
 1407 FORMAT(I2,I2,4A2,I2,I1,I6,2A2)                                    OPIH0347
      KZ=KZ+1                                                           OPIH0348
      MPZ=1                                                             OPIH0349
      IF(NXZ(5,1).EQ.MET) NXZ(5,1)=CD                                   OPIH0350
      IF(NXZ(5,1).EQ.CD) GOTO 1900                                      OPIH0351
      GOTO 140                                                          OPIH0352
   43 IF(IZ(11,K).NE.PLS) GO TO 44                                      OPIH0353
      IF(GRAFS.EQ.1) VIZM=3                                             OPIH0354
      IF(GRAFS.EQ.2) VIZM=2                                             OPIH0355
      IF(GRAFS.EQ.3) VIZM=1                                             OPIH0356
      WRITE(12,1407) DEP,MAS,(NOMZ(J),J=1,4),VIZM,IPRIT,                OPIH0357
     *IDAT,BMAS,SMAS                                                    OPIH0358
      MS=-3                                                             OPIH0359
      CALL PODZAG(U,TQ,QZ,ZG,NT,LP,1,TQC,IP)                            OPIH0360
      GOTO 140                                                          OPIH0361
   44 IF(IZ(14,K).GT.0) GO TO 11                                        OPIH0362
      IF(IZ(15,K).GT.0) GO TO 15                                        OPIH0363
      WRITE(12,1407) DEP,MAS,(NOMZ(J),J=1,4),VIZM,IPRIT,                OPIH0364
     *IDAT,BMAS,SMAS                                                    OPIH0365
    6 CONTINUE                                                          OPIH0366
      K=IA+1                                                            OPIH0367
      IPRNZ=1                                                           OPIH0368
      GO TO 8500                                                        OPIH0369
   11 CONTINUE                                                          OPIH0370
      IF(IPRNZ.EQ.1) IZ(14,K)=ND                                        OPIH0371
      IF(IZ(14,K).LT.10000) I1=LP+IZ(14,K)                              OPIH0372
      IF(IZ(14,K).LT.10000) GOTO 31                                     OPIH0373
      MS=0                                                              OPIH0374
      IWF=IZ(14,K)                                                      OPIH0375
      CALL KALDNI(IWF,0,KLD)                                            OPIH0376
      IWF=KLD(1)/10                                                     OPIH0377
      DO 30 I1=LP,LP1                                                   OPIH0378
      IF(IWF.EQ.KDP(I1)/10) GOTO 31                                     OPIH0379
   30 I2=I1                                                             OPIH0380
      I1=I2                                                             OPIH0381
   31 LI=I1-LP                                                          OPIH0382
      LZ=0                                                              OPIH0383
      ASSIGN 5003 TO MMM                                                OPIH0384
 5001 DO 720 KOL=1,2                                                    OPIH0385
      DO 1590  J3=2,NA                                                  OPIH0386
      J2=A(12,J3)+A(13,J3)-1                                            OPIH0387
      J1=A(12,J3)                                                       OPIH0388
      NPOS=J2-1                                                         OPIH0389
      DO 1600 J6=J1,J2                                                  OPIH0390
      KJ=J2-J6                                                          OPIH0391
      J4=J1+KJ                                                          OPIH0392
      IF(J4.EQ.J2.AND.TQC(2,J4).NE.0) GOTO 1591                         OPIH0393
      IF(TQC(2,J4).NE.0) GOTO 1600                                      OPIH0394
      IF(KJ.EQ.0) GOTO 1600                                             OPIH0395
      GOTO 700                                                          OPIH0396
 1591 NOMP=A(14,J3)                                                     OPIH0397
      KOLP=A(15,J3)                                                     OPIH0398
      KON=NOMP+KOLP-1                                                   OPIH0399
      IF(NA.EQ.3.AND.A(9,2).EQ.A(9,3)) GOTO 1600                        OPIH0400
      DO 710 J10=NOMP,KON                                               OPIH0401
      NSB=P(1,J10)                                                      OPIH0402
      IF(NSB.EQ.1) GOTO 710                                             OPIH0403
      NSB=A(12,NSB)                                                     OPIH0404
      IF(TQC(2,NSB).NE.0) GOTO 710                                      OPIH0405
      KJ=KJ+1                                                           OPIH0406
      GOTO 700                                                          OPIH0407
  710 CONTINUE                                                          OPIH0408
      GOTO 1600                                                         OPIH0409
  700 KJ=KJ+1                                                           OPIH0410
      DO 1610 JK=1,KJ                                                   OPIH0411
      J5=J1+JK-1                                                        OPIH0412
      IF(TQC(2,J5).EQ.0) GOTO 1610                                      OPIH0413
      TQC(1,J5)=NDR                                                     OPIH0414
      TQC(3,J5)=0                                                       OPIH0415
 1610 TQC(2,J5)=0                                                       OPIH0416
      GOTO 1590                                                         OPIH0417
 1600 CONTINUE                                                          OPIH0418
 1590 CONTINUE                                                          OPIH0419
  720 CONTINUE                                                          OPIH0420
      GOTO MMM,(5000,5003)                                              OPIH0421
 5003 CALL PODZAG(U,TQ,QZ,ZG,NT,LP,1,TQC,IP)                            OPIH0422
      J3=A(10,1)                                                        OPIH0423
      J3=A(11,1)+J3-1                                                   OPIH0424
      J3=S(1,J3)                                                        OPIH0425
      J8=A(12,J3)+A(13,J3)-1                                            OPIH0426
      J1=A(12,J3)                                                       OPIH0427
      DO 1500 J6=J1,J8                                                  OPIH0428
      KJ=J8-J6                                                          OPIH0429
      J4=J1+KJ                                                          OPIH0430
      IF(TQ(2,J4)-TQ(2,J4)/100*100.NE.83) GOTO 1500                     OPIH0431
      IF(QZ(1,J4).EQ.1.AND.QZ(2,J4).EQ.0) GOTO 1500                     OPIH0432
      R=(QZ(1,J4)+0.0)/U(2,TQ(1,J4))                                    OPIH0433
      J5=INT(R)                                                         OPIH0434
      J66=TQ(5,J4)-TQ(4,J4)                                             OPIH0435
      IF(R-J5.LE.0.0) J5=J5-1                                           OPIH0436
      J7=J66-J5                                                         OPIH0437
      IF(J7.LE.0) J7=0                                                  OPIH0438
      GOTO 1510                                                         OPIH0439
 1500 CONTINUE                                                          OPIH0440
      J7=0                                                              OPIH0441
 1510 DO 1530 J=2,NT                                                    OPIH0442
      IF(TQC(2,J).LE.0) GOTO 1530                                       OPIH0443
      IF(J.GT.J4.AND.J.NE.J8) GOTO 1520                                 OPIH0444
      IF(LF-TQ(4,J)-J7.GT.LZ)  LZ=LF-TQ(4,J)-J7                         OPIH0445
      GOTO 1530                                                         OPIH0446
 1520 IF(LF-TQ(4,J).GT.LZ) LZ=LF-TQ(4,J)                                OPIH0447
 1530 CONTINUE                                                          OPIH0448
      LJ=LF-LZ                                                          OPIH0449
      LJ=LI-LJ                                                          OPIH0450
      LF=LF+LJ                                                          OPIH0451
      DO 33 I1=1,NT                                                     OPIH0452
      IF(TQC(2,I1).LE.0) GO TO 33                                       OPIH0453
      DO 32 J=4,6                                                       OPIH0454
      TQ(J,I1)=TQ(J,I1)+LJ                                              OPIH0455
      IF(TQ(J,I1).LE.0) TQ(J,I1)=1                                      OPIH0456
   32 CONTINUE                                                          OPIH0457
   33 CONTINUE                                                          OPIH0458
      CALL PODZAG(U,TQ,QZ,ZG,NT,LP,0,TQC,IP)                            OPIH0459
      IF(IZ(14,K).LT.10000.AND.LF.LE.NKP) MS=0                          OPIH0460
      CONTINUE                                                          OPIH0461
      IF(LF.LE.NKP.AND.IZ(13,K).EQ.PLS)  MS=0                           OPIH0462
      GO TO 900                                                         OPIH0463
   15 MS=0                                                              OPIH0464
   35 KOLICH=IQ                                                         OPIH0465
      IQ=IZ(15,K)                                                       OPIH0466
      IF(IPRNZ.EQ.1) IQ=ND                                              OPIH0467
      CALL KALDNI(IQ,0,KLD)                                             OPIH0468
      IQ=KLD(1)/10                                                      OPIH0469
      DO 58 I=LP,LP1                                                    OPIH0470
      IF(IQ.EQ.KDP(I)/10) GO TO 59                                      OPIH0471
   58 I1=I                                                              OPIH0472
      I=I1                                                              OPIH0473
   59 IQ=I-LP                                                           OPIH0474
      LZ=0                                                              OPIH0475
      DO 721 KOL=1,2                                                    OPIH0476
      DO 1690 J3=2,NA                                                   OPIH0477
      J2=A(12,J3)+A(13,J3)-1                                            OPIH0478
      J1=A(12,J3)                                                       OPIH0479
      NPOS=J2-1                                                         OPIH0480
      DO 1700 J6=J1,J2                                                  OPIH0481
      KJ=J2-J6                                                          OPIH0482
      J4=J1+KJ                                                          OPIH0483
      IF(J4.EQ.J2.AND.TQC(2,J4).NE.0) GOTO 1691                         OPIH0484
      IF(TQC(2,J4).NE.0) GOTO 1700                                      OPIH0485
      IF(KJ.EQ.0) GOTO 1700                                             OPIH0486
      GOTO 701                                                          OPIH0487
 1691 NOMP=A(14,J3)                                                     OPIH0488
      KOLP=A(15,J3)                                                     OPIH0489
      KON=NOMP+KOLP-1                                                   OPIH0490
      IF(NA.EQ.3.AND.A(9,2).EQ.A(9,3)) GOTO 1700                        OPIH0491
      DO 711 J10=NOMP,KON                                               OPIH0492
      NSB=P(1,J10)                                                      OPIH0493
      IF(NSB.EQ.1) GOTO 711                                             OPIH0494
      NSB=A(12,NSB)                                                     OPIH0495
      IF(TQC(2,NSB).NE.0) GOTO 711                                      OPIH0496
      KJ=KJ+1                                                           OPIH0497
      GOTO 701                                                          OPIH0498
  711 CONTINUE                                                          OPIH0499
      GOTO 1700                                                         OPIH0500
  701 KJ=KJ+1                                                           OPIH0501
      DO 1710 JK=1,KJ                                                   OPIH0502
      J5=J1+JK-1                                                        OPIH0503
      IF(TQC(2,J5).EQ.0) GOTO 1710                                      OPIH0504
      TQC(1,J5)=NDR                                                     OPIH0505
      TQC(3,J5)=0                                                       OPIH0506
1710  TQC(2,J5)=0                                                       OPIH0507
      GOTO 1690                                                         OPIH0508
1700  CONTINUE                                                          OPIH0509
1690  CONTINUE                                                          OPIH0510
  721 CONTINUE                                                          OPIH0511
      IF(II.EQ.0) CALL PODZAG(U,TQ,QZ,ZG,NT,LP,1,TQC,IP)                OPIH0512
      IF(II.EQ.1) GOTO 730                                              OPIH0513
      DO 60 I=1,NT                                                      OPIH0514
      IF(TQC(2,I).LE.0) GOTO 60                                         OPIH0515
      DO 61 J=4,6                                                       OPIH0516
   61 TQ(J,I)=LF-TQ(J,I)                                                OPIH0517
   60 CONTINUE                                                          OPIH0518
  730 J3=A(10,1)                                                        OPIH0519
      J3=A(11,1)+J3-1                                                   OPIH0520
      J3=S(1,J3)                                                        OPIH0521
      J8=A(12,J3)+A(13,J3)-1                                            OPIH0522
      J1=A(12,J3)                                                       OPIH0523
      DO 1615 J6=J1,J8                                                  OPIH0524
      KJ=J8-J6                                                          OPIH0525
      J4=J1+KJ                                                          OPIH0526
      IF(TQ(2,J4)-TQ(2,J4)/100*100.NE.83) GOTO 1615                     OPIH0527
      IF(QZ(1,J4).EQ.1.AND.QZ(2,J4).EQ.0) GOTO 1615                     OPIH0528
      R=(QZ(1,J4)+0.0)/U(2,TQ(1,J4))                                    OPIH0529
      J5=INT(R)                                                         OPIH0530
      IF(R-J5.LE.0.0) J5=J5-1                                           OPIH0531
      J66=TQ(4,J4)-TQ(5,J4)                                             OPIH0532
      J7=J66-J5                                                         OPIH0533
      IF(J7.LE.0) J7=0                                                  OPIH0534
      GOTO 1614                                                         OPIH0535
1615  CONTINUE                                                          OPIH0536
      J7=0                                                              OPIH0537
1614  DO 1720 J=2,NT                                                    OPIH0538
      IF(TQC(2,J).LE.0) GOTO 1720                                       OPIH0539
      IF(J.GT.J4.AND.J.LE.J8) GOTO 1730                                 OPIH0540
      TQ(4,J)=TQ(4,J)-J7                                                OPIH0541
      IF(J.EQ.J4) GOTO 1730                                             OPIH0542
      TQ(5,J)=TQ(5,J)-J7                                                OPIH0543
      TQ(6,J)=TQ(6,J)-J7                                                OPIH0544
 1730 IF(TQ(4,J).GT.LZ) LZ=TQ(4,J)                                      OPIH0545
1720  CONTINUE                                                          OPIH0546
      IF(IPRNZ.EQ.0) GOTO 54                                            OPIH0547
      IF(NKP-NDR.LT.LZ) GOTO 54                                         OPIH0548
      IQ=NKP-LZ+1                                                       OPIH0549
   54 DO 50 J1=1,NT                                                     OPIH0550
      IF(TQC(2,J1).LE.0) GO TO 50                                       OPIH0551
      I2=LZ-TQ(5,J1)+IQ-1                                               OPIH0552
      IF(I2.GT.LP) GO TO 50                                             OPIH0553
      I3=TQ(1,J1)                                                       OPIH0554
      I4=U(7,I3)                                                        OPIH0555
      IF(I4.LE.0) GO TO 50                                              OPIH0556
      IJ=1                                                              OPIH0557
      I1=LZ-TQ(4,J1)+IQ-1                                               OPIH0558
      IF(I1.GE.I2) GO TO 51                                             OPIH0559
      I=I2-1                                                            OPIH0560
      DO 52 J=I1,I                                                      OPIH0561
      IF((ZG(I4,J)+U(2,I3)).GT.(U(4,I3)*U(3,I3)+50)/100) GO TO 53       OPIH0562
   52 CONTINUE                                                          OPIH0563
   51 I0=QZ(1,J1)-U(2,I3)*(I2-I1)                                       OPIH0564
      IF((ZG(I4,I2)+I0).GT.(U(4,I3)*U(3,I3)+50)/100) GO TO 53           OPIH0565
   50 CONTINUE                                                          OPIH0566
      GO TO 56                                                          OPIH0567
   53 IQ=IQ+1                                                           OPIH0568
      IF(IQ-LP) 54,54,90                                                OPIH0569
   90 IQ=KOLICH                                                         OPIH0570
      IF(II.EQ.1.AND.IPRNZ.EQ.1) GOTO 25                                OPIH0571
      IF(II.EQ.1) VIZM=22                                               OPIH0572
      IF(II.EQ.1) GOTO 91                                               OPIH0573
      DO 92 I1=1,NT                                                     OPIH0574
      IF(TQC(2,I1).LE.0) GOTO 92                                        OPIH0575
      DO 93 J=4,6                                                       OPIH0576
   93 TQ(J,I1)=LF-TQ(J,I1)                                              OPIH0577
   92 CONTINUE                                                          OPIH0578
      CALL PODZAG(U,TQ,QZ,ZG,NT,LP,0,TQC,IP)                            OPIH0579
      IF(IPRNZ.EQ.1) PPPP=1                                             OPIH0580
      GOTO 9                                                            OPIH0581
   56 DO 34 I1=1,NT                                                     OPIH0582
      IF(TQC(2,I1).LE.0) GO TO 34                                       OPIH0583
      DO 57 J=4,6                                                       OPIH0584
   57 TQ(J,I1)=LZ-TQ(J,I1)+IQ                                           OPIH0585
   34 CONTINUE                                                          OPIH0586
      CALL PODZAG(U,TQ,QZ,ZG,NT,LP,0,TQC,IP)                            OPIH0587
      CONTINUE                                                          OPIH0588
      LF=IQ+LZ                                                          OPIH0589
      IQ=IQ+LP                                                          OPIH0590
      IF(II.LE.0) GOTO 9000                                             OPIH0591
      LD=KDP(IQ)                                                        OPIH0592
      IF(LD.LT.0) LD=KDP(LP1)                                           OPIH0593
      LD=LD/10*100+(ND-ND/100*100)                                      OPIH0594
 9000 IQ=KOLICH                                                         OPIH0595
  900 LZ=0                                                              OPIH0596
      IPPZ=0                                                            OPIH0597
      DO 7010 J=2,NT                                                    OPIH0598
      IF(TQC(2,J).EQ.0) GOTO 7010                                       OPIH0599
      IF(TQ(2,J).EQ.1512.OR.TQ(2,J).EQ.1513) IPPZ=1                     OPIH0600
      IF(TQ(2,J).EQ.1512.OR.TQ(2,J).EQ.1513) GOTO 7010                  OPIH0601
      IF(LF-TQ(4,J).GT.LZ) LZ=LF-TQ(4,J)                                OPIH0602
 7010 CONTINUE                                                          OPIH0603
      IF(IPPZ.EQ.1) LZ=LZ+30                                            OPIH0604
    9 IF(LF.LE.NKP.OR.MS.EQ.-1) GRAF=3                                  OPIH0610
      IF(LF.LE.NKP.OR.MS.EQ.-1) GOTO 1504                               OPIH0611
      IF(DEP.EQ.15) GO TO 1505                                          OPIH0612
      IF(TQ(2,2).NE.1512.AND.TQ(2,2).NE.1513) GRAF=2                    OPIH0613
      IF(TQ(2,2).NE.1512.AND.TQ(2,2).NE.1513) GOTO 1504                 OPIH0614
      IF(TQC(2,2).LE.0) GRAF=2                                          OPIH0615
      IF(TQC(2,2).GT.0) GRAF=1                                          OPIH0616
      GO TO 1504                                                        OPIH0617
 1505 IF(TQ(2,NT).NE.101) GRAF=2                                        OPIH0618
      IF(TQ(2,NT).NE.101) GOTO 1504                                     OPIH0619
      IF(TQC(2,NT).LE.0) GRAF=2                                         OPIH0620
      IF(TQC(2,NT).GT.0) GRAF=1                                         OPIH0621
 1504 IF(II.LE.0) GO TO 1502                                            OPIH0622
      IF(GRAF.EQ.1) VIZM=12                                             OPIH0623
      IF(GRAF.EQ.2) VIZM=11                                             OPIH0624
      IF(GRAF.EQ.3) VIZM=8                                              OPIH0625
      GO TO 1503                                                        OPIH0626
 1502 IF(GRAFS.EQ.1.AND.GRAF.EQ.2) VIZM=14                              OPIH0627
      IF(GRAFS.EQ.1.AND.GRAF.EQ.3) VIZM=10                              OPIH0628
      IF(GRAFS.EQ.2.AND.GRAF.EQ.3) VIZM=9                               OPIH0629
      IF(GRAFS.EQ.3.AND.GRAF.EQ.2) VIZM=13                              OPIH0630
      IF(GRAFS.EQ.3.AND.GRAF.EQ.1) VIZM=18                              OPIH0631
      IF(GRAFS.EQ.1.AND.GRAF.EQ.1) VIZM=19                              OPIH0632
      IF(GRAFS.EQ.2.AND.GRAF.EQ.2) VIZM=20                              OPIH0633
      IF(GRAFS.EQ.3.AND.GRAF.EQ.3) VIZM=21                              OPIH0634
 1503 IF(PPPP.GT.0) GOTO 1900                                           OPIH0635
      WRITE(12,1407) DEP,MAS,(NOMZ(J),J=1,4),VIZM,IPRIT,                OPIH0636
     *IDAT,BMAS,SMAS                                                    OPIH0637
 1900 CONTINUE                                                          OPIH0638
      WRITE(N3) NXZ,(IW(I),I=1,NU),IQ,IP,ID,LD,LZ,LF,MP,MS,NA,NS,NT     OPIH0639
      WRITE(N3) ((A(I,J),I=1,26),J=1,NA)                                OPIH0640
      WRITE(N3) ((S(I,J),I=1,2),J=1,NS),((P(I,J),I=1,2),J=1,NS)         OPIH0641
      WRITE(N3) ((TQ(I,J),I=1,6),J=1,NT),((QZ(I,J),I=1,4),J=1,NT)       OPIH0642
      WRITE(N3) ((TQC(I,J),I=1,4),J=1,NT)                               OPIH0643
      KOLPZR=KOLPZR+1                                                   OPIH0644
  140 IF(II) 36,36,37                                                   OPIH0645
   36 GO TO 14                                                          OPIH0646
   37 GO TO 12                                                          OPIH0647
   18 REWIND N1                                                         OPIH0648
      IF(MPZ.NE.0) GOTO 39                                              OPIH0649
      DO 38 K=1,IA                                                      OPIH0650
      IF(IZ(18,K).LE.0) GO TO 39                                        OPIH0651
   38 CONTINUE                                                          OPIH0652
   39 II=1                                                              OPIH0653
   12 READ(N2) NXZ,(IW(I),I=1,NU),IQ,IP,ID,LZ,MP,NA,NS,NT               OPIH0654
      IF(NA.LE.-1000) GO TO 40                                          OPIH0655
      READ(N2) ((A(I,J),I=1,26),J=1,NA)                                 OPIH0656
      READ(N2) ((S(I,J),I=1,2),J=1,NS),((P(I,J),I=1,2),J=1,NS)          OPIH0657
      READ(N2) ((TQ(I,J),I=1,6),J=1,NT),((QZ(I,J),I=1,4),J=1,NT)        OPIH0658
      READ(N2) ((TQC(I,J),I=1,4),J=1,NT)                                OPIH0659
      PPPP=0                                                            OPIH0660
      IPRNZ=0                                                           OPIH0661
      KZI=KZI+1                                                         OPIH0662
      MS=0                                                              OPIH0663
      GRAF=0                                                            OPIH0664
      GRAFS=0                                                           OPIH0665
      VIZM=0                                                            OPIH0666
      IDAT=0                                                            OPIH0667
      IPRIT=0                                                           OPIH0668
      BMAS=IPROB                                                        OPIH0669
      SMAS=IPROB                                                        OPIH0670
      N=A(10,1)                                                         OPIH0671
      N=S(1,N)                                                          OPIH0672
      N=A(12,N)+A(13,N)-1                                               OPIH0673
      MAS=TQ(2,N)/100                                                   OPIH0674
      N=TQ(1,N)                                                         OPIH0675
      DEP=U(1,N)                                                        OPIH0676
      DO 20 K=1,IA                                                      OPIH0677
      IF(IZ(18,K).GE.1) GO TO 20                                        OPIH0678
      DO 21 J=1,4                                                       OPIH0679
      IF(NXZ(J,1).NE.IM(J,K)) GO TO 20                                  OPIH0680
   21 CONTINUE                                                          OPIH0681
      IF(TQC(1,1).NE.MEC) GOTO 9998                                     OPIH0682
      IF(TQC(2,1).EQ.0) GOTO 9998                                       OPIH0683
      IF(IZ(11,K).EQ.PLS.OR.IZ(12,K).EQ.PLS.OR.IZ(16,K).NE.0.OR.        OPIH0684
     1IZ(17,K).NE.0) IZ(18,K)=0                                         OPIH0685
      IF(IZ(11,K).EQ.PLS.OR.IZ(12,K).EQ.PLS.OR.IZ(16,K).NE.0.OR.        OPIH0686
     1IZ(17,K).NE.0) GOTO 9997                                          OPIH0687
 9998 CONTINUE                                                          OPIH0688
      DO 1800 J=1,4                                                     OPIH0689
      NOMZ(J)=NXZ(J,1)                                                  OPIH0690
 1800 CONTINUE                                                          OPIH0691
      XVOST2=D                                                          OPIH0692
      XVOST(2)=ZAK(8)                                                   OPIH0693
      IF(XVOST2.NE.D) GOTO 8889                                         OPIH0694
      DO 8886 J=1,7                                                     OPIH0695
 8886 ZAK(9-J)=ZAK(8-J)                                                 OPIH0696
      XVOST(2)=ZAK(2)                                                   OPIH0697
      NOMZ(1)=D                                                         OPIH0698
      ZAK(2)=XVOST(2)                                                   OPIH0699
 8889 IZ(18,K)=1                                                        OPIH0700
      IP1=IP                                                            OPIH0701
      IPP=0                                                             OPIH0702
      IF(IZ(16,K).GT.0) IP=IZ(16,K)                                     OPIH0703
      IF(IZ(16,K).EQ.8) IP=0                                            OPIH0704
      IF(IZ(16,K).GE.0) IPRIT=IZ(16,K)                                  OPIH0705
      IF(IZ(16,K).GE.0) VIZM=0                                          OPIH0706
      IF(IZ(16,K).EQ.0) GOTO 13                                         OPIH0707
      IF(QZ(4,A(12,S(1,A(10,1)))+A(13,S(1,A(10,1)))-1).LT.1000000.AND.IPOPIH0708
     1.GT.3) GOTO 13                                                    OPIH0709
      IF(QZ(4,A(12,S(1,A(10,1)))+A(13,S(1,A(10,1)))-1).LT.1000000.AND.IPOPIH0710
     11.EQ.0.AND.IP.GE.3) GOTO 13                                       OPIH0711
      IF(A(10,1).GE.NA) GOTO 13                                         OPIH0712
      IF(IP1.GT.3.AND.IP.GT.3) GOTO 13                                  OPIH0713
      IF(IP.EQ.2.AND.IP1.EQ.3) GOTO 13                                  OPIH0714
      IF(IP.EQ.3.AND.IP1.EQ.2) GOTO 13                                  OPIH0715
      TQ1(1,1)=TQ(1,1)                                                  OPIH0716
      TQ1(2,1)=TQ(2,1)                                                  OPIH0717
      TQ1(3,1)=TQ(3,1)                                                  OPIH0718
      TQ1(4,1)=0                                                        OPIH0719
      TQ1(5,1)=0                                                        OPIH0720
      TQ1(6,1)=0                                                        OPIH0721
      DO 7005 J=2,NT                                                    OPIH0722
      TQ1(1,J)=TQ(1,J)                                                  OPIH0723
      TQ1(2,J)=TQ(2,J)                                                  OPIH0724
      TQ1(3,J)=TQ(3,J)                                                  OPIH0725
      TQ1(4,J)=TQ(4,J)                                                  OPIH0726
      TQ1(5,J)=TQ(5,J)                                                  OPIH0727
      TQ1(6,J)=TQ(6,J)                                                  OPIH0728
      IF(TQC(2,J).EQ.0) TQ1(4,J)=LZ                                     OPIH0729
      IF(TQC(2,J).EQ.0) TQ1(5,J)=LZ                                     OPIH0730
      IF(TQC(2,J).EQ.0) TQ1(6,J)=LZ                                     OPIH0731
 7005 CONTINUE                                                          OPIH0732
      ASSIGN 13 TO MM                                                   OPIH0733
      GOTO 505                                                          OPIH0734
   13 CONTINUE                                                          OPIH0735
      IF(IZ(17,K).GT.0) MP=IZ(17,K)                                     OPIH0736
      IF(IZ(17,K).GT.0) IDAT=IZ(17,K)                                   OPIH0737
      IF(IZ(17,K).GT.0) VIZM=0                                          OPIH0738
      IF(IZ(14,K).GT.0) GO TO 22                                        OPIH0739
      IF(IZ(15,K).GT.0) GO TO 35                                        OPIH0740
      IF(IZ(12,K).EQ.PLS) VIZM=15                                       OPIH0741
      IF(IZ(12,K).EQ.PLS) GO TO 5                                       OPIH0742
      IF(IZ(11,K).EQ.PLS) VIZM=4                                        OPIH0743
      IF(IZ(11,K).EQ.PLS) WRITE(12,1407) DEP,MAS,                       OPIH0744
     *(NOMZ(J),J=1,4),VIZM,IPRIT,IDAT,BMAS,SMAS                         OPIH0745
      IF(IZ(11,K).EQ.PLS) GO TO 12                                      OPIH0746
      WRITE(12,1407) DEP,MAS,(NOMZ(J),J=1,4),VIZM,IPRIT,                OPIH0747
     *IDAT,BMAS,SMAS                                                    OPIH0748
   20 CONTINUE                                                          OPIH0749
      K=IA+1                                                            OPIH0750
      IPRNZ=1                                                           OPIH0751
      IF(DEP.NE.10.AND.DEP.NE.15) GOTO 25                               OPIH0752
      IF(NXZ(5,1).EQ.MET) GOTO 25                                       OPIH0753
      IF(IP.GE.4) GOTO 25                                               OPIH0754
      IF(IP.LE.2) GOTO 8883                                             OPIH0755
      IF(DEP.EQ.15) GOTO 8884                                           OPIH0756
      IF(TQ(2,2)/100.NE.15) GOTO 8883                                   OPIH0757
      IF(TQC(2,2).EQ.0) GOTO 8883                                       OPIH0758
      GOTO 25                                                           OPIH0759
 8884 IF(TQ(2,NT).NE.101) GOTO 8883                                     OPIH0760
      IF(TQC(2,NT).EQ.0) GOTO 8883                                      OPIH0761
      GOTO 25                                                           OPIH0762
 8883 CONTINUE                                                          OPIH0763
      DO 8006 J=1,4                                                     OPIH0764
 8006 NOMZ(J)=NXZ(J,1)                                                  OPIH0765
      XVOST2=D                                                          OPIH0766
      XVOST(2)=ZAK(8)                                                   OPIH0767
      IF(XVOST2.NE.D) GOTO 8882                                         OPIH0768
      DO 8881 J=1,7                                                     OPIH0769
 8881 ZAK(9-J)=ZAK(8-J)                                                 OPIH0770
      XVOST(2)=ZAK(2)                                                   OPIH0771
      NOMZ(1)=D                                                         OPIH0772
      ZAK(2)=XVOST(2)                                                   OPIH0773
 8882 IF(IP.EQ.0) iwf=nd                                                OPIH0774
      if(ip.eq.0) goto 8008
      GOTO 35                                                           OPIH0775
   22 IWF=IZ(14,K)                                                      OPIH0776
 8008 CALL KALDNI(IWF,0,KLD)                                            OPIH0777
      IWF=KLD(1)/10                                                     OPIH0778
      DO 24 I=LP,LP1                                                    OPIH0779
      IF(IWF.EQ.KDP(I)/10) GO TO 41                                     OPIH0780
   24 I1=I                                                              OPIH0781
      I=I1                                                              OPIH0782
   41 LI=I-LP                                                           OPIH0783
      LZ=0                                                              OPIH0784
      DO 722 KOL=1,2                                                    OPIH0785
      DO 159 J3=2,NA                                                    OPIH0786
      J2=A(12,J3)+A(13,J3)-1                                            OPIH0787
      J1=A(12,J3)                                                       OPIH0788
      NPOS=J2-1                                                         OPIH0789
      DO 160 J6=J1,J2                                                   OPIH0790
      KJ=J2-J6                                                          OPIH0791
      J4=J1+KJ                                                          OPIH0792
      IF(J4.EQ.J2.AND.TQC(2,J4).NE.0) GOTO 162                          OPIH0793
      IF(TQC(2,J4).NE.0) GOTO 160                                       OPIH0794
      IF(KJ.EQ.0) GOTO 160                                              OPIH0795
      GOTO 702                                                          OPIH0796
  162 NOMP=A(14,J3)                                                     OPIH0797
      KOLP=A(15,J3)                                                     OPIH0798
      KON=NOMP+KOLP-1                                                   OPIH0799
      IF(NA.EQ.3.AND.A(9,2).EQ.A(9,3)) GOTO 160                         OPIH0800
      DO 712 J10=NOMP,KON                                               OPIH0801
      NSB=P(1,J10)                                                      OPIH0802
      IF(NSB.EQ.1) GOTO 712                                             OPIH0803
      NSB=A(12,NSB)                                                     OPIH0804
      IF(TQC(2,NSB).NE.0) GOTO 712                                      OPIH0805
      KJ=KJ+1                                                           OPIH0806
      GOTO 702                                                          OPIH0807
  712 CONTINUE                                                          OPIH0808
      GOTO 160                                                          OPIH0809
  702 KJ=KJ+1                                                           OPIH0810
      DO 161 JK=1,KJ                                                    OPIH0811
      J5=J1+JK-1                                                        OPIH0812
      IF(TQC(2,J5).EQ.0) GOTO 161                                       OPIH0813
      TQC(1,J5)=NDR                                                     OPIH0814
      TQC(3,J5)=0                                                       OPIH0815
  161 TQC(2,J5)=0                                                       OPIH0816
      GOTO 159                                                          OPIH0817
  160 CONTINUE                                                          OPIH0818
  159 CONTINUE                                                          OPIH0819
  722 CONTINUE                                                          OPIH0820
      J3=A(10,1)                                                        OPIH0821
      J3=A(11,1)+J3-1                                                   OPIH0822
      J3=S(1,J3)                                                        OPIH0823
      J2=A(12,J3)+A(13,J3)-1                                            OPIH0824
      J1=A(12,J3)                                                       OPIH0825
      DO 150 J6=J1,J2                                                   OPIH0826
      KJ=J2-J6                                                          OPIH0827
      J4=J1+KJ                                                          OPIH0828
      IF((TQ(2,J4)-TQ(2,J4)/100*100).NE.83) GOTO 150                    OPIH0829
      IF(QZ(1,J4).EQ.1.AND.QZ(2,J4).EQ.0) GOTO 150                      OPIH0830
      R=(QZ(1,J4)+0.0)/U(2,TQ(1,J4))                                    OPIH0831
      J5=INT(R)                                                         OPIH0832
      IF(R-J5.LE.0.0) J5=J5-1                                           OPIH0833
      J66=TQ(4,J4)-TQ(5,J4)                                             OPIH0834
      J7=J66-J5                                                         OPIH0835
      GOTO 151                                                          OPIH0836
  150 CONTINUE                                                          OPIH0837
      J7=0                                                              OPIH0838
 151  DO 152 J=2,NT                                                     OPIH0839
      IF(TQC(2,J).LE.0) GOTO 152                                        OPIH0840
      IF(J.GT.J4.AND.J.LE.J2) GOTO 153                                  OPIH0841
      IF(J7.LE.0) J7=0                                                  OPIH0842
      TQ(4,J)=TQ(4,J)-J7                                                OPIH0843
      IF(J.EQ.J4) GOTO 153                                              OPIH0844
      TQ(5,J)=TQ(5,J)-J7                                                OPIH0845
      TQ(6,J)=TQ(6,J)-J7                                                OPIH0846
 153  IF(TQ(4,J).GT.LZ) LZ=TQ(4,J)                                      OPIH0847
  152 CONTINUE                                                          OPIH0848
      LF=LI+LZ                                                          OPIH0849
      LD=KDP(I)/10*100+(ND-ND/100*100)                                  OPIH0850
      DO 26 M1=1,NT                                                     OPIH0851
      DO 26 M=4,6                                                       OPIH0852
   26 TQ(M,M1)=LF-TQ(M,M1)                                              OPIH0853
      CALL PODZAG(U,TQ,QZ,ZG,NT,LP,0,TQC,IP)                            OPIH0854
      CONTINUE                                                          OPIH0855
      IF(LF.LE.NKP.AND.IZ(13,K).EQ.PLS)  MS=0                           OPIH0856
      IF(IZ(13,K).NE.PLS) MS=0                                          OPIH0857
      IF(NXZ(1,1).NE.SZ1.AND.NXZ(1,1).NE.SZ2) GOTO 9                    OPIH0863
      TQC(2,2)=0                                                        OPIH0864
      GO TO 9                                                           OPIH0865
    5 IP=9                                                              OPIH0866
      IPRIT=9                                                           OPIH0867
   91 WRITE(12,1407) DEP,MAS,(NOMZ(J),J=1,4),VIZM,IPRIT,                OPIH0868
     *IDAT,BMAS,SMAS                                                    OPIH0869
   25 LZ=0                                                              OPIH0870
      IPPZ=0                                                            OPIH0871
      DO 7011 J=2,NT                                                    OPIH0872
      IF(TQC(2,J).EQ.0) GOTO 7011                                       OPIH0873
      IF(TQ(2,J).EQ.1512.OR.TQ(2,J).EQ.1513) IPPZ=1                     OPIH0874
      IF(TQ(2,J).EQ.1512.OR.TQ(2,J).EQ.1513) GOTO 7011                  OPIH0875
      IF(TQ(4,J).GE.LZ) LZ=TQ(4,J)                                      OPIH0876
 7011 CONTINUE                                                          OPIH0877
      IF(IPPZ.EQ.1) LZ=LZ+30                                            OPIH0878
 9997 CONTINUE                                                          OPIH0879
      WRITE(N4) NXZ,(IW(I),I=1,NU),IQ,IP,ID,LZ,MP,NA,NS,NT              OPIH0880
      WRITE(N4) ((A(I,J),I=1,26),J=1,NA)                                OPIH0881
      WRITE(N4) ((S(I,J),I=1,2),J=1,NS),((P(I,J),I=1,2),J=1,NS)         OPIH0882
      WRITE(N4) ((TQ(I,J),I=1,6),J=1,NT),((QZ(I,J),I=1,4),J=1,NT)       OPIH0883
      WRITE(N4) ((TQC(I,J),I=1,4),J=1,NT)                               OPIH0884
      KZ=KZ+1                                                           OPIH0885
      GO TO 12                                                          OPIH0886
   40 NA=-1000                                                          OPIH0887
      WRITE(N4) NXZ,(IW(I),I=1,NU),IQ,IP,ID,LZ,MP,NA,NS,NT              OPIH0888
      DO 44445 J=1,LP                                                   OPIH0890
      DO 44445 I=1,NUC                                                  OPIH0891
44445 ZG1(I,J)=ZG(I,J)/10                                               OPIH0892
      REWIND N2                                                         OPIH0894
      REWIND N4                                                         OPIH0895
      IF(NR.NE.0) GOTO70                                                OPIH0896
   42 NA=-1000                                                          OPIH0897
      WRITE(N3) NXZ,(IW(I),I=1,NU),IQ,IP,ID,LD,LZ,LF,MP,MS,NA,NS,NT     OPIH0898
   70 ND=ND-10000                                                       OPIH0899
      write(*,7110) ND                                                  OPIH0900
      write(*,7111) KZI                                                 OPIH0901
      write(*,71) KZ                                                    OPIH0902
      write(*,7112) KOLPZI                                              OPIH0903
      write(*,7113) KOLPZR                                              OPIH0904
      pause'Á‡ÔË¯Ë ‰‡ÌÌ˚Â ÔÓÚÓÍÓÎ‡'
      DO 1509 K=1,IA                                                    OPIH0905
      IF(IZ(18,K).GT.0) GO TO 1509                                      OPIH0906
      VIZM=17                                                           OPIH0907
      PROBEL=NPROB                                                      OPIH0908
      WRITE(12,1810) PROBEL,(IM(J,K),J=1,4),VIZM                        OPIH0909
 1810 FORMAT(A4,4A2,I2)                                                 OPIH0910
 1509 CONTINUE                                                          OPIH0911
      IF(NR.EQ.1) GOTO 49                                               OPIH0912
      DO 44446 J=1,LP                                                   OPIH0913
      DO 44446 I=1,NUC                                                  OPIH0914
44446 ZG1(I,J)=ZG(I,J)/10                                               OPIH0915
      WRITE(N3) ((ZG1(I,J),I=1,NUC),J=1,LP),((F(I,J),I=1,NUC),J=1,3)    OPIH0916
      REWIND N3                                                         OPIH0917
  49  STOP                                                              OPIH0918
      END                                                               OPIH0919
      SUBROUTINE PODZAG(U,TQ,QZ,ZG,NT,LP,IND,TQC,IP)                    OPIH0920
      INTEGER QZ(4,1),ZG(100,1)                                         OPIH0921
      INTEGER*2 TQC(4,1)                                                OPIH0922
      INTEGER*2 U(20,1),TQ(6,1)                                         OPIH0923
      DO 1 J1=1,NT                                                      OPIH0924
      IF(TQC(2,J1).LE.0) GOTO 1                                         OPIH0925
  11  IF(TQ(4,J1).GT.LP.OR.TQ(5,J1).LT.0) GOTO 1                        OPIH0926
      IF(TQ(5,J1).GT.LP) I2=LP+1                                        OPIH0927
  10  IF(TQ(5,J1).LE.LP) I2=TQ(5,J1)                                    OPIH0928
      I3=TQ(1,J1)                                                       OPIH0929
      I4=U(7,I3)                                                        OPIH0930
      IF(I4.LE.0) GO TO 1                                               OPIH0931
      I1=TQ(4,J1)                                                       OPIH0932
      IF(I1.LE.0) I1=1                                                  OPIH0933
      IF(IND) 4,4,5                                                     OPIH0934
    4 IF(I1.GE.I2) GOTO 8                                               OPIH0935
      I=I2-1                                                            OPIH0936
      DO 2 J=I1,I                                                       OPIH0937
      KZG=ZG(I4,J)                                                      OPIH0938
      IF(QZ(4,J1).LT.1000000.OR.IP.NE.0.OR.U(2,I3).NE.820) KZG=KZG+U(2,IOPIH0939
     13)                                                                OPIH0940
      IF(QZ(4,J1).GE.1000000.AND.IP.EQ.0.AND.U(2,I3).EQ.820) KZG=KZG+U(2OPIH0941
     1,I3)*2                                                            OPIH0942
      ZG(I4,J)=KZG                                                      OPIH0943
    2 CONTINUE                                                          OPIH0944
      IF(I2.EQ.LP+1) GOTO 1                                             OPIH0945
    3 IF(QZ(4,J1).LT.1000000.OR.IP.NE.0.OR.U(2,I3).NE.820) KZG=ZG(I4,I2)OPIH0946
     1+QZ(1,J1)-U(2,I3)*(I2-I1)                                         OPIH0947
      IF(QZ(4,J1).GE.1000000.AND.IP.EQ.0.AND.U(2,I3).EQ.820) KZG=ZG(I4,IOPIH0948
     12)+QZ(1,J1)-U(2,I3)*2*(I2-I1)                                     OPIH0949
      ZG(I4,I2)=KZG                                                     OPIH0950
      IF(KZG.LT.0) ZG(I4,I2)=0                                          OPIH0951
      GO TO 1                                                           OPIH0952
  8   IF(QZ(1,J1).LE.U(2,I3)) GOTO 3                                    OPIH0953
      KTR=QZ(1,J1)/LP                                                   OPIH0954
      LPR=LP                                                            OPIH0955
      DO 9 J=1,LPR                                                      OPIH0956
    9 ZG(I4,J)=ZG(I4,J)+KTR                                             OPIH0957
      GOTO 1                                                            OPIH0958
    5 IP1=IP                                                            OPIH0959
      IF(I1.GE.I2) GOTO 7                                               OPIH0960
      I=I2-1                                                            OPIH0961
      DO 6 J=I1,I                                                       OPIH0962
      IF(QZ(4,J1).LT.1000000.OR.IP1.NE.0.OR.U(2,I3).NE.820) ZG(I4,J)=ZG OPIH0963
     1(I4,J)-U(2,I3)                                                    OPIH0964
      IF(ZG(I4,J).LT.0)  ZG(I4,J)=0                                     OPIH0965
      IF(QZ(4,J1).GE.1000000.AND.IP1.EQ.0.AND.U(2,I3).EQ.820) ZG(I4,J)=ZOPIH0966
     1G(I4,J)-U(2,I3)*2                                                 OPIH0967
      IF(ZG(I4,J).LT.0)  ZG(I4,J)=0                                     OPIH0968
    6 CONTINUE                                                          OPIH0969
      IF(I2.EQ.LP+1) GOTO1                                              OPIH0970
    7 IF(QZ(4,J1).LT.1000000.OR.IP1.NE.0.OR.U(2,I3).NE.820) ZG(I4,I2)=ZGOPIH0971
     1(I4,I2)+U(2,I3)*(I2-I1)-QZ(1,J1)                                  OPIH0972
      IF(QZ(4,J1).GE.1000000.AND.IP1.EQ.0.AND.U(2,I3).EQ.820) ZG(I4,I2)=OPIH0973
     1ZG(I4,I2)+U(2,I3)*2*(I2-I1)-QZ(1,J1)                              OPIH0974
      IF(ZG(I4,I2).LT.0) ZG(I4,I2)=0                                    OPIH0975
    1 CONTINUE                                                          OPIH0976
      RETURN                                                            OPIH0977
      END                                                               OPIH0978
 
