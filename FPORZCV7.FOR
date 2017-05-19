      INTEGER*2 A(26,200),S(2,200),P(2,200),TQ(6,500),                  RZOC0001
     *TQC(4,500),U(20,100),NXZ(10,4)                                    RZOC0002
      INTEGER*2 U6(100),U2(100),ND/11/                                  RZOC0003
      INTEGER*2 EC(3,100),MET/'2 '/,MSIZ2,STZZ2                         RZOC0004
      LOGICAL*1 MSIZ,STZZ,MSIZ1(2)/' ',' '/,STZZ1(2)/'0',' '/,MSIZ4     RZOC0005
      INTEGER QZ1(500),QZ(4,500),UW(2,200),IW(100),XIFR(3000)           RZOC0006
      REAL*8 SMAT                                                       RZOC0007
      DATA L1,L2,NF/4,1,2/,LIMA,LIMS,LIMT/2*200,500/                    RZOC0010
      DATA L/72/,NKMAX/'/'/,NK/1/,JFM/''/,F3/'3'/                       6RZOC0011
  200 FORMAT(I3,I2,4A2,I5,22A2,I5,I2,I1,I3,I8,I5,I8,F10.2)              RZOC0018
      LI=1                                                              RZOC0019
  150 FORMAT(I8,64X,I9)                                                 RZOC0020
    6 FORMAT(A1,20A2,I2,I5,15A2,2X,5A2,I5,I8)                           RZOC0021
    7 FORMAT(I8,I1,I2)                                                  RZOC0022
    9 FORMAT(20X,'ÏPOTOKOË ÔOPMÈPOBAHÈß ÏOPTÔEËß ÏO ÈÏ   ',2X,I8////10X,RZOC0023
     *'B ÏOPTÔEËE ÁÛËO-',I5/10X,'BBEÄEHO ÇAKAÇOB-',I5/10X,'B ÏOPTÔEËE   RZOC0024
     * CTAËO-',I5////)                                                  RZOC0025
   10 FORMAT(A1,170X)                                                   RZOC0026
   20 FORMAT(//10X,'**** ÏPOBEPÈTÜ ÄAHHÛE ÄËß ÇAKAÇA ',10A2,' (B ÏOPTÔEËRZOC0027
     1Ü HE ÏOMEÙEH).')                                                  RZOC0028
      OPEN(2,FILE='F:\ASUIPW\tek_INF\omoih.dat')
      OPEN(11,FILE='F:\ASUIPW\tek_INF\cmatin.dat',ACCESS='DIRECT',
     1FORM='FORMATTED',RECL=81)
      open(8,file='F:\ASUIPW\tek_INF\kdz.dat')
      open(9,file='F:\ASUIPW\tek_INF\bpm.dat')
      LIL=1                                                             RZOC0029
      KI=1                                                              RZOC0030
      MK=1                                                              RZOC0031
      M=0
      NSP=0                                                             RZOC0032
      NMT=0
      IDEP=0
      IERL=50
      open(666,file='tmp.txt')
      read(666,777) idata
  777 format(i8)
      n=0
      DO 151 J=1,10000                                                  RZOC0039
      READ(ND,150,REC=J) XIFR(J),I2                                     RZOC0040
      IF(I2.GT.999990000) GOTO  152                                     RZOC0041
  151 CONTINUE                                                          RZOC0042
  152 NM=J-1                                                            RZOC0043
      OPEN(UNIT=L1,FILE='f:\asyip\U.DAT',FORM='UNFORMATTED')
      READ(L1) NW,NU                                                    RZOC0044
      READ(L1) ((UW(I,J),I=1,2),J=1,NW),((U(I,J),I=1,20),J=1,NU)        RZOC0045
      REWIND L1                                                         RZOC0046
      NUU=NU-1                                                          RZOC0047
      DO 300 I=1,NW                                                     RZOC0048
      IF(UW(1,I).NE.170501) GOTO 300                                    RZOC0049
      K10=UW(2,I)                                                       RZOC0050
      GOTO 301                                                          RZOC0051
  300 CONTINUE                                                          RZOC0052
  301 L1=3                                                              RZOC0053
      DO 3000 I=1,NU                                                    RZOC0054
      U6(I)=U(6,I)                                                      RZOC0055
 3000 U2(I)=U(2,I)                                                      RZOC0056
      IF(N.LT.0) GOTO 4                                                 RZOC0057
      IF(N.EQ.0) GOTO 4                                                 RZOC0058
      M=N
      N=M-1                                                             RZOC0070
    4 CONTINUE                                                          RZOC0072
      READ(NF,6,ERR=5,END=5) JFM,((NXZ(I,J),I=1,10),J=1,2),IP,IQ,
     1((NXZ(I,J),I=1,10),J=3,4),ID,MP                                   RZOC0075
      NK0=NK                                                            RZOC0086
      NK=NK+1                                                           RZOC0087
      IF(JFM.EQ.NKMAX) GOTO 5                                           RZOC0088
      IF(JFM.NE.F3) GOTO 4                                              RZOC0089
      IF(IP.NE.3) IP3=0                                                 RZOC0090
      IF(IP.EQ.3) IP3=1                                                 RZOC0091
      IF(IP.EQ.3) IP=2                                                  RZOC0092
      NSP=NSP+1                                                         RZOC0093
      CALL STRSSV(NF,NA,NS,A,S,LIMA,LIMS,NK,NKMAX)                      RZOC0094
      IF(NA.LE.1) GOTO 4                                                RZOC0095
      IRCZ=0                                                            RZOC0096
      ITRZ=1                                                            RZOC0097
      CALL TCLZKZ(NF,LIMT,NW,NU,NA,NT,UW,U,A,TQ,TQC,QZ,EC,NE,NK,NKMAX,ITRZOC0098
     1RZ,IRCZ)                                                          RZOC0099
      CALL STRPRM(LIMS,NA,NS,NXZ,A,S,P,IQ)                              RZOC0100
      IND=IERL                                                          RZOC0101
      CALL CHKPRT(LIMT,NA,NT,A,S,P,TQ,QZ,TQC,U,NU,IND)                  RZOC0102
      CALL SGOKIZ(NA,NT,A,S,P,TQ,QZ,TQC,U)                              RZOC0103
      CALL SVODCA(NT,TQ,QZ,NU,NW,IW,UW,IRAS,ITRZ,IRCZ)                  RZOC0104
      DO 64 J=2,NT                                                      RZOC0105
   64 QZ1(J)=TQ(4,J)*1000000+TQ(5,J)*1000+TQ(6,J)                       RZOC0106
      IF(IP.GT.2) GOTO 88                                               RZOC0107
      DO 500  J=1,NU                                                    RZOC0108
      IF(IP.EQ.0.OR.IP.EQ.1) U(6,J)=0                                   RZOC0109
      IF(IP.EQ.0.AND.U(2,J).EQ.820) U(2,J)=U(2,J)*2                     RZOC0110
  500 CONTINUE                                                          RZOC0111
      CALL PRIOR(A,TQ,QZ,NT,NA,S,U,IP)                                  RZOC0112
      IF(IP.GT.0) CALL SGRLZE(LZ,NA,NT,NU,A,S,P,TQ,QZ,TQC,U,IP,IW)      RZOC0113
      CALL MEHDET(NA,NT,A,S,P,TQ,TQC)                                   RZOC0114
      IF(TQ(2,2)/100.EQ.5) TQ(4,2)=QZ1(2)/1000000                       RZOC0115
      IF(TQ(2,2)/100.EQ.5) TQ(5,2)=QZ1(2)/1000-TQ(4,2)*1                RZOC0116
     1000                                                               RZOC0117
      IF(TQ(2,2)/100.EQ.15) TQ(6,2)=QZ1(2)-TQ(4,2)*100000               RZOC0118
     10-TQ(5,2)*1000                                                    RZOC0119
   88 CONTINUE                                                          RZOC0120
      CALL MEHDET(NA,NT,A,S,P,TQ,TQC)                                   RZOC0121
      IF(IP.GT.0) CALL SGRLZE(LZ,NA,NT,NU,A,S,P,TQ,QZ,TQC,U,IP,IW)      RZOC0122
      I=NS+1                                                            RZOC0123
      IF(NE.LE.0) GOTO 60                                               RZOC0124
      DO 61 J=1,NE                                                      RZOC0125
      K=EC(2,J)                                                         RZOC0126
      DO 62 J1=2,NA                                                     RZOC0127
      IF(A(9,J1).EQ.K) GOTO 63                                          RZOC0128
   62 CONTINUE                                                          RZOC0129
      GOTO 61                                                           RZOC0130
   63 J1=A(12,J1)+EC(3,J)-1                                             RZOC0131
      NS=NS+1                                                           RZOC0132
      S(1,NS)=J1                                                        RZOC0133
      S(2,NS)=EC(1,J)                                                   RZOC0134
   61 CONTINUE                                                          RZOC0135
      IF(NS.LT.I) GOTO 60                                               RZOC0136
      NE=I                                                              RZOC0137
      CALL  EXTCON(LZ,NA,NS,NT,NE,NU,A,S,P,TQ,QZ,TQC,U,IP,IW)           RZOC0138
   60 NE=I                                                              RZOC0139
      NS=NE-1                                                           RZOC0140
      IF(TQ(2,2).NE.501) GOTO 400                                       RZOC0142
      CALL ZAGOP(A,S,P,TQ,TQC,U,NXZ,IW,QZ,UW,K10,NA,NS,NT,LZ,ITRZ,IRCZ) RZOC0143
  400 CONTINUE                                                          RZOC0144
      IF(IP3.EQ.1) IP=3                                                 RZOC0145
      DO 501 J=1,NU                                                     RZOC0146
      U(2,J)=U2(J)                                                      RZOC0147
  501 U(6,J)=U6(J)                                                      RZOC0148
      NZ=A(10,1)                                                        RZOC0149
      NZ=S(1,NZ)                                                        RZOC0150
      NZ=A(12,NZ)+A(13,NZ)-1                                            RZOC0151
   70 LZ=0                                                              RZOC0152
      IPPZ=0                                                            RZOC0153
      DO 71 J=2,NT                                                      RZOC0154
      IF(TQ(2,J).EQ.512.OR.TQ(2,J).EQ.513) IPPZ=1                       RZOC0155
      IF(TQ(2,J).EQ.512.OR.TQ(2,J).EQ.513) GOTO 71                      RZOC0156
      IF(TQ(4,J).GT.LZ) LZ=TQ(4,J)                                      RZOC0157
   71 CONTINUE                                                          RZOC0158
      IF(IPPZ.EQ.1) LZ=LZ+30                                            RZOC0159
      CALL VEDMAT(A,NA,NXZ,P,L,XIFR,NM,ND,MK,SMAT)                      RZOC0160
      NSTU=TQ(1,NZ)                                                     RZOC0161
      nuhb=TQ(2,nz)/100
      KW=0                                                              RZOC0162
      DO 31 I=1,NUU                                                     RZOC0163
  31  KW=KW+IW(I)                                                       RZOC0164
      IF(NUHB.EQ.3.OR.NUHB.EQ.4) ID=(KW*105+500)/1000                   RZOC0165
      IF(NUHB.EQ.1.OR.NUHB.EQ.2) ID=(KW*112+500)/1000
      OPEN(3,FILE='F:\ASUIPW\tek_INF\SPTR.DAT')
      DO 201 J=1,NU                                                     RZOC0167
      IF(IW(J).EQ.0) GOTO 201                                           RZOC0168
      IF(IW(J).GT.99999) GOTO 1983                                      RZOC0169
      WRITE(3,200) U(1,NSTU),NUHB,(NXZ(I,1),I=1,4),IQ,(NXZ(I,4),I=1,5),
     *(NXZ(I,2),I=1,9),(U(I,J),I=13,20),IW(J),U(8,J),IP,LZ,MP,ID,IRAS,
     *SMAT                                                              RZOC0171
      GOTO 201                                                          RZOC0172
 1983 KOR=99999                                                         RZOC0173
      KOP=IW(J)-99999                                                   RZOC0174
      WRITE(3,200) U(1,NSTU),NUHB,(NXZ(I,1),I=1,4),IQ,(NXZ(I,4),I=1,5), RZOC0175
     *(NXZ(I,2),I=1,9),(U(I,J),I=13,20),KOR,U(8,J),IP,LZ,MP,ID,IRAS,
     *SMAT                                                              RZOC0176
      IF(KOP.LE.99999) GOTO 1984                                        RZOC0177
      KOP=KOP-99999                                                     RZOC0178
      WRITE(3,200) U(1,NSTU),NUHB,(NXZ(I,1),I=1,4),IQ,(NXZ(I,4),I=1,5), RZOC0179
     *(NXZ(I,2),I=1,9),(U(I,J),I=13,20),KOR,U(8,J),IP,LZ,MP,ID,IRAS,
     *SMAT   
      IF(KOP.LE.99999) GOTO 1984                                        RZOC0177
      KOP=KOP-99999                                                     RZOC0178
      WRITE(3,200) U(1,NSTU),NUHB,(NXZ(I,1),I=1,4),IQ,(NXZ(I,4),I=1,5), RZOC0179
     *(NXZ(I,2),I=1,9),(U(I,J),I=13,20),KOR,U(8,J),IP,LZ,MP,ID,IRAS,
     *SMAT   
      IF(KOP.LE.99999) GOTO 1984                                        RZOC0177
      KOP=KOP-99999                                                     RZOC0178
      WRITE(3,200) U(1,NSTU),NUHB,(NXZ(I,1),I=1,4),IQ,(NXZ(I,4),I=1,5), RZOC0179
     *(NXZ(I,2),I=1,9),(U(I,J),I=13,20),KOR,U(8,J),IP,LZ,MP,ID,IRAS,
     *SMAT   
      IF(KOP.LE.99999) GOTO 1984                                        RZOC0181
      KOP=KOP-99999                                                     RZOC0182
      WRITE(3,200) U(1,NSTU),NUHB,(NXZ(I,1),I=1,4),IQ,(NXZ(I,4),I=1,5), RZOC0183
     *(NXZ(I,2),I=1,9),(U(I,J),I=13,20),KOR,U(8,J),IP,LZ,MP,ID,IRAS,
     *SMAT   
 1984 WRITE(3,200) U(1,NSTU),NUHB,(NXZ(I,1),I=1,4),IQ,(NXZ(I,4),I=1,5), RZOC0185
     *(NXZ(I,2),I=1,9),(U(I,J),I=13,20),KOP,U(8,J),IP,LZ,MP,ID,IRAS,
     *SMAT   
  201 CONTINUE                                                          RZOC0187
      IF(IDEP.EQ.0)GOTO 410                                             RZOC0188
  410 CALL KDZ(A,S,P,TQ,TQC,U,NXZ,QZ,UW,IW,NW,NU,KI,LIL,NT,IQ)          RZOC0190
  210 CONTINUE                                                          RZOC0191
      DO 140 J=1,NA                                                     RZOC0192
      IF(A(11,J).EQ.0) A(10,J)=0                                        RZOC0193
  140 CONTINUE                                                          RZOC0194
      IF(TQ(2,2).EQ.501) QZ(4,NT)=QZ(4,1)                               RZOC0198
      NXZ(10,1)=MET                                                     RZOC0199
      N=N+1                                                             RZOC0206
      GOTO 4                                                            RZOC0207
    5 NA=-1000                                                          RZOC0208
      PRINT 9,IDATA,M,NSP,N                                             RZOC0211
      pause'çàïèøè êîëè÷åñòâî çàêàçîâ è íàæìè êë"ENTER"'
      REWIND L2                                                         RZOC0212
      STOP
      END                                                               RZOC0214
      SUBROUTINE PRIOR(A,TQ,QZ,NT,NA,S,U,IP)                            RZOC0215
      INTEGER*2 A(26,200),TQ(6,500),PRIZN(500)                          RZOC0216
      INTEGER*2 U(20,100)                                               RZOC0217
      INTEGER*2 UR(200),NU(10),KU(10)                                   RZOC0218
      INTEGER*2 S(2,200),SB                                             RZOC0219
      INTEGER QZ(4,500),TR                                              RZOC0220
      INTEGER*2 SLES/'83'/                                              RZOC0221
   90 CONTINUE                                                          RZOC0222
      IF(IP.EQ.1) INOP=100                                              RZOC0223
      IF(IP.EQ.2) INOP=200                                              RZOC0224
      IF(IP.EQ.0) INOP=0                                                RZOC0225
      DO 1 I=2,NT                                                       RZOC0226
      IF(IP.EQ.1.OR.IP.EQ.0) GOTO 91                                    RZOC0227
      GOTO 1                                                            RZOC0228
   91 R=(QZ(1,I)+0.0)/U(2,TQ(1,I))                                      RZOC0229
      J4=INT(R)                                                         RZOC0230
      IF(R-J4.LE.0.0) J4=J4-1                                           RZOC0231
      TQ(6,I)=TQ(5,I)-U(6,TQ(1,I))                                      RZOC0232
      TQ(4,I)=TQ(5,I)+J4                                                RZOC0233
    1 PRIZN(I)=0                                                        RZOC0234
      DO 2 I=2,NA                                                       RZOC0235
      NACH=A(12,I)                                                      RZOC0236
      IPR=PRIZN(NACH)                                                   RZOC0237
      IF(IPR.GT.0) GOTO 2                                               RZOC0238
      KON=NACH+A(13,I)-1                                                RZOC0239
      J=KON                                                             RZOC0240
      TR=0                                                              RZOC0241
      KRAN=0                                                            RZOC0242
    5 IF(TQ(5,J).EQ.TQ(6,J)) GOTO 31                                    RZOC0243
      IF(KRAN.EQ.0) GOTO 6                                              RZOC0244
      K=J+1                                                             RZOC0245
      GOTO 7                                                            RZOC0246
   31 IF(TQ(4,J).EQ.TQ(5,J)) GOTO 32                                    RZOC0247
      IF(KRAN.NE.0) K=J+1                                               RZOC0248
      IF(KRAN.NE.0) GOTO 7                                              RZOC0249
      N=J                                                               RZOC0250
      KRAN=1                                                            RZOC0251
      ID2=TQ(4,J)-TQ(5,J)                                               RZOC0252
      TR=QZ(1,J)-U(2,TQ(1,J))*ID2                                       RZOC0253
      TR=TR+INOP                                                        RZOC0254
      IF(TR.LT.U(2,TQ(1,J))) GOTO 6                                     RZOC0255
      KRAN=0                                                            RZOC0256
      TR=0                                                              RZOC0257
      GOTO 6                                                            RZOC0258
   32 IF(KRAN.EQ.0) N=J                                                 RZOC0259
      KRAN=1                                                            RZOC0260
      TR=TR+QZ(1,J)+INOP                                                RZOC0261
      I1=TQ(2,J)-TQ(2,J)/100*100                                        RZOC0262
      IF(I1.EQ.SLES.AND.QZ(1,J).EQ.1) TR=TR-INOP                        RZOC0263
      IF(TR.GE.U(2,TQ(1,J))) GOTO 4                                     RZOC0264
    6 J=J-1                                                             RZOC0265
      IF(J.GE.NACH) GOTO 5                                              RZOC0266
      IF(KRAN.EQ.0) PRIZN(NACH)=1                                       RZOC0267
      IF(KRAN.EQ.0) GOTO 2                                              RZOC0268
      K=NACH                                                            RZOC0269
      GOTO 7                                                            RZOC0270
    4 IF(N.EQ.J) K=J                                                    RZOC0271
      IF(N.EQ.J) J=J-1                                                  RZOC0272
      IF(N.EQ.J) GOTO 7                                                 RZOC0273
      K=J+1                                                             RZOC0274
    7 IF(K.EQ.N) GOTO 33                                                RZOC0275
      M=K                                                               RZOC0276
    8 TQ(4,M)=TQ(4,N)                                                   RZOC0277
      TQ(5,M)=TQ(4,N)                                                   RZOC0278
      TQ(6,M)=TQ(4,N)                                                   RZOC0279
      M=M+1                                                             RZOC0280
      IF(M.LT.N) GOTO 8                                                 RZOC0281
      IF(K.EQ.NACH) PRIZN(NACH)=1                                       RZOC0282
      IF(K.EQ.NACH) GOTO 2                                              RZOC0283
      M=K-1                                                             RZOC0284
    9 ID1=TQ(5,M)-TQ(6,M)                                               RZOC0285
      ID2=TQ(4,M)-TQ(5,M)                                               RZOC0286
      TQ(6,M)=TQ(4,M+1)+1                                               RZOC0287
      TQ(5,M)=TQ(6,M)+ID1                                               RZOC0288
      TQ(4,M)=TQ(5,M)+ID2                                               RZOC0289
      M=M-1                                                             RZOC0290
      IF(M.GE.NACH) GOTO 9                                              RZOC0291
   33 KRAN=0                                                            RZOC0292
      TR=0                                                              RZOC0293
      J=J+1                                                             RZOC0294
      GOTO 6                                                            RZOC0295
    2 CONTINUE                                                          RZOC0296
      DO 50 K=1,NA                                                      RZOC0297
   50 UR(K)=0                                                           RZOC0298
      N=1                                                               RZOC0299
      I=1                                                               RZOC0300
   51 NU(I)=A(10,N)                                                     RZOC0301
      KU(I)=NU(I)+A(11,N)-1                                             RZOC0302
      M1=NU(I)                                                          RZOC0303
      M2=KU(I)                                                          RZOC0304
      DO 52 J=M1,M2                                                     RZOC0305
      IF(I.GT.UR(S(1,J))) UR(S(1,J))=I                                  RZOC0306
   52 CONTINUE                                                          RZOC0307
   53 N=S(1,NU(I))                                                      RZOC0308
      I=I+1                                                             RZOC0309
      IF(A(11,N).GT.0) GOTO 51                                          RZOC0310
   54 I=I-1                                                             RZOC0311
      IF(I.LE.0) GOTO 60                                                RZOC0312
      NU(I)=NU(I)+1                                                     RZOC0313
      IF(NU(I).GT.KU(I)) GOTO 54                                        RZOC0314
      GOTO 53                                                           RZOC0315
   60 DO 61 I=1,NT                                                      RZOC0316
   61 PRIZN(I)=0                                                        RZOC0317
      MUR=0                                                             RZOC0318
      DO 62 K=1,NA                                                      RZOC0319
      IF(UR(K).GT.MUR) MUR=UR(K)                                        RZOC0320
   62 CONTINUE                                                          RZOC0321
      MU=0                                                              RZOC0322
   68 MU=MU+1                                                           RZOC0323
      IF(MU.GE.MUR) GOTO 70                                             RZOC0324
      DO 63 I=1,NA                                                      RZOC0325
      IF(UR(I).NE.MU) GOTO 63                                           RZOC0326
      IF(A(11,I).LE.0) GOTO 63                                          RZOC0327
      MX=TQ(4,A(12,I))                                                  RZOC0328
      ND=A(10,I)                                                        RZOC0329
      KD=ND+A(11,I)-1                                                   RZOC0330
      MK=A(9,I)                                                         RZOC0331
   64 IF(MK.EQ.A(9,S(1,ND))) GOTO 65                                    RZOC0332
      NMK=A(12,S(1,ND))                                                 RZOC0333
      KMK=NMK+A(13,S(1,ND))-1                                           RZOC0334
      IF(PRIZN(A(12,S(1,ND))).EQ.0) GOTO 66                             RZOC0335
      IF(TQ(6,KMK).GE.MX) GOTO 65                                       RZOC0336
   66 ID1=TQ(5,KMK)-TQ(6,KMK)                                           RZOC0337
      ID2=TQ(4,KMK)-TQ(5,KMK)                                           RZOC0338
      TQ(6,KMK)=MX                                                      RZOC0339
   67 TQ(5,KMK)=TQ(6,KMK)+ID1                                           RZOC0340
      ID3=TQ(4,KMK)                                                     RZOC0341
      TQ(4,KMK)=TQ(5,KMK)+ID2                                           RZOC0342
      IF(KMK.LE.NMK) PRIZN(NMK)=1                                       RZOC0343
      IF(KMK.LE.NMK) GOTO 65                                            RZOC0344
      KMK=KMK-1                                                         RZOC0345
      ID1=TQ(5,KMK)-TQ(6,KMK)                                           RZOC0346
      ID2=TQ(4,KMK)-TQ(5,KMK)                                           RZOC0347
      ID3=TQ(6,KMK)-ID3                                                 RZOC0348
      TQ(6,KMK)=TQ(4,KMK+1)+ID3                                         RZOC0349
      GOTO 67                                                           RZOC0350
   65 ND=ND+1                                                           RZOC0351
      IF(ND.LE.KD) GOTO 64                                              RZOC0352
   63 CONTINUE                                                          RZOC0353
   70 RETURN                                                            RZOC0354
   80 RETURN                                                            RZOC0355
      END                                                               RZOC0356
      SUBROUTINE KDZ(A,S,P,TQ,TQC,U,NXZ,QZ,UW,IW,NW,NU,KI,LI,NT,IQ)     RZOC0357
      INTEGER*2 A(26,200),S(2,200),P(2,200),TQ(6,500),TQC(4,500),U(20,10RZOC0358
     *0),NXZ(10,4)                                                      RZOC0359
      INTEGER*2 MET/'CÏ'/                                               RZOC0360
      INTEGER QZ(4,500),UW(2,200),IW(100)                               RZOC0361
      INTEGER*2 DET(50),IMOP(7)                                         RZOC0362
    1 FORMAT(4A2,5A2,7A2,I5,2I3,I5,I2,10I3,I5,I9,10A2)                  RZOC0364
      N=A(10,1)                                                         RZOC0365
      N=S(1,N)                                                          RZOC0366
      N=A(12,N)+A(13,N)-1                                               RZOC0367
      N=TQ(1,N)                                                         RZOC0368
      N=U(1,N)                                                          RZOC0369
      KI=KI+1                                                           RZOC0370
      NK1=98                                                            RZOC0371
      KO=1                                                              RZOC0372
      DO 4 K=2,NT                                                       RZOC0373
      N=TQ(3,K)                                                         RZOC0374
      M=N/1000                                                          RZOC0375
      N=N-N/1000*1000                                                   RZOC0376
      N1=TQ(1,K)                                                        RZOC0377
      DO I=1,8
      IMOP(I)=U(I+12,N1)
      ENDDO
      DO 5 J=1,50                                                       RZOC0379
      DET(J)=0                                                          RZOC0380
    5 CONTINUE                                                          RZOC0381
      I=1                                                               RZOC0382
      M2=N+M                                                            RZOC0383
      DO 6 J=N,M2                                                       RZOC0384
      M1=A(1,J)                                                         RZOC0385
      NK=A(9,M1)                                                        RZOC0386
      DET(I)=A(2,M1)                                                    RZOC0387
      I=I+1                                                             RZOC0388
    6 CONTINUE                                                          RZOC0389
      IF(NK.EQ.NK1) KO=KO+1                                             RZOC0390
      IF(NK.EQ.NK1) GOTO 100                                            RZOC0391
      KO=1                                                              RZOC0392
  100 NZ=A(10,1)                                                        RZOC0393
      NZ=S(1,NZ)                                                        RZOC0394
      NZ=A(12,NZ)+A(13,NZ)-1                                            RZOC0395
      NZ=TQ(1,NZ)                                                       RZOC0396
      NZ=U(1,NZ)                                                        RZOC0397
      WRITE(8,1) (NXZ(J,1),J=1,4),(NXZ(J,4),J=1,5),(IMOP(j),j=1,7),
     1IQ,NK,KO,TQC(2,K),NZ,(DET(J),J=1,10),QZ(1,K),QZ(2,K),
     2(NXZ(J,3),J=1,10)          
      NK1=NK                                                            RZOC0401
    4 CONTINUE                                                          RZOC0402
      RETURN                                                            RZOC0403
      END                                                               RZOC0404
      SUBROUTINE ZAGOP(A,S,P,TQ,TQC,U,NXZ,IW,QZ,UW,K10,NA,NS,NT,LZ,ITRZ,RZOC0405
     1IRCZ)                                                             RZOC0406
      INTEGER*2 A(26,200),S(2,200),P(2,200),TQ(6,500),TQC(4,500),U(20,10RZOC0407
     *0),NXZ(10,4),KDP(180),DET,OP                                      RZOC0408
      INTEGER*2 S1(2,200)                                               RZOC0409
      INTEGER IW(100),QZ(4,500),UW(2,200)                               RZOC0410
      INTEGER*2 C/'  '/,H/'00'/                                         RZOC0411
      DO 12 I=1,200                                                     RZOC0412
      DO 13 J=1,2                                                       RZOC0413
      S1(J,I)=S(J,I)                                                    RZOC0414
   13 CONTINUE                                                          RZOC0415
   12 CONTINUE                                                          RZOC0416
      IF(NXZ(5,1).NE.C) GOTO 6                                          RZOC0417
      N=A(10,1)                                                         RZOC0418
      N=S(1,N)                                                          RZOC0419
      N=A(12,N)+A(13,N)-1                                               RZOC0420
      N=TQ(1,N)                                                         RZOC0421
      NZ=U(1,N)                                                         RZOC0422
      IF(NZ.EQ.10) GOTO 6                                               RZOC0423
      DET=A(9,1)                                                        RZOC0424
      DO 3 I=1,NA                                                       RZOC0425
      IF(A(9,I).EQ.1) NMK1=A(12,I)                                      RZOC0426
      IF(A(9,I).EQ.1) KMK1=A(13,I)                                      RZOC0427
      IF(DET.GT.A(9,I)) GOTO 3                                          RZOC0428
      DET=A(9,I)                                                        RZOC0429
    3 CONTINUE                                                          RZOC0430
      OP=TQ(4,1)                                                        RZOC0431
      NZIK=NMK1+KMK1                                                    RZOC0432
      DO 4 I=NZIK,NT                                                    RZOC0433
      IF(OP.GT.TQ(4,I)) GOTO 4                                          RZOC0434
      OP=TQ(4,I)                                                        RZOC0435
    4 CONTINUE                                                          RZOC0436
      N=NA+1                                                            RZOC0437
      M=NT+1                                                            RZOC0438
      DO 5 I=1,16                                                       RZOC0439
      A(I,N)=0                                                          RZOC0440
    5 CONTINUE                                                          RZOC0441
      DO 7 I=17,26                                                      RZOC0442
      A(I,N)=H                                                          RZOC0443
    7 CONTINUE                                                          RZOC0444
      A(1,N)=N                                                          RZOC0445
      A(10,N)=0                                                         RZOC0446
      A(12,N)=M                                                         RZOC0447
      A(2,N)=1                                                          RZOC0448
      A(9,N)=99                                                         RZOC0449
      A(11,N)=0                                                         RZOC0450
      A(13,N)=1                                                         RZOC0451
      A(14,N)=NS+1                                                      RZOC0452
      K=A(10,S(1,A(10,1)))                                              RZOC0453
      JPRZ=S(1,A(10,1))                                                 RZOC0454
      K=K+A(11,S(1,A(10,1)))-1                                          RZOC0455
      K1=K+1                                                            RZOC0456
      K2=N-2                                                            RZOC0457
      DO 10 I=K1,K2                                                     RZOC0458
      S(1,I+1)=S1(1,I)                                                  RZOC0459
      S(2,I+1)=S1(2,I)                                                  RZOC0460
   10 CONTINUE                                                          RZOC0461
      S(1,K1)=N                                                         RZOC0462
      S(2,K1)=1                                                         RZOC0463
      K1=A(10,1)+1                                                      RZOC0464
      K1=S(1,K1)                                                        RZOC0465
      DO 11 I=K1,N                                                      RZOC0466
      IF(A(11,I).GT.0) A(11,I)=A(11,I)+1                                RZOC0467
   11 CONTINUE                                                          RZOC0468
      A(10,1)=NS+1                                                      RZOC0469
      A(15,N)=1                                                         RZOC0470
      OP=OP+30                                                          RZOC0471
      LZ=OP                                                             RZOC0472
      TQ(1,M)=K10                                                       RZOC0473
      TQ(3,M)=N                                                         RZOC0474
      TQ(4,M)=OP+1                                                      RZOC0475
      TQ(5,M)=OP+1                                                      RZOC0476
      TQ(6,M)=OP                                                        RZOC0477
      QZ(1,M)=ITRZ                                                      RZOC0478
      QZ(2,M)=IRCZ                                                      RZOC0479
      R=(QZ(1,M)+0.0)/U(2,TQ(1,M))                                      RZOC0480
      J4=INT(R)                                                         RZOC0481
      IF(R-J4.LE.0.0) J4=J4-1                                           RZOC0482
      TQ(4,M)=TQ(5,M)+J4                                                RZOC0483
      QZ(3,M)=0                                                         RZOC0484
      QZ(4,M)=NT                                                        RZOC0485
      TQ(2,M)=0501                                                      RZOC0486
      TQC(2,M)=1                                                        RZOC0487
      TQC(1,M)=-1000                                                    RZOC0488
      TQC(3,M)=1                                                        RZOC0489
      TQC(4,M)=0                                                        RZOC0490
      DET=S(1,NS)                                                       RZOC0491
      OP=S(2,NS)                                                        RZOC0492
      P(2,NS+1)=1                                                       RZOC0493
      P(1,NS+1)=JPRZ                                                    RZOC0494
      NS=NS+1                                                           RZOC0495
      NA=NA+1                                                           RZOC0496
      DO 20 I=1,NA                                                      RZOC0497
      IF(A(9,I).EQ.1) IPRK1=P(1,A(14,I))                                RZOC0498
      IF(A(9,I).EQ.1) NSTK1=I                                           RZOC0499
      IF(A(9,I).EQ.1) P(1,A(14,I))=NA                                   RZOC0500
  20  CONTINUE                                                          RZOC0501
      NC1=A(10,IPRK1)                                                   RZOC0502
      NC2=A(10,IPRK1)+A(11,IPRK1)-1                                     RZOC0503
      DO 30 I=NC1,NC2                                                   RZOC0504
      IF(S(1,I).EQ.NSTK1) GOTO 31                                       RZOC0505
  30  CONTINUE                                                          RZOC0506
  31  SK2=S(2,I)                                                        RZOC0507
      NSTSK1=I+1                                                        RZOC0508
      NS1=NS-1                                                          RZOC0509
      DO 32 I=NSTSK1,NS1                                                RZOC0510
      S(1,I-1)=S(1,I)                                                   RZOC0511
  32  S(2,I-1)=S(2,I)                                                   RZOC0512
      S(1,NS1)=NSTK1                                                    RZOC0513
      S(2,NS1)=SK2                                                      RZOC0514
      A(10,NA)=NS1                                                      RZOC0515
      A(11,NA)=1                                                        RZOC0516
      A(11,IPRK1)=A(11,IPRK1)-1                                         RZOC0517
      NA1=NA-1                                                          RZOC0518
      DO 33 I=2,NA1                                                     RZOC0519
      IF(A(10,I).GT.NC1) A(10,I)=A(10,I)-1                              RZOC0520
  33  CONTINUE                                                          RZOC0521
      NZIK1=NZIK-1                                                      RZOC0522
      NZIK2=NMK1+1                                                      RZOC0523
      DO 15 I=NZIK2,NZIK1                                               RZOC0524
      TQ(4,I)=TQ(4,I)+J4+LZ+2-TQ(6,NZIK1)                               RZOC0525
      TQ(5,I)=TQ(5,I)+J4+LZ+2-TQ(6,NZIK1)                               RZOC0526
  15  TQ(6,I)=TQ(6,I)+J4+LZ+2-TQ(6,NZIK1)                               RZOC0527
      TQ(4,NMK1)=TQ(4,NMK1+1)                                           RZOC0528
      TQ(5,NMK1)=TQ(4,NMK1+1)                                           RZOC0529
      TQ(6,NMK1)=TQ(4,NMK1+1)                                           RZOC0530
      NT=NT+1                                                           RZOC0531
      LZ=TQ(4,2)                                                        RZOC0532
    6 RETURN                                                            RZOC0533
      END                                                               RZOC0534
      SUBROUTINE EXTCON(NI,NA,NS,NT,NE,NU,A,S,P,TQ,QZ,TQC,U,IP,IW)      RZOC0535
      INTEGER IW(100)                                                   RZOC0536
      INTEGER*2 A(26,1),S(2,1),P(2,1),TQ(6,1),TQC(4,1),U(20,1)          RZOC0537
      INTEGER QZ(4,1)                                                   RZOC0538
      NQ=NS-NE+1                                                        RZOC0539
      IF(NQ.LE.0) GOTO 10                                               RZOC0540
      DO 1 I=1,NQ                                                       RZOC0541
      ID=0                                                              RZOC0542
      DO 2 J=NE,NS                                                      RZOC0543
      J1=S(1,J)                                                         RZOC0544
      J2=S(2,J)                                                         RZOC0545
      IF(TQ(6,J1).GT.TQ(4,J2)) GOTO 2                                   RZOC0546
      ID=1                                                              RZOC0547
      J2=TQ(4,J2)-TQ(6,J1)+1                                            RZOC0548
      CALL SGCORR(J1,J2,A,S,TQ)                                         RZOC0549
    2 CONTINUE                                                          RZOC0550
      IF(ID.LT.1) GOTO10                                                RZOC0551
      IF(IP.GT.0) CALL SGRLZE(NI,NA,NT,NU,A,S,P,TQ,QZ,TQC,U,IP,IW)      RZOC0552
    1 CONTINUE                                                          RZOC0553
      IF(ID.GT.0) NI=-NI                                                RZOC0554
   10 RETURN                                                            RZOC0555
      END                                                               RZOC0556
      SUBROUTINE MEHDET(NA,NT,A,S,P,TQ,TQC)                             RZOC0557
      INTEGER*2 A(26,1),S(2,1),P(2,1),TQ(6,1),TQC(4,1),W1(100)/100*0/,  RZOC0558
     1W2(100)/100*0/,W3(50)/50*0/,W4(50)/50*0/                          RZOC0559
      DATA NK/82/                                                       RZOC0560
      DO 11 I=1,NA                                                      RZOC0561
   11 A(16,I)=0                                                         RZOC0562
      IT=0                                                              RZOC0563
      DO 10 I=1,NT                                                      RZOC0564
      TQC(4,I)=0                                                        RZOC0565
      I1=TQ(2,I )-TQ(2,I )/100*100                                      RZOC0566
      IF(I1.LE.NK) GO TO 10                                             RZOC0567
      IF(TQ(4,I).GT.IT) IT=TQ(4,I)                                      RZOC0568
      TQC(4,I)=1                                                        RZOC0569
   10 CONTINUE                                                          RZOC0570
      IF(IT.LE.0) GO TO 24                                              RZOC0571
      IT=IT+1                                                           RZOC0572
      K=0                                                               RZOC0573
      DO 9 I=1,NA                                                       RZOC0574
      IF(A(11,I).GT.0) GO TO 9                                          RZOC0575
      CALL DERPOD(I,A,P,TQC,W1,W2,W3,TQ,K)                              RZOC0576
    9 CONTINUE                                                          RZOC0577
   15 CONTINUE                                                          RZOC0578
      IF(K.LE.0) GO TO 12                                               RZOC0579
      DO 14 I=1,K                                                       RZOC0580
   14 W4(I)=W3(I)                                                       RZOC0581
      K1=K                                                              RZOC0582
      K=0                                                               RZOC0583
      DO 13 I=1,K1                                                      RZOC0584
      J1=W4(I)                                                          RZOC0585
      CALL DERPOD(J1,A,P,TQC,W1,W2,W3,TQ,K)                             RZOC0586
   13 CONTINUE                                                          RZOC0587
      GO TO 15                                                          RZOC0588
   12 J0=1                                                              RZOC0589
      J=0                                                               RZOC0590
    2 J=J+1                                                             RZOC0591
      W1(J)=J0                                                          RZOC0592
      W2(J)=A(11,J0)                                                    RZOC0593
      I1=A(13,J0)                                                       RZOC0594
      IF(I1.LE.0) GO TO 3                                               RZOC0595
      IF(A(16,J0)-1) 5,3,6                                              RZOC0596
    6 I1=A(16,J0)-1                                                     RZOC0597
    5 I2=A(12,J0)+I1-1                                                  RZOC0598
      I3=W1(J-1)                                                        RZOC0599
      I3=A(12,I3)                                                       RZOC0600
      IF(TQ(6,I2).GT.TQ(4,I3)) GO TO 4                                  RZOC0601
      II=TQ(4,I3)-TQ(6,I2)+1                                            RZOC0602
      GO TO 8                                                           RZOC0603
    4 CONTINUE                                                          RZOC0604
      IF(TQ(6,I2).GE.IT) GO TO 3                                        RZOC0605
      II=IT-TQ(6,I2)                                                    RZOC0606
    8 DO 7 I=1,I1                                                       RZOC0607
      I2=A(12,J0)+I-1                                                   RZOC0608
      TQ(4,I2)=TQ(4,I2)+II                                              RZOC0609
      TQ(5,I2)=TQ(5,I2)+II                                              RZOC0610
    7 TQ(6,I2)=TQ(6,I2)+II                                              RZOC0611
      IF(A(16,J0).GT.0) GO TO 3                                         RZOC0612
      A(16,J0)=-1                                                       RZOC0613
    3 W2(J)=W2(J)-1                                                     RZOC0614
      IF(W2(J).LT.0) GO TO 1                                            RZOC0615
      J0=A(10,J0)+W2(J)                                                 RZOC0616
      J0=S(1,J0)                                                        RZOC0617
      GO TO 2                                                           RZOC0618
    1 J=J-1                                                             RZOC0619
      IF(J.LE.0) GO TO 24                                               RZOC0620
      J0=W1(J)                                                          RZOC0621
      GO TO 3                                                           RZOC0622
   24 DO 17 I=1,NA                                                      RZOC0623
   17 A(16,I)=-1                                                        RZOC0624
      RETURN                                                            RZOC0625
      END                                                               RZOC0626
      SUBROUTINE DERPOD(I,A,P,TQC,W1,W2,W3,TQ,K)                        RZOC0627
      INTEGER*2 A(26,1),P(2,1),TQC(4,1),W1(1),W2(1),W3(1),TQ(6,1)       RZOC0628
      JJ=0                                                              RZOC0629
      J=0                                                               RZOC0630
      J0=I                                                              RZOC0631
   12 J=J+1                                                             RZOC0632
      IF(J.GT.100) STOP 100                                             RZOC0633
      W1(J)=J0                                                          RZOC0634
      W2(J)=A(15,J0)                                                    RZOC0635
      IF(JJ.LE.0) GO TO 13                                              RZOC0636
    2 A(16,J0)=1                                                        RZOC0637
      L=A(12,J0)                                                        RZOC0638
      TQC(4,L)=1                                                        RZOC0639
      IF(TQ(3,L).LT.1000) GO TO 15                                      RZOC0640
      I1=TQ(3,L)/1000                                                   RZOC0641
      I2=TQ(3,L)-I1*1000                                                RZOC0642
      I1=I1+1                                                           RZOC0643
      DO 1 L=1,I1                                                       RZOC0644
      J1=I2+L-1                                                         RZOC0645
      J1=A(1,J1)                                                        RZOC0646
      IF(A(16,J1).GT.0) GO TO 1                                         RZOC0647
      K=K+1                                                             RZOC0648
      W3(K)=J1                                                          RZOC0649
    1 A(16,J1)=1                                                        RZOC0650
      GO TO 15                                                          RZOC0651
   13 I1=A(13,J0)                                                       RZOC0652
      IF(I1.LE.0) GO TO 15                                              RZOC0653
      DO 14 L=1,I1                                                      RZOC0654
      I2=A(12,J0)+L-1                                                   RZOC0655
      IF(TQC(4,I2).LE.0) GO TO 14                                       RZOC0656
      A(16,J0)=L                                                        RZOC0657
      JJ=J                                                              RZOC0658
      GO TO 15                                                          RZOC0659
   14 CONTINUE                                                          RZOC0660
   15 W2(J)=W2(J)-1                                                     RZOC0661
      IF(W2(J).LT.0) GO TO 16                                           RZOC0662
      J0=W1(J)                                                          RZOC0663
      J0=A(14,J0)+W2(J)                                                 RZOC0664
      J0=P(1,J0)                                                        RZOC0665
      IF(A(16,J0).LE.0) GO TO 12                                        RZOC0666
      IF(A(16,J0).GT.1.AND.JJ.GT.0) GO TO 2                             RZOC0667
      GO TO 15                                                          RZOC0668
   16 J=J-1                                                             RZOC0669
      IF(J.LE.0) RETURN                                                 RZOC0670
      IF(JJ.GT.J) JJ=0                                                  RZOC0671
      GO TO 15                                                          RZOC0672
      END                                                               RZOC0673
      SUBROUTINE STRSSV(NF,NA,NS,A,S,LIMA,LIMS,NK,NKMAX)                RZOC0674
      INTEGER*2 A(26,1),S(2,1),H(10),Q(9)                               RZOC0675
      CHARACTER*1 F2/'2'/,JFM
    1 FORMAT(A1,40X,10A2,I3,I2,2I3,3I5,I3,I2)                           RZOC0676
      NA=1                                                              RZOC0677
      NS=0                                                              RZOC0678
      J0=1                                                              RZOC0679
   28 READ(NF,1,ERR=15)  JFM,H,Q                                        RZOC0680
      IF(JFM.NE.F2) GOTO 15                                             RZOC0681
C     ***** ARRANGEMENT:                                                RZOC0682
      IF(NA.LE.1) GOTO 16                                               RZOC0683
      DO 17 J=2,NA                                                      RZOC0684
      DO 18 J1=1,10                                                     RZOC0685
      IF(A(J1+16,J).NE.H(J1)) GOTO 19                                   RZOC0686
   18 CONTINUE                                                          RZOC0687
      GOTO 20                                                           RZOC0688
   19 IF(Q(8).LE.0) GOTO 17                                             RZOC0689
      IF(A(2,J).EQ.Q(8)) GOTO 21                                        RZOC0690
   17 CONTINUE                                                          RZOC0691
   16 NA=NA+1                                                           RZOC0692
      IF(NA.GT.LIMA) STOP 11                                            RZOC0693
      J=NA                                                              RZOC0694
      DO 22 J1=1,10                                                     RZOC0695
   22 A(J1+16,J)=H(J1)                                                  RZOC0696
      A(2,J)=Q(8)                                                       RZOC0697
      DO 23 J1=2,7                                                      RZOC0698
   23 A(J1+1,J)=Q(J1)                                                   RZOC0699
      A( 9,J)=Q(9)                                                      RZOC0700
      A(11,J)=0                                                         RZOC0701
      A(13,J)=0                                                         RZOC0702
      A(16,J)=NK                                                        RZOC0703
      GOTO 24                                                           RZOC0704
   20 IF(A(2,J).EQ.Q(8).OR.A(2,J)*Q(8).LE.0) GOTO 32                    RZOC0705
   21 PRINT 100,A(16,J),A(2,J),(A(J1,J),J1=17,26),NK,Q(8),H             RZOC0706
   32 IF(A(2,J).LE.0) A(2,J)=Q(8)                                       RZOC0707
      IF(A(3,J)+A(4,J)+A(5,J).GT.0) GOTO 25                             RZOC0708
      DO 26 J1=2,7                                                      RZOC0709
   26 A(J1+1,J)=Q(J1)                                                   RZOC0710
   25 IF(A(9,J).LE.0) A(9,J)=Q(9)                                       RZOC0711
   24 IF(Q(1).GT.0) GOTO 27                                             RZOC0712
      J0=J                                                              RZOC0713
      IF(A(11,J).GT.0) J0=1                                             RZOC0714
      A(10,J0)=NS+1                                                     RZOC0715
      A(11,J0)=0                                                        RZOC0716
      GOTO 29                                                           RZOC0717
   27 A(11,J0)=A(11,J0)+1                                               RZOC0718
      NS=NS+1                                                           RZOC0719
      IF(NS.GT.LIMS) STOP 12                                            RZOC0720
      S(1,NS)=J                                                         RZOC0721
      S(2,NS)=Q(1)                                                      RZOC0722
   29 NK=NK+1                                                           RZOC0723
      GOTO 28                                                           RZOC0724
   15 RETURN                                                            RZOC0725
  100 FORMAT(/5X,61HHECOOTBETCTBÈE:  ÁËOK NO  HOMEP ÄET.     :---OÁOÇHA×RZOC0726
     1EHÈE----:,2(/25X,I4,7X,I5,5X,10A2))                               RZOC0727
      END                                                               RZOC0728
      SUBROUTINE CHKPRT(LIMT,NA,NT,A,S,P,TQ,QZ,TQC,UG,NUG,IP)           RZOC0729
      INTEGER*2 A(26,1),S(2,1),TQ(6,1),TQC(4,1)                         RZOC0730
      INTEGER*2 P(2,1),UG(20,1)                                         RZOC0731
      INTEGER QZ(4,1),Q(40)                                             RZOC0732
      NA1=NA+1                                                          RZOC0733
      DO 1 J=2,NA                                                       RZOC0734
      IF(A(13,J).GT.0) GOTO 1                                           RZOC0735
      NT=NT+1                                                           RZOC0736
      NA1=NA1-1                                                         RZOC0737
      A(1,NA1)=J                                                        RZOC0738
      A(12,J)=NT                                                        RZOC0739
      A(13,J)=1                                                         RZOC0740
      IF(NT.GT.LIMT) STOP 41                                            RZOC0741
      IQ=0                                                              RZOC0742
      J1=A(14,J)                                                        RZOC0743
      J2=A(15,J)+J1-1                                                   RZOC0744
      DO 2 J3=J1,J2                                                     RZOC0745
    2 IQ=IQ+P(2,J3)                                                     RZOC0746
      TQ(1,NT)=NUG                                                      RZOC0747
      TQ(2,NT)=0                                                        RZOC0748
      TQ(3,NT)=NA1                                                      RZOC0749
      TQ(4,NT)=-1                                                       RZOC0750
      TQ(5,NT)=-1                                                       RZOC0751
      TQ(6,NT)=-1                                                       RZOC0752
      QZ(1,NT)=1                                                        RZOC0753
      QZ(2,NT)=0                                                        RZOC0754
      TQC(1,NT)=-1000                                                   RZOC0755
      TQC(2,NT)=IQ                                                      RZOC0756
      TQC(3,NT)=0                                                       RZOC0757
      TQC(4,NT)=-1                                                      RZOC0758
    1 CONTINUE                                                          RZOC0759
      J1=0                                                              RZOC0760
      J2=0                                                              RZOC0761
      J3=0                                                              RZOC0762
      DO 3 J=2,NT                                                       RZOC0763
      IF(QZ(1,J).LE.0) QZ(1,J)=1                                        RZOC0764
      J1=J1+QZ(1,J)                                                     RZOC0765
      IF(TQ(1,J).LT.NUG) GOTO 3                                         RZOC0766
      J2=J2+QZ(1,J)                                                     RZOC0767
      J3=J3+1                                                           RZOC0768
    3 CONTINUE                                                          RZOC0769
      RERL=IP/100.0                                                     RZOC0770
      IP=1                                                              RZOC0771
      IF((J3+0.0)/NT.GT.RERL.OR.(J2+0.0)/J1.GT.RERL) IP=-1              RZOC0772
      IF(IP.GE.0) GOTO 4                                                RZOC0773
C     ***** PRINT FOR CHECK:                                            RZOC0774
      PRINT 37                                                          RZOC0775
      PRINT 100                                                         RZOC0776
      DO 29 J=1,NA                                                      RZOC0777
      J1=A(10,J)                                                        RZOC0778
      J2=A(11,J)+J1-1                                                   RZOC0779
      IF(J2.LT.J1) GOTO 29                                              RZOC0780
      PRINT 105                                                         RZOC0781
      PRINT 101,(A(I,J),I=17,26),A(2,J),A(9,J)                          RZOC0782
      J5=0                                                              RZOC0783
      DO 30 J3=J1,J2                                                    RZOC0784
      J5=J5+1                                                           RZOC0785
      J4=S(1,J3)                                                        RZOC0786
      D1=A(6,J4)/10.                                                    RZOC0787
      D2=A(7,J4)/10.                                                    RZOC0788
      D3=A(8,J4)/10.                                                    RZOC0789
      L=(A(3,J4)*1000+A(4,J4))*1000+A(5,J4)                             RZOC0790
       PRINT 102,J5,(A(I,J4),I=17,26),S(2,J3),L,D1,D2,D3,               RZOC0791
     1A(2,J4),A(9,J4)                                                   RZOC0792
   30 CONTINUE                                                          RZOC0793
   29 CONTINUE                                                          RZOC0794
      PRINT 105                                                         RZOC0795
C     *****PRINT FOR CHECK:                                             RZOC0796
   10 I1=0                                                              RZOC0797
      PRINT 38                                                          RZOC0798
      PRINT 107                                                         RZOC0799
      DO 32 J=1,NT                                                      RZOC0800
      IF(TQ(3,J).EQ.I1) GOTO 33                                         RZOC0801
      PRINT 108                                                         RZOC0802
      JJ=0                                                              RZOC0803
      I1=TQ(3,J)                                                        RZOC0804
      NK=I1/1000                                                        RZOC0805
      NN=I1-NK*1000                                                     RZOC0806
      NK=NK+NN                                                          RZOC0807
      J1=0                                                              RZOC0808
      DO 35 J2=NN,NK                                                    RZOC0809
      J1=J1+1                                                           RZOC0810
      J3=A(1,J2)                                                        RZOC0811
   35 Q(J1)=A(2,J3)                                                     RZOC0812
   36 PRINT 104,A(9,J3),(Q(J2),J2=1,J1)                                 RZOC0813
   33 J1=TQ(1,J)                                                        RZOC0814
      IO=UG(1,J1)*10000+TQ(2,J)                                         RZOC0815
      R=QZ(2,J)/100.0                                                   RZOC0816
      T=QZ(1,J)/100.0                                                   RZOC0817
      JJ=JJ+1                                                           RZOC0818
      PRINT 103,JJ,IO,TQC(2,J),T,R                                      RZOC0819
   32 CONTINUE                                                          RZOC0820
      PRINT 108                                                         RZOC0821
    4 RETURN                                                            RZOC0822
   37 FORMAT(//7X,'CÏEÖÈÔÈKAÖÈß ÇAKAÇA:')                               RZOC0823
   38 FORMAT(//7X,'MA4ØPÓTHÛE KAPTÛ ÇAKAÇA:')                           RZOC0824
  100 FORMAT(//5X,92(1H-)/5X,8H:      :,20X,8H:      :,15X,41H: PAÇMEPÛ RZOC0825
     1ÇAÃOTOBOK (MM)  :     :       :/5X,52H:HOMEP :    OÁOÇHA×EHÈE     RZOC0826
     2:KOË-BO:     ØÈÔP      :,25(1H-),15H:HOMEP: HOMEP :/5X,8H: Ï/Ï  :,RZOC0827
     320X,64H:      :   MATEPÈAËA   : ÄËÈHA : ØÈPÈHA  :TOËÙÈHA: ÄET.:M/KRZOC0828
     4APTÛ:)                                                            RZOC0829
  101 FORMAT(5X,8H:CÁOPKA:,10A2,8H:      :,15X,27H:       :         :   RZOC0830
     1    !,I5,3H!  ,I5,1H!)                                            RZOC0831
  102 FORMAT(5X,2H! ,I4,2H !,10A2,2H! ,I5,1H!,I15,1H!,F7.1,1H!,F9.1,1H!,RZOC0832
     1F7.1,1H!,I5,3H!  ,I5,1H!)                                         RZOC0833
  103 FORMAT(5X,1H!,55X,2H! ,I3,3H ! ,I8,4H   !,I5,12H !      !   ,F6.2,RZOC0834
     14H   !,F6.2,3H  !)                                                RZOC0835
  104 FORMAT(5X,10H: M/KAPTA ,I4,10H ÄËß ÄET.:,5I6/(5X,1H:,23X,5I6))    RZOC0836
  105 FORMAT(5X,8H!------!,20(1H-),8H!------!,15(1H-),41H!-------!------RZOC0837
     1---!-------!-----!-------!)                                       RZOC0838
  107 FORMAT(//61X,56(1H-)/61X,56H:HOMEP:    ØÈÔP    :KOË-BO:PAÇPßÄ:TPÓÄRZOC0839
     1OEMKOCTÜ:PACÖEHKA:/61X,56H: Ï/Ï :OÁOPÓÄOBAHÈß:      :PAÁOTÛ:   (H/RZOC0840
     2×.)   : (PÓÁ.) :)                                                 RZOC0841
  108 FORMAT(5X,1H!,55(1H-),56H!-----!------------!------!------!-------RZOC0842
     1-----!--------!)                                                  RZOC0843
      END                                                               RZOC0844
      SUBROUTINE SVODCA(NT,TQ,QZ,NU,NW,IW,UW,IRAS,ITRZ,IRCZ)            RZOC0845
      INTEGER*2 TQ(6,1)                                                 RZOC0846
      INTEGER QZ(4,1)  ,IW(1)                                           RZOC0847
      INTEGER UW(2,1)                                                   RZOC0848
      IRAS=0                                                            RZOC0849
      DO 1 J=1,NU                                                       RZOC0850
    1 IW(J)=0                                                           RZOC0851
      DO 2 I=1,NT                                                       RZOC0852
      J=TQ(1,I)                                                         RZOC0853
      IRAS=IRAS+QZ(2,I)                                                 RZOC0854
    2 IW(J)=IW(J)+QZ(1,I)                                               RZOC0855
      DO 3 I=1,NW                                                       RZOC0856
      IF(UW(1,I).EQ.170501) IW(UW(2,I))=IW(UW(2,I))+ITRZ                RZOC0857
    3 CONTINUE                                                          RZOC0858
c      IRAS=(IRAS+IRCZ+50)/100                                           RZOC0859
      iras=iras+ircz
      RETURN                                                            RZOC0860
      END                                                               RZOC0861
      SUBROUTINE TCLZKZ(NF,LIMT,NU,NUG,NA,NT,U,UG,A,TQ,TQC,QZ,EC,NE,NK,NRZOC0862
     1KMAX,ITRZ,IRCZ)                                                   RZOC0863
      INTEGER*2 A(26,1),TQ(6,1),TQC(4,1)                                RZOC0864
      INTEGER*2 UG(20,1)                                                RZOC0865
      INTEGER*2 EC(3,1)                                                 RZOC0866
      INTEGER U(2,1)                                                    RZOC0867
      INTEGER QZ(4,1)                                                   RZOC0868
      CHARACTER*1 F1/'1'/,JFM
    1 FORMAT(A1,20X,I2,2I2,I2,I6,I5,I5,I9)                              RZOC0869
      NE=0                                                              RZOC0870
      A(1,1)=1                                                          RZOC0871
      TQ(1,1)=NUG                                                       RZOC0872
      TQ(2,1)=0                                                         RZOC0873
      TQ(3,1)=1                                                         RZOC0874
      TQ(4,1)=-1                                                        RZOC0875
      TQ(5,1)=-1                                                        RZOC0876
      TQ(6,1)=-1                                                        RZOC0877
      QZ(1,1)=1                                                         RZOC0878
      QZ(2,1)=0                                                         RZOC0879
      TQC(1,1)=-1000                                                    RZOC0880
      TQC(2,1)=1                                                        RZOC0881
      TQC(3,1)=0                                                        RZOC0882
      TQC(4,1)=-1                                                       RZOC0883
      NT=1                                                              RZOC0884
      NT1=1                                                             RZOC0885
      IB=0                                                              RZOC0886
      nk=nk+1
  22  READ(NF,1,END=10,ERR=99) JFM,MK,MK1,JT1,JTM,IOB,IQ,ITR,IRC         
      IF(JFM.NE.F1) GOTO 1000                                           RZOC0888
      IF(JTM.NE.1)  GOTO 12                                             RZOC0889
C     *****ARRANGEMENT:                                                 RZOC0890
      IF(MK.NE.1) GOTO 11                                               RZOC0891
      IF(IOB.EQ.170501) ITRZ=ITR                                        RZOC0892
      IF(IOB.EQ.170501) IRCZ=IRC                                        RZOC0893
      IF(IOB.EQ.170501) ITR=1                                           RZOC0894
      IF(IOB.EQ.170501) IRC=0                                           RZOC0895
   11 NT2=NT1+1                                                         RZOC0896
      DO 13 J=1,NA                                                      RZOC0897
      IF(A(13,J).GT.0.OR.A(9,J).NE.MK) GOTO 13                          RZOC0898
   14 NT1=NT1+1                                                         RZOC0899
      A(1,NT1)=J                                                        RZOC0900
      A(12,J)=NT+1                                                      RZOC0901
   13 CONTINUE                                                          RZOC0902
      IF(NT1.GE.NT2) GOTO 19                                            RZOC0903
   17 PRINT 101,NK,MK                                                   RZOC0904
   21 NK=NK+1                                                           RZOC0905
      IF(NK.GT.NKMAX) GOTO 10                                           RZOC0906
      READ(NF,1,ERR=10) JFM,MK,MK1,JT1,JTM,IOB,IQ,ITR,IRC               RZOC0907
      IF(JFM.NE.F1) GOTO 10                                             RZOC0908
      IF(JTM.EQ.1) GOTO 11                                              RZOC0909
      GOTO 21                                                           RZOC0910
   19 IB=(NT1-NT2)*1000+NT2                                             RZOC0911
   12 IF(IB.LE.0) GOTO 17                                               RZOC0912
   24 NT=NT+1                                                           RZOC0913
      IF(NT.GT.LIMT) STOP 21                                            RZOC0914
   28 DO 30 J1=NT2,NT1                                                  RZOC0915
      J2=A(1,J1)                                                        RZOC0916
   30 A(13,J2)=A(13,J2)+1                                               RZOC0917
   29 TQ(1,NT)=NUG                                                      RZOC0918
      TQ(2,NT)=IOB-IOB/10000*10000                                      RZOC0919
      TQ(3,NT)=IB                                                       RZOC0920
      TQ(4,NT)=-1                                                       RZOC0921
      TQ(5,NT)=-1                                                       RZOC0922
      TQ(6,NT)=-1                                                       RZOC0923
      TQC(1,NT)=-1000                                                   RZOC0924
      TQC(3,NT)=0                                                       RZOC0925
      TQC(4,NT)=-1                                                      RZOC0926
      IF(MK1*JT1.LE.0) GOTO32                                           RZOC0927
      NE=NE+1                                                           RZOC0928
      EC(1,NE)=NT                                                       RZOC0929
      EC(2,NE)=MK1                                                      RZOC0930
      EC(3,NE)=JT1                                                      RZOC0931
   32 CONTINUE                                                          RZOC0932
      DO 31 J1=1,NU                                                     RZOC0933
      IF(IOB.EQ.U(1,J1)) TQ(1,NT)=U(2,J1)                               RZOC0934
   31 CONTINUE                                                          RZOC0935
      TQC(2,NT)=IQ                                                      RZOC0936
      QZ(1,NT)=ITR                                                      RZOC0937
      QZ(2,NT)=IRC                                                      RZOC0938
  151 NK=NK+1                                                           RZOC0939
      GOTO 22                                                           RZOC0940
   99 WRITE(*,150)  MK,JTM
  150 FORMAT(1X,'îøèáêà ÷òåíèÿ',2I3)
      goto 151
 1000 NK=NK+1
   10 RETURN                                                            RZOC0941
  101 FORMAT(/5X,7HÁËOK NO,I4,8H M/KAPTA,I4,15H HE OÁPAÁOTAHA.)         RZOC0942
      END                                                               RZOC0943
      SUBROUTINE STRPRM(LIMS,NA,NS,NXZ,A,S,P,IS)                        RZOC0944
      INTEGER*2 A(26,1),S(2,1),P(2,1),W1(100),W2(100)                   RZOC0945
      INTEGER*2 NXZ(10),D1/'AA'/,NOMA,A17(10)                           RZOC0946
      INTEGER P4(200)                                                   RZOC0947
      LOGICAL*1 ZAK(2),OBOZ(20),PROB/' '/                               RZOC0948
      character*1 zakn(2),an/'À'/
      EQUIVALENCE (NOMA,ZAKn(1)),(A17,OBOZ)                              RZOC0949
      DO 14 J=2,26                                                      RZOC0950
      A(J,1)=0                                                          RZOC0951
      IF(J.GT.16) A(J,1)=NXZ(J-16)                                      RZOC0952
   14 CONTINUE                                                          RZOC0953
      A(10,1)=NS+1                                                      RZOC0954
      A(12,1)=1                                                         RZOC0955
      A(13,1)=1                                                         RZOC0956
      NP=0                                                              RZOC0957
      DO 11 JA=2,NA                                                     RZOC0958
      A(15,JA)=0                                                        RZOC0959
      IP=NP+1                                                           RZOC0960
      DO 12 J=2,NA                                                      RZOC0961
      J1=A(10,J)                                                        RZOC0962
      J2=A(11,J)+J1-1                                                   RZOC0963
      IF(J1.GT.J2) GOTO 12                                              RZOC0964
      DO 13 JS=J1,J2                                                    RZOC0965
      IF(S(1,JS).NE.JA) GOTO 13                                         RZOC0966
      A(14,JA)=IP                                                       RZOC0967
      A(15,JA)=A(15,JA)+1                                               RZOC0968
      NP=NP+1                                                           RZOC0969
      P(1,NP)=J                                                         RZOC0970
      P(2,NP)=S(2,JS)                                                   RZOC0971
   13 CONTINUE                                                          RZOC0972
   12 CONTINUE                                                          RZOC0973
      IF(A(15,JA).GT.0) GOTO 11                                         RZOC0974
      NS=NS+1                                                           RZOC0975
      IF(NS.GT.LIMS) STOP 31                                            RZOC0976
      S(1,NS)=JA                                                        RZOC0977
   15 S(2,NS)=IS                                                        RZOC0978
      A(11,1)=A(11,1)+1                                                 RZOC0979
      NP=NP+1                                                           RZOC0980
      A(14,JA)=NP                                                       RZOC0981
      A(15,JA)=1                                                        RZOC0982
      P(1,NP)=1                                                         RZOC0983
      P(2,NP)=IS                                                        RZOC0984
   11 CONTINUE                                                          RZOC0985
      DO 1 J1=1,NA                                                      RZOC0986
    1 A(16,J1)=A(15,J1)                                                 RZOC0987
      J0=1                                                              RZOC0988
      J=0                                                               RZOC0989
    2 J=J+1                                                             RZOC0990
      W1(J)=J0                                                          RZOC0991
      W2(J)=A(11,J0)                                                    RZOC0992
      J1=A(14,J0)                                                       RZOC0993
      J2=A(15,J0)+J1-1                                                  RZOC0994
      JM=-1                                                             RZOC0995
      IF(J2.LT.J1) GOTO 3                                               RZOC0996
      JM=0                                                              RZOC0997
      DO 4 J3=J1,J2                                                     RZOC0998
      J4=P(1,J3)                                                        RZOC0999
      P4(J3)=-P(2,J3)*A(16,J4)                                          RZOC1000
    4 JM=JM-P4(J3)                                                      RZOC1001
    3 A(16,J0)=JM                                                       RZOC1002
    5 W2(J)=W2(J)-1                                                     RZOC1003
      IF(W2(J).LT.0) GOTO 6                                             RZOC1004
      J0=A(10,J0)+W2(J)                                                 RZOC1005
      J0=S(1,J0)                                                        RZOC1006
      A(16,J0)=A(16,J0)-1                                               RZOC1007
      IF(A(16,J0).LE.0) GOTO 2                                          RZOC1008
    7 J0=W1(J)                                                          RZOC1009
      GOTO 5                                                            RZOC1010
    6 J=J-1                                                             RZOC1011
      IF(J.GT.0) GOTO 7                                                 RZOC1012
      DO 25 K=2,NA                                                      RZOC1013
      NOMA=A(17,K)                                                      RZOC1014
      ZAK(2)=ZAK(1)                                                     RZOC1015
      if(zakn(1).ne.an) goto 29
      P4(A(14,K))=P4(A(14,K))/IS                                        RZOC1017
   29 IF(P4(A(14,K)).GT.32000) P(2,A(14,K))=32000                       RZOC1025
      IF(P4(A(14,K)).LE.32000) P(2,A(14,K))=P4(A(14,K))                 RZOC1026
   25 CONTINUE                                                          RZOC1027
      RETURN                                                            RZOC1028
      END                                                               RZOC1029
      SUBROUTINE SGOKIZ(N,NT,A,S,P,TQ,QZ,TQC,U)                         RZOC1030
      INTEGER*2 A(26,1),S(2,1),P(2,1),TQ(6,1),TQC(4,1),W1(100),W2(100)  RZOC1031
      INTEGER*2 U(20,1)                                                 RZOC1032
      INTEGER QZ(4,1)                                                   RZOC1033
      IND=1                                                             RZOC1034
C     *****LATE STARTS:                                                 RZOC1035
      DO 1 J=1,N                                                        RZOC1036
    1 A(16,J)=A(11,J)                                                   RZOC1037
      J=0                                                               RZOC1038
      J0=1                                                              RZOC1039
      JT=1                                                              RZOC1040
    2 J=J+1                                                             RZOC1041
      NN=TQ(3,J0)                                                       RZOC1042
      NK=NN/1000                                                        RZOC1043
      W2(J)=NK+1                                                        RZOC1044
      NN=NN-NK*1000                                                     RZOC1045
      W1(J)=NN                                                          RZOC1046
      NK=NN+NK                                                          RZOC1047
      JN=0                                                              RZOC1048
      JM=-1                                                             RZOC1049
      IF(J.LE.1) GOTO 3                                                 RZOC1050
      DO 21 J7=1,2                                                      RZOC1051
      DO 4 J1=NN,NK                                                     RZOC1052
      J4=A(1,J1)                                                        RZOC1053
      J2=A(14,J4)                                                       RZOC1054
      J3=A(15,J4)+J2-1                                                  RZOC1055
      DO 5 J4=J2,J3                                                     RZOC1056
      J5=P(1,J4)                                                        RZOC1057
      J5=A(12,J5)                                                       RZOC1058
      IF(J5.EQ.J0) GOTO 5                                               RZOC1059
      J6=TQ(4,J5)                                                       RZOC1060
      IF(J6.LT.0) GOTO 6                                                RZOC1061
      IF(J7.GT.1) GOTO 42                                               RZOC1062
      QZ(4,J5)=1                                                        RZOC1063
      GOTO 5                                                            RZOC1064
   42 IF(QZ(4,J5).LT.0) GOTO 5                                          RZOC1065
      QZ(4,J5)=-1                                                       RZOC1066
      IF(J6.GT.JM) JM=J6                                                RZOC1067
      JN=JN+QZ(3,J5)                                                    RZOC1068
    5 CONTINUE                                                          RZOC1069
    4 CONTINUE                                                          RZOC1070
   21 CONTINUE                                                          RZOC1071
    3 J1=J0+JT                                                          RZOC1072
      DO 7 J2=1,JT                                                      RZOC1073
      J1=J1-1                                                           RZOC1074
      QZ(3,J1)=JN+J2                                                    RZOC1075
      JM=JM+1                                                           RZOC1076
      TQ(6,J1)=JM                                                       RZOC1077
      J3=TQ(1,J1)                                                       RZOC1078
      JM=JM+U(6,J3)                                                     RZOC1079
      TQ(5,J1)=JM                                                       RZOC1080
      R=(QZ(1,J1)+0.0)/U(2,J3)                                          RZOC1081
      J4=INT(R)                                                         RZOC1082
      IF(R-J4.LE.0.0) J4=J4-1                                           RZOC1083
      JM=JM+J4                                                          RZOC1084
    7 TQ(4,J1)=JM                                                       RZOC1085
    8 W2(J)=W2(J)-1                                                     RZOC1086
      IF(W2(J).LT.0) GOTO 6                                             RZOC1087
   11 J0=NN+W2(J)                                                       RZOC1088
      J1=A(1,J0)                                                        RZOC1089
    9 A(16,J1)=A(16,J1)-1                                               RZOC1090
      IF(A(16,J1).LT.0) GOTO 8                                          RZOC1091
      J0=A(10,J1)+A(16,J1)                                              RZOC1092
      J0=S(1,J0)                                                        RZOC1093
      JT=A(13,J0)                                                       RZOC1094
      J0=A(12,J0)                                                       RZOC1095
      IF(TQ(4,J0)) 2,9,9                                                RZOC1096
    6 J=J-1                                                             RZOC1097
      IF(J.LE.0) GOTO 10                                                RZOC1098
      NN=W1(J)                                                          RZOC1099
      GOTO 11                                                           RZOC1100
C     *****EMPTY BLOCKS CANCELLATION:                                   RZOC1101
   10 J=J+1                                                             RZOC1102
   31 IF(J.GT.N) GOTO 12                                                RZOC1103
      J2=A(13,J)                                                        RZOC1104
      J1=A(12,J)+J2-1                                                   RZOC1105
      IF(TQ(6,J1).GE.0) GOTO 10                                         RZOC1106
      IF(IND.GT.0) PRINT 100                                            RZOC1107
      IND=-1                                                            RZOC1108
      NN=TQ(3,J1)                                                       RZOC1109
      NK=NN/1000                                                        RZOC1110
      NN=NN-NK*1000                                                     RZOC1111
      NK=NN+NK                                                          RZOC1112
      J3=0                                                              RZOC1113
      DO 13 J4=NN,NK                                                    RZOC1114
      J3=J3+1                                                           RZOC1115
      J5=A(1,J4)                                                        RZOC1116
      W1(J3)=J5                                                         RZOC1117
   13 W2(J3)=A(2,J5)                                                    RZOC1118
      PRINT 101,A(9,J),(W2(J4),J4=1,J3)                                 RZOC1119
      DO 14 J4=1,N                                                      RZOC1120
      J5=0                                                              RZOC1121
      J6=A(1,J4)                                                        RZOC1122
      DO 15 J7=1,J3                                                     RZOC1123
      IF(J6-W1(J7)) 16,16,15                                            RZOC1124
   15 J5=J5+1                                                           RZOC1125
   16 J7=J4                                                             RZOC1126
      IF(J7.GT.NK) J7=J7-J3                                             RZOC1127
   14 A(1,J7)=J6-J5                                                     RZOC1128
      DO 17 J4=1,NT                                                     RZOC1129
      J5=J4                                                             RZOC1130
      IF(J4.LE.J1) GOTO 18                                              RZOC1131
      J5=J4-J2                                                          RZOC1132
      DO 19 J6=1,4                                                      RZOC1133
      TQ(J6,J5)=TQ(J6,J4)                                               RZOC1134
      QZ(J6,J5)=QZ(J6,J4)                                               RZOC1135
   19 TQC(J6,J5)=TQC(J6,J4)                                             RZOC1136
      TQ(5,J5)=TQ(5,J4)                                                 RZOC1137
      TQ(6,J5)=TQ(6,J4)                                                 RZOC1138
   18 J6=TQ(3,J5)                                                       RZOC1139
      J7=J6-J6/1000*1000                                                RZOC1140
      IF(J7.GT.NK) J6=J6-J3                                             RZOC1141
   17 TQ(3,J5)=J6                                                       RZOC1142
      J5=-1                                                             RZOC1143
      J6=0                                                              RZOC1144
      NK=J3+1                                                           RZOC1145
      DO 20 J7=1,NK                                                     RZOC1146
      J4=J5+2                                                           RZOC1147
      J5=N                                                              RZOC1148
      IF(J7.LT.NK) J5=W1(J7)-1                                          RZOC1149
      IF(J4.GT.J5) GOTO 20                                              RZOC1150
      DO 22 J8=J4,J5                                                    RZOC1151
      J6=J6+1                                                           RZOC1152
      IF(J6.GE.J8) GOTO 23                                              RZOC1153
      DO 24 J9=2,26                                                     RZOC1154
   24 A(J9,J6)=A(J9,J8)                                                 RZOC1155
   23 IF(A(12,J6).GT.J1) A(12,J6)=A(12,J6)-J2                           RZOC1156
      K1=A(10,J6)                                                       RZOC1157
      K2=A(11,J6)+K1-1                                                  RZOC1158
      IF(K1.GT.K2) GOTO 25                                              RZOC1159
      K3=K1-1                                                           RZOC1160
      DO 26 K4=K1,K2                                                    RZOC1161
      K5=0                                                              RZOC1162
      K6=S(1,K4)                                                        RZOC1163
      DO 27 K7=1,J3                                                     RZOC1164
      IF(W1(K7)-K6) 27,26,28                                            RZOC1165
   27 K5=K5+1                                                           RZOC1166
   28 K3=K3+1                                                           RZOC1167
      S(1,K3)=K6-K5                                                     RZOC1168
      S(2,K3)=S(2,K4)                                                   RZOC1169
   26 CONTINUE                                                          RZOC1170
      A(11,J6)=K3-K1+1                                                  RZOC1171
   25 K1=A(14,J6)                                                       RZOC1172
      K2=A(15,J6)+K1-1                                                  RZOC1173
      IF(K1.GT.K2) GOTO 22                                              RZOC1174
      DO 29 K4=K1,K2                                                    RZOC1175
      K5=0                                                              RZOC1176
      K6=P(1,K4)                                                        RZOC1177
      DO 30 K7=1,J3                                                     RZOC1178
      IF(K6-W1(K7)) 29,29,30                                            RZOC1179
   30 K5=K5+1                                                           RZOC1180
   29 P(1,K4)=K6-K5                                                     RZOC1181
   22 CONTINUE                                                          RZOC1182
   20 CONTINUE                                                          RZOC1183
      N=N-J3                                                            RZOC1184
      NT=NT-J2                                                          RZOC1185
      GOTO 31                                                           RZOC1186
C     *****PARAMETERS OF TAILS:                                         RZOC1187
   12 DO 32 J=1,N                                                       RZOC1188
   32 A(16,J)=A(11,J)                                                   RZOC1189
      J=0                                                               RZOC1190
      J0=1                                                              RZOC1191
   33 J=J+1                                                             RZOC1192
      TQC(4,J0)=-2                                                      RZOC1193
      NN=TQ(3,J0)                                                       RZOC1194
      NK=NN/1000                                                        RZOC1195
      W2(J)=NK+1                                                        RZOC1196
      NN=NN-NK*1000                                                     RZOC1197
      W1(J)=NN                                                          RZOC1198
   34 W2(J)=W2(J)-1                                                     RZOC1199
      IF(W2(J).LT.0) GOTO 35                                            RZOC1200
   36 J0=NN+W2(J)                                                       RZOC1201
      J1=A(1,J0)                                                        RZOC1202
   37 A(16,J1)=A(16,J1)-1                                               RZOC1203
      IF(A(16,J1).LT.0) GOTO 34                                         RZOC1204
      J2=A(10,J1)+A(16,J1)                                              RZOC1205
      J2=S(1,J2)                                                        RZOC1206
      J0=A(12,J2)                                                       RZOC1207
      IF(TQC(4,J0)+1) 37,33,33                                          RZOC1208
   35 J1=A(1,NN)                                                        RZOC1209
      J0=A(12,J1)                                                       RZOC1210
      JT=A(13,J1)+J0-1                                                  RZOC1211
      NK=NN+TQ(3,J0)/1000                                               RZOC1212
      JN=0                                                              RZOC1213
      JM=0                                                              RZOC1214
      DO 43 J7=1,2                                                      RZOC1215
      DO 38 J1=NN,NK                                                    RZOC1216
      J3=A(1,J1)                                                        RZOC1217
      J2=A(10,J3)                                                       RZOC1218
      J3=A(11,J3)+J2-1                                                  RZOC1219
      IF(J2.GT.J3) GOTO 38                                              RZOC1220
      DO 39 J4=J2,J3                                                    RZOC1221
      J5=S(1,J4)                                                        RZOC1222
      J6=A(12,J5)                                                       RZOC1223
      IF(J6.EQ.J0) GOTO 39                                              RZOC1224
      IF(J7.GT.1) GOTO 44                                               RZOC1225
      TQC(1,J6)=-2000                                                   RZOC1226
      GOTO 39                                                           RZOC1227
   44 IF(TQC(1,J6).GT.-1500) GOTO 39                                    RZOC1228
      TQC(1,J6)=-1000                                                   RZOC1229
      J6=J6+A(13,J5)-1                                                  RZOC1230
      IF(TQC(3,J6).GT.JM) JM=TQC(3,J6)                                  RZOC1231
      JN=JN+QZ(4,J6)                                                    RZOC1232
   39 CONTINUE                                                          RZOC1233
   38 CONTINUE                                                          RZOC1234
   43 CONTINUE                                                          RZOC1235
      DO 40 J1=J0,JT                                                    RZOC1236
      JM=JM+TQ(4,J1)-TQ(6,J1)+1                                         RZOC1237
      JN=JN+1                                                           RZOC1238
      TQC(3,J1)=JM                                                      RZOC1239
   40 QZ(4,J1)=JN                                                       RZOC1240
      J=J-1                                                             RZOC1241
      IF(J.LE.0) GOTO 41                                                RZOC1242
      NN=W1(J)                                                          RZOC1243
      GOTO 36                                                           RZOC1244
   41 RETURN                                                            RZOC1245
  100 FORMAT(/5X,31HB O×EPEÄHOCTÈ MAPØPÓTOB- ÖÈKËÛ;/5X,34HÈÇ'ßTÛ CËEÄÓÞÙRZOC1246
     1ÈE MAPØPÓTHÛE KAPTÛ:)                                             RZOC1247
  101 FORMAT(/5X,7HM/KAPTA,I3,18H, COÄEPÆÈT ÄETAËÈ:,10I5/(33X,10I5))    RZOC1248
      END                                                               RZOC1249
      SUBROUTINE SGRLZE(NI,NA,NT,NU,A,S,P,TQ,QZ,TQC,U,IP,IW)            RZOC1250
      INTEGER IW(100)                                                   RZOC1251
      INTEGER*2 A(26,1),S(2,1),P(2,1),TQ(6,1),TQC(4,1),T1(500),T2(500)  RZOC1252
      INTEGER*2 U(20,1)                                                 RZOC1253
      INTEGER QZ(4,1)                                                   RZOC1254
      DO 15 J=1,NA                                                      RZOC1255
   15 A(16,J)=A(11,J)                                                   RZOC1256
      NU1=NU-1                                                          RZOC1257
      NI=0                                                              RZOC1258
    1 NI=NI+1                                                           RZOC1259
      IPRK=0                                                            RZOC1260
      DO 2 JU=1,NU1                                                     RZOC1261
      IF(IP.EQ.1.AND.IW(JU).LT.(U(4,JU)*U(3,JU)+50)/100) GOTO 2         RZOC1262
      IF(IP.NE.1.AND.IW(JU).LT.U(2,JU)) GOTO 2                          RZOC1263
      IPRK=1                                                            RZOC1264
      IND=-1                                                            RZOC1265
      M=0                                                               RZOC1266
      DO 3 J=1,NT                                                       RZOC1267
      J1=TQ(4,J)                                                        RZOC1268
      IF(J1.LT.NI) GOTO 3                                               RZOC1269
      IND=1                                                             RZOC1270
      J2=TQ(5,J)                                                        RZOC1271
      IF(J2.GT.NI.OR.TQ(1,J).NE.JU) GOTO 3                              RZOC1272
      M=M+1                                                             RZOC1273
      IF(M.GT.500) STOP 51                                              RZOC1274
      T1(M)=J                                                           RZOC1275
      T2(M)=0                                                           RZOC1276
    3 CONTINUE                                                          RZOC1277
      IF(IND.LT.0) GOTO 4                                               RZOC1278
      IF(M.LE.0) GOTO 2                                                 RZOC1279
      LT=U(2,JU)                                                        RZOC1280
      LG=LT                                                             RZOC1281
      IF(IP.LE.3.AND.LT.LE.(U(3,JU)*U(4,JU)+50)/100) LG=(U(3,JU)*U(4,JU)RZOC1282
     1+50)/100                                                          RZOC1283
      DO 6 J1=1,M                                                       RZOC1284
      JQ=LT                                                             RZOC1285
      J=T1(J1)                                                          RZOC1286
      J2=TQ(5,J)                                                        RZOC1287
      IF(J2.GE.NI) JQ=QZ(1,J)-(TQ(4,J)-J2)*LT                           RZOC1288
    6 LG=LG-JQ                                                          RZOC1289
      IF(LG.GE.0) GOTO 2                                                RZOC1290
      DO 7 J0=1,M                                                       RZOC1291
      DO 12 J1=1,M                                                      RZOC1292
      IF(T2(J1)) 13,13,12                                               RZOC1293
   12 CONTINUE                                                          RZOC1294
   13 JM=J1                                                             RZOC1295
      I=T1(J1)                                                          RZOC1296
      DO 8 J2=J1,M                                                      RZOC1297
      IF(T2(J2).GT.0) GOTO 8                                            RZOC1298
      J=T1(J2)                                                          RZOC1299
      IF(TQC(3,I)-TQC(3,J)) 8,9,10                                      RZOC1300
    9 IF(QZ(4,I)-QZ(4,J)) 8,11,10                                       RZOC1301
   11 IF(QZ(1,I)-QZ(1,J)) 10,8,8                                        RZOC1302
   10 JM=J2                                                             RZOC1303
      I=J                                                               RZOC1304
    8 CONTINUE                                                          RZOC1305
      J2=TQ(5,I)                                                        RZOC1306
      T2(JM)=NI-J2+1                                                    RZOC1307
      JQ=LT                                                             RZOC1308
      IF(J2.GE.NI) JQ=QZ(1,I)-(TQ(4,I)-J2)*LT                           RZOC1309
      LG=LG+JQ                                                          RZOC1310
      IF(LG.GE.0) GOTO 14                                               RZOC1311
    7 CONTINUE                                                          RZOC1312
   14 DO 5 J=1,M                                                        RZOC1313
      J2=T2(J)                                                          RZOC1314
      IF(J2.LE.0) GOTO 5                                                RZOC1315
      J1=T1(J)                                                          RZOC1316
      CALL SGCORR(J1,J2,A,S,TQ)                                         RZOC1317
    5 CONTINUE                                                          RZOC1318
    2 CONTINUE                                                          RZOC1319
      IF(IPRK.EQ.0) GOTO 4                                              RZOC1320
      GOTO 1                                                            RZOC1321
    4 DO 16 J=1,NA                                                      RZOC1322
   16 A(16,J)=-1                                                        RZOC1323
      RETURN                                                            RZOC1324
      END                                                               RZOC1325
      SUBROUTINE SGCORR(JT,ND,A,S,TQ )                                  RZOC1326
      INTEGER*2 A(26,1),S(2,1),TQ(6,1),W1(100),W2(100)                  RZOC1327
      I=TQ(6,JT)+ND                                                     RZOC1328
      IPR=0                                                             RZOC1329
      J=0                                                               RZOC1330
    1 J=J+1                                                             RZOC1331
      NN=TQ(3,JT)                                                       RZOC1332
      NK=NN/1000                                                        RZOC1333
      W2(J)=NK+1                                                        RZOC1334
      NN=NN-NK*1000                                                     RZOC1335
      W1(J)=NN                                                          RZOC1336
      J1=A(1,NN)                                                        RZOC1337
      J0=A(12,J1)                                                       RZOC1338
      J2=JT+1                                                           RZOC1339
      DO 2 J1=J0,JT                                                     RZOC1340
      J2=J2-1                                                           RZOC1341
      J3=I-TQ(6,J2)                                                     RZOC1342
      IPR=0                                                             RZOC1343
      IF(J3.LE.0) GOTO 3                                                RZOC1344
      IF(TQ(6,J2-1).EQ.TQ(4,J2)) IPR=1                                  RZOC1345
      DO 4 J4=4,6                                                       RZOC1346
    4 TQ(J4,J2)=TQ(J4,J2)+J3                                            RZOC1347
      IF(IPR.EQ.1) GOTO 2                                               RZOC1348
      I=TQ(4,J2)+1                                                      RZOC1349
    2 CONTINUE                                                          RZOC1350
    5 W2(J)=W2(J)-1                                                     RZOC1351
      IF(W2(J).LT.0) GOTO 7                                             RZOC1352
   10 J1=NN+W2(J)                                                       RZOC1353
      J1=A(1,J1)                                                        RZOC1354
    6 A(16,J1)=A(16,J1)-1                                               RZOC1355
      IF(A(16,J1).LT.0) GOTO 5                                          RZOC1356
      J2=A(10,J1)+A(16,J1)                                              RZOC1357
      J2=S(1,J2)                                                        RZOC1358
      J3=A(12,J2)                                                       RZOC1359
      IF(J3.EQ.J0) GOTO 6                                               RZOC1360
      JT=A(13,J2)+J3-1                                                  RZOC1361
      GOTO 1                                                            RZOC1362
    7 NK=NN+NK                                                          RZOC1363
      DO 8 J1=NN,NK                                                     RZOC1364
      J2=A(1,J1)                                                        RZOC1365
    8 A(16,J2)=A(11,J2)                                                 RZOC1366
    3 J=J-1                                                             RZOC1367
      IF(J.LE.0) GOTO 9                                                 RZOC1368
      NN=W1(J)                                                          RZOC1369
      J1=A(1,NN)                                                        RZOC1370
      J0=A(12,J1)                                                       RZOC1371
      I=TQ(4,J0)+1                                                      RZOC1372
      NK=TQ(3,J0)/1000                                                  RZOC1373
      GOTO 10                                                           RZOC1374
    9 RETURN                                                            RZOC1375
      END                                                               RZOC1376
 
