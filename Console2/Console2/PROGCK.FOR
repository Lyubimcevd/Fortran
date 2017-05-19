      INTEGER ISM(9),ISZ(9),F(100,3),BOT(6),CON/-1/,ZG(100,60),U(20,100)OGCK0001
      INTEGER*2 ZG1(100,60),U1(20,100),KDP(180),NXZ(5),INS(5),KLS(10)   OGCK0002
      INTEGER*2 PRV,PRVG,CON1/-1/,DEK                                   OGCK0003
      integer*2 dummy2
      integer*4 dummy4,dummy
      open(8,file='F:\ASUIPW\tek_INF\prozex.dat',access='direct',
     1recl=174,form='formatted')
      open(9,file='F:\ASUIPW\tek_INF\zagr.dat',form='unformatted')
      NL2=13                                                            OGCK0005
      M1=0                                                              OGCK0006
      M2=0                                                              OGCK0007
      M3=0                                                              OGCK0008
      DATA N1/07/,N2/04/,LM1/0/                                         OGCK0009
   20 FORMAT(' ÈTOÃO ÏO MACTEPCKOÉ',4X,I4,'ÇAK',4X,I6,1X,I6,5(1X,I6),1X,OGCK0010
     1I6,T120,A5/)                                                      OGCK0011
   21 FORMAT(' ÈTOÃO ÏO ÖEXÓ      ',4X,I4,'ÇAK',4X,I6,1X,I6,5(1X,I6),1X,OGCK0012
     1I6,T120,A5)                                                       OGCK0013
   11 FORMAT(//5X,'ÏPOÃPAMMA ÖEXA',I2,2X,'HA ',I2,1H.,I2,1X,'19',I2,    OGCK0014
     1/80X,'BÛÏÓCKHOÉ ÃPAÔÈK',8X,'ËÈCT N ',I4)                          OGCK0015
   15 FORMAT(//5X,'ÏPOÃPAMMA ÖEXA',I2,2X,'HA ',I2,1H.,I2,1X,'19',I2,    OGCK0016
     1/80X,'ÇAÄEËÜHÛÉ ÃPAÔÈK',8X,'ËÈCT N ',I4)                          OGCK0017
   16 FORMAT(//5X,'ÏPOÃPAMMA ÖEXA',I2,2X,'HA ',I2,1H.,I2,1X,'19',I2,    OGCK0018
     1/80X,'ÇAÏÓCKHOÉ ÃPAÔÈK',8X,'ËÈCT N ',I4)                          OGCK0019
   12 FORMAT(//5X,'ÏOPTÔEËÜ ÖEXA',I2,' :')                              OGCK0020
    1 FORMAT(//1X,128(1H-)/1X,    8X,1H!,10X,1H!,20X,1H!,3H K ,1H!,2HM ,OGCK0021
     11H!,34X,1H!,2HÖ ,1H!,6X,1H!,2(8X,1H!),1HÏ,1H!,2HK!,6X,1H!         OGCK0022
     2/1X,    8X,1H!,10X,1H!,20X,1H!,3H O ,1H!,2HA ,1H!,11X,'TPÓÄOEMKOCTOGCK0023
     3Ü',11X,1H!,2HÈ ,1H!,6X,1H!,2(8X,1H!),1HP,1H!,2HA!,6X,1H!          OGCK0024
     4/1X,    8X,1H!,3X,3HBÈÄ,4X,1H!,20X,1H!,3H Ë ,1H!,2HC ,1H!,34X,1H!,OGCK0025
     52HK ,1H!,' ÄATA ',1H!,2('  ÄATA  ',1H!),1HÈ,1H!,2HT!,' ÄATA ','! ÄOGCK0026
     *ATA '                                                             OGCK0027
     6/1X,          'N ÇAKAÇA!',10X,1H!,3X,4HØÈÔP,3X,7H×EPTEÆA,3X,1H!,3HOGCK0028
     7 È ,1H!,2HT ,1H!,34(1H-),1H!,2HË ,1H!,6X,1H!,2(8X,1H!),1HO,1H!,2HEOGCK0029
     8!,6X,1H!                                                          OGCK0030
     9/1X,    8X,1H!,'  ÈHCTPÓ- ',1H!,20X,1H!,3H × ,1H!,2HE ,1H!,6H OÁÙAOGCK0031
     Aß,1H!,'  OCTATOK  ÏO  OÏEPAÖÈßM   ',1H!,2X,1H!,6HBÛÏÓC-,1H!,8HKOMÏOGCK0032
     BËEK-,1H!,8H ÇAÏÓC- ,1H!,1HP,1H!,2HÃ!,'ÏOTPEÁ',  '!HA×AËA'         OGCK0033
     C/1X,    8X,1H!,10X,1H!,6X,6HÄETAËÈ,8X,1H!,6H E !P ,1H!  ,6X,1H!,27OGCK0034
     DX,1H!,2HÈ ,1H!,6X,1H!,2(8X,1H!),1HÈ,1H!,2HO!,6X,1H!               OGCK0035
     E/1X,    8X,1H!,3X,5HMEHTA,2X,1H!,20X,1H!,3H C ,1H!,2HC ,1H!,6X,1H!OGCK0036
     F,27(1H-),1H!,2H3 ,1H!,2X,2HKA,2X,1H!,8H TAÖÈÈ  ,1H!,8H  KA    ,1H!OGCK0037
     G,1HT,1H!,2HP!,'HOCTÈ ',   '!ÏË-HÈß')                              OGCK0038
  101 FORMAT(1X,    8X,1H!,10X,1H!,20X,1H!,3H T ,1H!,2HK ,1H!,6X,1H!,27XOGCK0039
     1,1H!,2HÃ ,1H!,6X,1H!,2(8X,1H!),1HE,1H!,2HÈ!,6X,'!  B   '          OGCK0040
     2/1X,    8X,1H!,10X,1H!,20X,1H!,3H B ,1H!,2HA ,1H!,6X,'!CË. TOK ÔP.OGCK0041
     3 ØË. K-P ÇAÃ HEÇ!',2HO ,1H!,6X,1H!,2(8X,1H!),1HT,1H!,2Hß!,6X,'!TEKOGCK0042
     *ÓÙ.'                                                              OGCK0043
     4/1X,    8X,1H!,10X,1H!,20X,1H!,3H O ,1H!,2Hß ,1H!,6X,1H!,27X,1H!,2OGCK0044
     5HT.,1H!,6X,1H!,2(8X,1H!),1X,1H!,1X,1H!,6X,'!MECßÖE'/1X,128(1H-))  OGCK0045
 2000 FORMAT(122X,I5)                                                   OGCK0046
   13 FORMAT(10I8)                                                      OGCK0047
 1001 FORMAT(1H )                                                       OGCK0048
      KB10=0                                                            OGCK0049
      KD10=0                                                            OGCK0050
      KB15=0                                                            OGCK0051
      KD15=0                                                            OGCK0052
      READ(9) NN,NK,LP                                                  OGCK0053
      lp1=lp*2
      open(666,file = 'tmp.txt')
      read(666,777) nd
  777 format(i6)
      NMG=NN-NN/10000*10000                                             OGCK0057
      NDG=ND-ND/10000*10000                                             OGCK0058
      IF(NMG.NE.NDG) ND=NN/10000                                        OGCK0059
      IF(NMG.EQ.NDG) ND=ND/10000                                        OGCK0060
  600 CONTINUE                                                          OGCK0061
      NG=NN-NN/100*100                                                  OGCK0062
      NM=NN/100                                                         OGCK0063
      NM=NM-NM/100*100                                                  OGCK0064
      open(10,file='F:\ASUIPW\tek_INF\kolpr.dat')
      READ(10,13) KZ10,KZ15                                             OGCK0065
      READ(10,13) KBZ10,KPZ10                                           OGCK0066
      READ(10,13) KBU10,KPU10                                           OGCK0067
      READ(10,13) KBZ15,KPZ15                                           OGCK0068
      READ(10,13) KBU15,KPU15                                           OGCK0069
      read(666,7777) lstck
 7777 format(i5)
      LSTC=1                                                            OGCK0072
      KOL=0                                                             OGCK0073
      IPR=0                                                             OGCK0074
      open(2,file='prn')
      open(unit=n2,file='F:\ASUIPW\tek_INF\u.dat',form='unformatted')
      READ(N2) NW,NU                                                    OGCK0075
      READ(N2) ((K,I=1,2),J=1,NW),((U1(I,J),I=1,20),J=1,NU)             OGCK0076
      DO 5000 J=1,NU                                                    OGCK0077
      DO 5000 I=1,20                                                    OGCK0078
      U(I,J)=0                                                          OGCK0079
 5000 U(I,J)=U1(I,J)                                                    OGCK0080
      NUC=0                                                             OGCK0081
      DO 14 I=1,NU                                                      OGCK0082
      U(4,I)=(U(3,I)*U(4,I)+50)/100                                     OGCK0083
      IF(U(7,I).LE.0) GOTO 14                                           OGCK0084
      NUC=NUC+1                                                         OGCK0085
      U(7,I)=NUC                                                        OGCK0086
   14 CONTINUE                                                          OGCK0087
      NR=1                                                              OGCK0088
      NZ=10                                                             OGCK0089
      read(9) (KDP(i),i=1,lp1)
      READ(9,end=999) ((ZG1(I,J),I=1,NUC),J=1,LP),((F(I,J),I=1,NUC),J=1,
     13)
  999 DO 5001 J=1,LP                                                    OGCK0091
      DO 5001 I=1,NUC                                                   OGCK0092
      ZG(I,J)=0                                                         OGCK0093
 5001 ZG(I,J)=ZG1(I,J)*10                                               OGCK0094
      IF(LSTCK.GT.0) GOTO 961                                           OGCK0095
      IF(KZ10.EQ.0) GOTO 960                                            OGCK0096
      DO 96 I=1,KZ10                                                    OGCK0097
      CALL PTZG(LP,NU,KDP,U,ZG,NUC,NZ,NM,NG,NK,NN,ND,U1)                OGCK0098
      CALL PTPD(LP,NU,KDP,U,ZG,NUC,NZ,NM,NG,NK,ND)                      OGCK0099
   96 CONTINUE                                                          OGCK0100
  960 CONTINUE                                                          OGCK0101
      IF(KZ15.EQ.0) GOTO 961                                            OGCK0102
      DO 95 I=1,KZ15                                                    OGCK0103
      NZ=15                                                             OGCK0104
      CALL PTZG(LP,NU,KDP,U,ZG,NUC,NZ,NM,NG,NK,NN,ND,U1)                OGCK0105
      CALL PTPD(LP,NU,KDP,U,ZG,NUC,NZ,NM,NG,NK,ND)                      OGCK0106
   95 CONTINUE                                                          OGCK0107
  961 CONTINUE                                                          OGCK0108
      NK=1
   3  READ(8,333,END=115) NZ,NXZ,INS,KLS,IQ,NL,KT,
     1(BOT(I),I=1,6),nkt,LZ,LF,ID,ld,IP,MS,PRV,NTR,MP,PRVG,DEK,MID      OGCK0111
  333 format(i2,20a2,22i6)
      IF(NZ.EQ.10.AND.PRVG.NE.CON1) KB10=NK                             OGCK0112
      IF(NZ.EQ.10.AND.PRVG.EQ.CON1) KD10=NK                             OGCK0113
      IF(NZ.EQ.15.AND.PRVG.NE.CON1) KB15=NK                             OGCK0114
      IF(NZ.EQ.15.AND.PRVG.EQ.CON1) KD15=NK                             OGCK0115
      NK=NK+1                                                           OGCK0117
      GOTO 3                                                            OGCK0118
  115 NZ=10                                                             OGCK0119
      N1=1                                                              OGCK0120
      N2=KB10                                                           OGCK0121
      IF(KB10.EQ.0) GOTO 81                                             OGCK0122
      IBD=0                                                             OGCK0123
      IZ=KBZ10                                                          OGCK0124
  100 IF(IZ.EQ.0) GOTO 81                                               OGCK0125
      DO 80 IM=1,IZ                                                     OGCK0126
      L=72                                                              OGCK0127
      N=1                                                               OGCK0128
      DO 40 I=1,9                                                       OGCK0129
      ISM(I)=0                                                          OGCK0130
   40 ISZ(I)=0                                                          OGCK0131
      LST=1                                                             OGCK0132
      DO 82 NK=N1,N2                                                    OGCK0133
    8 READ(8,333,rec=NK) NZ,NXZ,INS,KLS,IQ,NL,KT,(BOT(I),I=1,6),NKT,LZ, OGCK0134
     1lf,ld,id,IP,MS,PRV,NTR,MP,PRVG,DEK,MID                            OGCK0135
      IF(NZ.EQ.10.AND.NL.EQ.15.AND.IBD.EQ.1) IBD=2                      OGCK0136
      IF(NZ.EQ.10.AND.NL.NE.15.AND.IBD.EQ.2) IBD=1                      OGCK0137
      IF(NZ.EQ.15.AND.NL.EQ.1.AND.IBD.EQ.1) IBD=2                       OGCK0138
      IF(NZ.EQ.15.AND.NL.NE.1.AND.IBD.EQ.2) IBD=1                       OGCK0139
      IF(NK.NE.N1) GOTO 507                                             OGCK0140
      IF(LSTC.LT.LSTCK) GOTO 506                                        OGCK0141
      IF(IBD.EQ.2) WRITE(2,16) NZ,ND,NM,NG,LST                          OGCK0142
      IF(IBD.EQ.1) WRITE(2,15) NZ,ND,NM,NG,LST                          OGCK0143
      IF(IBD.EQ.0) WRITE(2,11) NZ,ND,NM,NG,LST                          OGCK0144
      WRITE(2,1)                                                        OGCK0145
      WRITE(2,101)                                                      OGCK0146
  506 L=L-18                                                            OGCK0147
  507 CONTINUE                                                          OGCK0148
      IF(LF.EQ.CON) LF=10000                                            OGCK0149
      IF(NK.EQ.N1) GOTO 9                                               OGCK0150
      IF(NL1.EQ.NL) GOTO 9                                              OGCK0151
      IF(LSTC.LT.LSTCK) GOTO 500                                        OGCK0152
      IF(L.LE.2) WRITE(2,2000) LSTC                                     OGCK0153
      IF(L.LE.2) WRITE(2,1001)                                          OGCK0154
      WRITE(2,20) ISM(9),(ISM(I),I=1,8)                                 OGCK0155
  500 IF(L.LE.2) L=72                                                   OGCK0156
      L=L-2                                                             OGCK0157
      IF(IPR.EQ.0) GOTO 10                                              OGCK0158
      IF(LSTC.LT.LSTCK) L=72                                            OGCK0159
      IF(LSTC.LT.LSTCK) LST=1                                           OGCK0160
      IF(LSTC.LT.LSTCK) LSTC=LSTC+1                                     OGCK0161
      IF(LSTC.LT.LSTCK) GOTO 501                                        OGCK0162
      LL1=L                                                             OGCK0163
      DO 401 I=1,LL1                                                    OGCK0164
  401 WRITE(2,1001)                                                     OGCK0165
      WRITE(2,2000) LSTC                                                OGCK0166
      LSTC=LSTC+1                                                       OGCK0167
      L=72                                                              OGCK0168
      LST=1                                                             OGCK0169
      IF(IBD.EQ.2) WRITE(2,16) NZ,ND,NM,NG,LST                          OGCK0170
      IF(IBD.EQ.1) WRITE(2,15) NZ,ND,NM,NG,LST                          OGCK0171
      IF(IBD.EQ.0) WRITE(2,11) NZ,ND,NM,NG,LST                          OGCK0172
      WRITE(2,1)                                                        OGCK0173
      WRITE(2,101)                                                      OGCK0174
  501 L=L-18                                                            OGCK0175
      IF(L.LE.0) L=72+L                                                 OGCK0176
   10 DO 111 I=1,9                                                      OGCK0177
111   ISM(I)=0                                                          OGCK0178
   9  IF(LSTC.LT.LSTCK) GOTO 502                                        OGCK0179
      IF(L.LE.2) WRITE(2,2000) LSTC                                     OGCK0180
      IF(L.LE.2) WRITE(2,1001)                                          OGCK0181
  502 IF(L.LE.2) GOTO 402                                               OGCK0182
403   CONTINUE                                                          OGCK0183
      NTR=NTR/10                                                        OGCK0184
      DO 160 I=1,6                                                      OGCK0185
      IF(BOT(I).LE.0) NKT=NKT-BOT(I)                                    OGCK0186
      IF(BOT(I).LE.0) BOT(I)=0                                          OGCK0187
 160  CONTINUE                                                          OGCK0188
      KT=NTR                                                            OGCK0189
      IF(LSTC.LT.LSTCK) GOTO 504                                        OGCK0190
      IF(NL.EQ.33)                                                      OGCK0191
     1WRITE(2,25)   (NXZ(I),I=1,4),INS,KLS,IQ,NL2,KT,(BOT(I),I=1,6),NKT,OGCK0192
     2LZ,LF,LD,ID,IP,MS,MP,MID                                          OGCK0193
      IF(NL.EQ.33) GOTO 504                                             OGCK0194
      WRITE(2,25)   (NXZ(I),I=1,4),INS,KLS,IQ,NL,KT,(BOT(I),I=1,6),NKT, OGCK0195
     1LZ,LF,LD,ID,IP,MS,MP,MID                                          OGCK0196
  504 CONTINUE                                                          OGCK0197
      L=L-1                                                             OGCK0198
      N=N+1                                                             OGCK0199
      ISM(1)=ISM(1)+KT                                                  OGCK0200
      ISM(8)=ISM(8)+NKT                                                 OGCK0201
      ISM(9)=ISM(9)+1                                                   OGCK0202
      DO 122 I=1,6                                                      OGCK0203
122   ISM(I+1)=ISM(I+1)+BOT(I)                                          OGCK0204
      ISZ(1)=ISZ(1)+KT                                                  OGCK0205
      ISZ(8)=ISZ(8)+NKT                                                 OGCK0206
      ISZ(9)=ISZ(9)+1                                                   OGCK0207
      DO 133 I=1,6                                                      OGCK0208
133   ISZ(I+1)=ISZ(I+1)+BOT(I)                                          OGCK0209
      NL1=NL                                                            OGCK0210
 82   CONTINUE                                                          OGCK0211
      IF(LSTC.LT.LSTCK) GOTO 505                                        OGCK0212
      WRITE(2,20) ISM(9),(ISM(I),I=1,8)                                 OGCK0213
      WRITE(2,21) ISZ(9),(ISZ(I),I=1,8)                                 OGCK0214
  505 L=L-3                                                             OGCK0215
      IF(LSTC.LT.LSTCK) LSTC=LSTC+1                                     OGCK0216
      IF(LSTC.LT.LSTCK) GOTO 80                                         OGCK0217
      IF(L.EQ.0) GOTO 80                                                OGCK0218
      IF(L.LT.0) L=72+L                                                 OGCK0219
      LL1=L                                                             OGCK0220
      DO 400 I=1,LL1                                                    OGCK0221
400   WRITE(2,1001)                                                     OGCK0222
      WRITE(2,2000) LSTC                                                OGCK0223
      LSTC=LSTC+1                                                       OGCK0224
 80   CONTINUE                                                          OGCK0225
      GOTO 81                                                           OGCK0226
  402 LST=LST+1                                                         OGCK0227
      LSTC=LSTC+1                                                       OGCK0228
      IF(LSTC.LT.LSTCK) GOTO 503                                        OGCK0229
      IF(IBD.EQ.2) WRITE(2,16) NZ,ND,NM,NG,LST                          OGCK0230
      IF(IBD.EQ.0) WRITE(2,11) NZ,ND,NM,NG,LST                          OGCK0231
      IF(IBD.EQ.1) WRITE(2,15) NZ,ND,NM,NG,LST                          OGCK0232
      WRITE(2,1)                                                        OGCK0233
      WRITE(2,101)                                                      OGCK0234
  503 L=54                                                              OGCK0235
      GOTO 403                                                          OGCK0236
 81   KOL=KOL+1                                                         OGCK0237
      IF(KOL.GT.1) GOTO 150                                             OGCK0238
      IZ=KBU10                                                          OGCK0239
      IPR=1                                                             OGCK0240
      IF(KB10.EQ.0) GOTO 81                                             OGCK0241
      GOTO 100                                                          OGCK0242
150   IF(KOL.GT.2) GOTO 151                                             OGCK0243
      NZ=15                                                             OGCK0244
      IZ=KBZ15                                                          OGCK0245
      IF(KD10.EQ.0) N1=KB10+1                                           OGCK0246
      IF(KD10.NE.0) N1=KD10+1                                           OGCK0247
      N2=KB15                                                           OGCK0248
      IPR=0                                                             OGCK0249
      IBD=0                                                             OGCK0250
      IF(KB15.EQ.0) GOTO 81                                             OGCK0251
      GOTO 100                                                          OGCK0252
151   IF(KOL.GT.3) GOTO 152                                             OGCK0253
      IZ=KBU15                                                          OGCK0254
      IPR=1                                                             OGCK0255
      IF(KB15.EQ.0) GOTO 81                                             OGCK0256
      GOTO 100                                                          OGCK0257
152   IF(KOL.GT.4) GOTO 153                                             OGCK0258
      NZ=10                                                             OGCK0259
      IZ=KPZ10                                                          OGCK0260
      N2=KD10                                                           OGCK0261
      N1=KB10+1                                                         OGCK0262
      IBD=1                                                             OGCK0263
      IPR=0                                                             OGCK0264
      IF(KD10.EQ.0) GOTO 81                                             OGCK0265
      GOTO 100                                                          OGCK0266
  153 IF(KOL.GT.5) GOTO 154                                             OGCK0267
      IZ=KPU10                                                          OGCK0268
      IPR=1                                                             OGCK0269
      IF(KD10.EQ.0) GOTO 81                                             OGCK0270
      GOTO 100                                                          OGCK0271
  154 IF(KOL.GT.6) GOTO 155                                             OGCK0272
      IPR=0                                                             OGCK0273
      NZ=15                                                             OGCK0274
      IZ=KPZ15                                                          OGCK0275
      IF(KB15.EQ.0.AND.KD10.GT.KB10) N1=KD10+1                          OGCK0276
      IF(KB10.EQ.0.AND.KD10.LE.KB10) N1=KB10+1                          OGCK0277
      IF(KB15.NE.0) N1=KB15+1                                           OGCK0278
      N2=KD15                                                           OGCK0279
      IBD=1                                                             OGCK0280
      IF(KD15.EQ.0) GOTO 81                                             OGCK0281
      GOTO 100                                                          OGCK0282
  155 IF(KOL.GT.7) GOTO 156                                             OGCK0283
      IZ=KPU15                                                          OGCK0284
      IPR=1                                                             OGCK0285
      IBD=2                                                             OGCK0286
      IF(KD15.EQ.0) GOTO 81                                             OGCK0287
      GOTO 100                                                          OGCK0288
  156 CONTINUE                                                          OGCK0289
   25 FORMAT(1X,      4A2,1X,5A2,1X,10A2,T43,I4,T48,I2,T50,I5,1X,7I4,1X,OGCK0290
     1I3,1X,I6,1X,I6,3X,I6,2X,I1,1X,I1,1X,I6,1X,I6)                     OGCK0291
      END                                                               OGCK0292
      SUBROUTINE PTZG(LP,NU,KDP,U,ZG,NUC,NH,NM,NG,NK,NN,ND,U1)          OGCK0293
       INTEGER ZC(25),ZM,ZH,RM,RH,U(20,1),ZG(100,1)                     OGCK0294
      INTEGER*2 KDP(1),U1(20,1)                                         OGCK0295
      INTEGER*2 WNN,WKK                                                 OGCK0296
      INTEGER*2 WN1                                                     OGCK0297
    9 FORMAT(1x,'ÈTOÃO ÏO MACT.',A2,2I6)                                OGCK0298
   11 FORMAT(1x,13HÈTOÃO ÏO ÖEXÓ,3X,2I6)                                OGCK0299
    6 FORMAT(1x,15X,27HÇAÃPÓÇKA ÏOÄPAÇÄEËEHÈÈ ÖEXA,I4,11H ÏO ÄHßM HA,   OGCK0300
     1I3,1H.,I2,'. 19', I2,  37H ÃOÄA(B ÏPOÖEHTAX K ÄHEBHOMÓ PECÓPCÓ))  OGCK0301
    1 FORMAT(1X,28HÏOÄPAÇÄEËEHÈE   PECÓPC  ÏËAH,23(I4))                 OGCK0302
    2 FORMAT(1X,   8A2,    I6,I6  ,23(I4    ))                          OGCK0303
    8 FORMAT(1X,120(1H-))                                               OGCK0305
  10  FORMAT(1H )                                                       OGCK0306
      L=72
      ZH=0                                                              OGCK0307
      RH=0                                                              OGCK0308
      WRITE(2,6) NH,ND,NM,NG                                            OGCK0309
      WRITE(2,8)                                                        OGCK0310
      LP1=2*LP                                                          OGCK0311
      J1=1                                                              OGCK0312
      J2=10                                                             OGCK0313
      WKK=NK/100                                                        OGCK0314
      WNN=NN/100                                                        OGCK0315
      DO 12 I=LP,LP1                                                    OGCK0316
      WN1=KDP(I)/10                                                     OGCK0317
      IF(WKK.EQ.WN1) J2=I-LP                                            OGCK0318
      IF(WNN.EQ.WN1) J1=I-LP                                            OGCK0319
   12 CONTINUE                                                          OGCK0320
      IF(J2.GT.23) J2=23                                                OGCK0321
      J3=J1+LP                                                          OGCK0322
      J4=J2+LP                                                          OGCK0323
      DO 13 I=J3,J4                                                     OGCK0324
      J0=I-J3+1                                                         OGCK0325
   13 ZC(J0)=KDP(I)/1000                                                OGCK0326
      J5=J2-J1+1                                                        OGCK0327
      WRITE(2,1) (ZC(I),I=1,J5)                                         OGCK0328
      WRITE(2,8)                                                        OGCK0329
      N=4                                                               OGCK0330
      DO 48 I=1,NU                                                      OGCK0331
   48 U(12,I)=0                                                         OGCK0332
      M=0                                                               OGCK0333
      J4=0                                                              OGCK0334
      DO 3 I=1,NU                                                       OGCK0335
      IF(U(7,I).LE.0) GO TO 3                                           OGCK0336
       IF(U(12,I).GT.0) GO TO 3                                         OGCK0337
      M=M+1                                                             OGCK0338
      NMM=U1(20,I)                                                      OGCK0339
      IF(U1(1,I).NE.NH) GO TO 3                                         OGCK0340
      ZM=0                                                              OGCK0341
      RM=0                                                              OGCK0342
      DO 49 I1=I,NU                                                     OGCK0343
      IF(U1(1,I1).NE.NH) GO TO 49                                       OGCK0344
      IF(U1(20,I1).NE.NMM) GO TO 49                                     OGCK0345
      IF(U(7,I1).LE.0) GOTO 49                                          OGCK0346
      U(12,I1)=1                                                        OGCK0347
      J4=0                                                              OGCK0348
      ZC(25)=0                                                          OGCK0349
      DO 7 J=1,J2                                                       OGCK0350
      J4=J4+1                                                           OGCK0351
      ZC(25)=ZC(25)+ZG(M,J)                                             OGCK0352
    7 ZC(J4)=(ZG(M,J)*100+U(4,I1)/2)/U(4,I1)                            OGCK0353
      ZC(24)=(U(4,I1)*(J2-J1+1)+50)/100                                 OGCK0354
      M=M+1                                                             OGCK0355
      ZM=ZM+ZC(25)                                                      OGCK0356
      RM=RM+ZC(24)                                                      OGCK0357
      ZC(25)=(ZC(25)+50)/100                                            OGCK0358
      WRITE(2,2) (U1(J,I1),J=13,20),ZC(24),ZC(25),(ZC(J),J=1,J5)        OGCK0359
      N=N+1                                                             OGCK0360
      IF(N.LE.L-5) GO TO 14                                             OGCK0361
      N=N+1                                                             OGCK0362
      DO 15 K=N,L                                                       OGCK0363
   15 WRITE(2,10)                                                       OGCK0364
      N=0                                                               OGCK0365
      L=72                                                              OGCK0366
   14 CONTINUE                                                          OGCK0367
  49  CONTINUE                                                          OGCK0368
      ZM=(ZM+50)/100                                                    OGCK0369
      WRITE(2,8)                                                        OGCK0370
      WRITE(2,9) NMM,RM,ZM                                               OGCK0371
      WRITE(2,10)
      N=N+3                                                             OGCK0372
      IF(N.LE.L-5) GO TO 16                                             OGCK0373
      N=N+1                                                             OGCK0374
      DO 17 K=N,L                                                       OGCK0375
   17 WRITE(2,10)                                                       OGCK0376
      N=0                                                               OGCK0377
      L=72                                                              OGCK0378
   16 CONTINUE                                                          OGCK0379
      ZH=ZH+ZM                                                          OGCK0380
      RH=RH+RM                                                          OGCK0381
      M=M-1                                                             OGCK0382
    3 CONTINUE                                                          OGCK0383
      WRITE(2,8)                                                        OGCK0384
      WRITE(2,11) RH,ZH                                                 OGCK0385
      N=N+4                                                             OGCK0386
      DO 18 I=N,L                                                       OGCK0387
   18 WRITE(2,10)                                                       OGCK0388
      RETURN                                                            OGCK0389
      END                                                               OGCK0390
      SUBROUTINE PTPD(LP,NU,KDP,U,ZG,NUC,NH,NN,NK,NK1,ND)               OGCK0391
      INTEGER ZC(24),ZR(24),U(20,1),ZG(100,1)                           OGCK0392
      INTEGER*2 KDP(1)                                                  OGCK0393
      INTEGER*2 WNN,WKK                                                 OGCK0394
      INTEGER*2 OP(6,7) / 'CË','EC','AP','HA','ß ','  ',                OGCK0395
     1'TO','KA','PH','Aß','  ','  ','ÔP','EÇ','EP','HA','ß ',           OGCK0396
     2'  ','ØË','ÈÔ','OB','KA','  ','  ','KO','OP','Ä-','PA','CT','O×', OGCK0397
     3     'ÇA','ÃO','TO','BK','A ','  ','  ','  ','  ','  ','  ','  '/ OGCK0398
      DATA L/72/                                                        OGCK0399
  10  FORMAT(1H )                                                       OGCK0400
   21 FORMAT(1H ,30X,22HOÁ'EMHAß ÇAÃPÓÇKA ÖEXA, I4,'  HA  ',I2,1H.,I2,  OGCK0401
     1'. 19',I2,5H ÃOÄA/)                                               OGCK0402
   22 FORMAT(1H ,20X,1H!,I4,1H!,6A2,1H!,2(I10,1H!),I4,1H!)              OGCK0403
   23 FORMAT(1H ,20X,46(1H-))                                           OGCK0404
   24 FORMAT(1H ,20X,1H!,4X,  1H!,  12X,1H!,2(10X,1H!),4X,1H!)          OGCK0405
   25 FORMAT(1H ,20X,1H!,7X,11HÈ T O Ã O !,2(I10,1H!),I4,1H!)           OGCK0406
   26 FORMAT(1H ,20X,6H!H/H !,12X, 12H!  PECÓPC  !,16HÇ A Ã P Ó Ç K A!, OGCK0407
     1/ 21X,19H!    !  OÏEPAÖÈÈ  !, 26(1H-),1H!,/21X,                   OGCK0408
     26H!Ï/Ï !,12X,1H!,2( 2X,5HH/×AC,3X,1H!),2H %,3H  !)                OGCK0409
      NO=6                                                              OGCK0410
      I1=NN/10000                                                       OGCK0411
      I4=NN                                                             OGCK0412
      I3=NK                                                             OGCK0413
      I1=1                                                              OGCK0414
      LP1=2*LP                                                          OGCK0415
      WKK=NK1/100                                                       OGCK0416
      DO 12 I=LP,LP1                                                    OGCK0417
      WNN=KDP(I)/10                                                     OGCK0418
      IF(WKK.EQ.WNN) I2=I-LP                                            OGCK0419
   12 CONTINUE                                                          OGCK0420
    9 WRITE(2,10)                                                       OGCK0421
      WRITE(2,10)                                                       OGCK0422
      WRITE(2,21) NH,ND,I4,I3                                           OGCK0423
      WRITE(2,23)                                                       OGCK0424
      WRITE(2,26)                                                       OGCK0425
      WRITE(2,23)                                                       OGCK0426
      DO 32 K=1,24                                                      OGCK0427
      ZR(K)=0                                                           OGCK0428
  32  ZC(K)=0                                                           OGCK0429
      M=0                                                               OGCK0430
      DO 30 I=1,NU                                                      OGCK0431
      K=U(8,I)                                                          OGCK0432
      IF(U(7,I).LE.0) GOTO 30                                           OGCK0433
      M=M+1                                                             OGCK0434
      ZR(K)=ZR(K)+U(4,I)                                                OGCK0435
      DO 31 J=I1,I2                                                     OGCK0436
  31  ZC(K)=ZC(K)+ZG(M,J)                                               OGCK0437
 30   CONTINUE                                                          OGCK0438
      JR=0                                                              OGCK0439
      JS=0                                                              OGCK0440
      DO 27 J=1,NO                                                      OGCK0441
      J1=J                                                              OGCK0442
      IF(NH.GT.10) J1=J+7                                               OGCK0443
      ZC(J1)=(ZC(J1)+50)/100                                            OGCK0444
      ZR(J1)=(ZR(J1)*(I2-I1+1)+50)/100                                  OGCK0445
      IF(ZR(J1).LE.0) WRITE(2,22) J,(OP(I,J),I=1,NO),ZR(J1),ZC(J1)      OGCK0446
      JR=JR+ZR(J1)                                                      OGCK0447
      IF(ZR(J1).NE.0) JK=(ZC(J1)*100+ZR(J1)/2)/ZR(J1)                   OGCK0448
      JS=JS+ZC(J1)                                                      OGCK0449
      IF(ZR(J1).NE.0)  WRITE(2,22) J,(OP(I,J),I=1,NO),ZR(J1),ZC(J1),JK  OGCK0450
      WRITE(2,24)                                                       OGCK0451
  27  CONTINUE                                                          OGCK0452
      WRITE(2,23)                                                       OGCK0453
      IF(JR.EQ.0) JR=1                                                  OGCK0454
      JK=(JS*100+JR/2)/JR                                               OGCK0455
      WRITE(2,25) JR,JS,JK                                              OGCK0456
      WRITE(2,23)                                                       OGCK0457
      DO 29 I=24,L                                                      OGCK0458
  29  WRITE(2,10)                                                       OGCK0459
      RETURN                                                            OGCK0460
      END                                                               OGCK0461
 
