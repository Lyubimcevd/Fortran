      EXTERNAL REGIST                                                   ETMD0001
      INTEGER*2 A(26,200),S(2,200),P(2,200),TQ(6,500),TQC(4,500),       ETMD0002
     1NXZ(10,4),U(20,100),KDP(180),I0(13,5000),KDP1,TPD(17)             ETMD0003
      INTEGER QZ(4,500),UW(2,200),IW(100)                               ETMD0004
      INTEGER H3(6),H4(6),H5(6),H6(6),H7(6),H12(2),H13(2)               ETMD0005
      INTEGER H8(6),H14(6),H15(6)                                       ETMD0006
      INTEGER H16(6),H17(6),H18(6),H19(6),H20(6),H21(6),H22(6),H23(6)   ETMD0007
      INTEGER*2 EDF/-1/,IKT(1)                                             7ETMD0008
      INTEGER RES,PL,MF,PRM,TPL,PRBP,PRBF,PRBBF,PRB,PRH,CYT             ETMD0009
      INTEGER*2 ZG(100,60),NKP(1)                                              ETMD0010
      INTEGER PRIZ                                                      ETMD0011
      INTEGER F(100,3)                                                  ETMD0012
      INTEGER F1,F2,F3                                                  ETMD0013
      INTEGER*2 A4/' 3'/,A5/' 4'/                                       ETMD0014
      INTEGER*2 A6/' 5'/,A7/' 7'/                                       ETMD0015
      INTEGER*2 A0/' 0'/                                                ETMD0016
      INTEGER*2 A1/'  '/                                                ETMD0017
      INTEGER*2 A8/' 6'/                                                ETMD0018
      INTEGER*2 A2/' 1'/,A3/' 2'/                                       ETMD0019
      INTEGER*2 MET/'ÑÏ'/,METZ/'2 '/                                    ETMD0020
      INTEGER*2 U5(100)                                                 ETMD0021
      integer*2 dummy2
      integer*4 dummy4,dummy
 3000 FORMAT(//////////15X,'ÏPOTOKOË BÛÏOËHEHÈß Ó×ETA',1X,I6//10X,'ÁÛËO'ETMD0022
     1,5X,'CÓTO×',5X,'CTAËO')                                           ETMD0023
 3001 FORMAT(6X,I8,5X,I8,5X,I8//////////)                               ETMD0024
      DATA H14/'HEBE','PEH ','PACÖ','EHOK',' COÓ','    '/               ETMD0025
      DATA H15/'HEBE','PEH ','PACÖ','EHOK',' ÏË-','ÃP  '/               ETMD0026
      DATA N1/03/,N2/08/,NF/04/,N0/1/,ND/10/                            ETMD0027
      DATA H3/'HEBE','PHA ',' MTK','    ',' COÓ','    '/                ETMD0028
      DATA H4/'HEBE','PHA ',' MTK','    ',' ÏË-','ÃP  '/                ETMD0029
      DATA H5/'HEBE','PHA ','  TO','    ',' COÓ','    '/                ETMD0030
      DATA H6/'HEBE','PHA ',' TO ','    ',' ÏË-','ÃP  '/                ETMD0031
      DATA H7/'HEBE','PEH ','Ø.OÁ','.   ','COÓ ','    '/                ETMD0032
      DATA H8/'HEBE','PEH ','Ø.OÁ','.   ',' ÏË-','ÃP  '/                ETMD0033
      DATA H16/'HET ','B ÏË','AHE,','ÏOPT',' ÏË-','ÃP  '/               ETMD0034
      DATA H17/'HET ','B ÏË','AHE,','ÏOPT',' COÓ','    '/               ETMD0035
      DATA H18/'HEBE','PHO ','ÇAÏO','ËHEH','ÈE  ',' COÓ'/               ETMD0036
      DATA H19/'HEBE','PHO ','ABAH','-HÈE','    ',' COÓ'/               ETMD0037
      DATA H20/'OÏEP','AÖÈß','BÛÏO','ËHEH','A   ',' COÓ'/               ETMD0038
      DATA H21/'ÇAKA','Ç 9 ','ÏPÈO','PÈTE','TA  ',' COÓ'/               ETMD0039
      DATA H22/'KOPP','EKÖÈ','ß HE','BÛÏ.','OÏ-È',' COÓ'/               ETMD0040
      DATA H23/'HEBE','PHO ','ÇAKP','ÛTÈE','ÇAÃ.',' COÓ'/               ETMD0041
      DATA H12/'BHEA','CÓÏ.'/                                           ETMD0042
      DATA H13/'ÈÇ Ï','OPT.'/                                           ETMD0043
      DATA M0/0/,IC/0/                                                  ETMD0044
      open(13,file='F:\ASUIPW\tek_INF\cbodbal.dat')
      DATA ND1/11/,N3/9/                                                ETMD0046
      open(unit=N3,file='F:\ASUIPW\tek_INF\zarcyt.dat')
      open(10,file='F:\ASUIPW\tek_INF\cou.dat',access='direct',
     1form='formatted',recl=52)
      open(11,file='F:\ASUIPW\tek_INF\nepu4.dat')
   12 FORMAT(10A2,2I2,I5,I4,I6,I7,I6)                                   ETMD0049
    4 FORMAT(2X,2A2,3X,A2,1X,I6,2X,I4,1X,4A2,3X,I1,2X,I5,1X,I4,1H-,I3,1XETMD0050
     *,6A4,1X,2A4)                                                      ETMD0051
   21 FORMAT(I2,I2,I6,I6,I7,4A2,A2,I2,I2,2A2,I2,i6,i7,i6,i7,i6,i7)      ETMD0052
  300 FORMAT(8A2,8I5)                                                   ETMD0053
      IFP=0                                                             ETMD0054
      IFT=0                                                             ETMD0055
      ICYT=0                                                            ETMD0056
      NI=1                                                              ETMD0057
      NIJ=1                                                             ETMD0058
      open(666,file='tmp.txt')
      read(666,777) idata
  777 format(i6)
      PRIZ=0                                                            ETMD0060
10001 I0(1,1)=ND                                                        ETMD0061
      KOL=0                                                             ETMD0062
      open(unit=nf,file='F:\ASUIPW\tek_INF\u.dat',form='unformatted')
      READ (NF) NW,NU                                                   ETMD0063
      READ (NF) ((UW(I,J),I=1,2),J=1,NW),((U(I,J),I=1,20),J=1,NU)       ETMD0064
      REWIND NF                                                         ETMD0065
      NUC=0                                                             ETMD0066
      DO 9991 J=1,NU                                                    ETMD0067
      IF(U(7,J).LE.0) GOTO 9991                                         ETMD0068
      NUC=NUC+1                                                         ETMD0069
 9991 CONTINUE                                                          ETMD0070
      DO 50 J=1,NU                                                      ETMD0071
      U(9,J)=0                                                          ETMD0072
      U(10,J)=0                                                         ETMD0073
      U(11,J)=0                                                         ETMD0074
      U(12,J)=0                                                         ETMD0075
      U(6,J)=0                                                          ETMD0076
      U5(J)=U(5,J)                                                      ETMD0077
      U(5,J)=0                                                          ETMD0078
   50 CONTINUE                                                          ETMD0079
      open(unit=n1,file='F:\ASUIPW\tek_INF\plan.dat',form='unformatted')
      open(unit=n2,file='F:\ASUIPW\tek_INF\plan1.dat',
     1form='unformatted')
      READ (N1) LP,NN,NK                                                ETMD0080
      LP1=LP+LP                                                         ETMD0081
      READ (N1) (KDP(I),I=1,LP1)                                        ETMD0082
      REWIND N1                                                         ETMD0083
      IF(IDATA/100-IDATA/10000*100.EQ.NK/100-NK/10000*100) GOTO 600     ETMD0084
      IDATA=NK                                                          ETMD0085
600   CALL KALDNI(NK,0,NKP)                                             ETMD0086
      DO 900 I4=LP,LP1                                                  ETMD0087
      IF(KDP(I4).EQ.NKP(1)) GOTO 901                                       ETMD0088
900   CONTINUE                                                          ETMD0089
901   NKP=I4-LP                                                         ETMD0090
      I4=IDATA                                                          ETMD0091
      CALL KALDNI(I4,0,IKT)                                             ETMD0092
      DO 902 I4=LP,LP1                                                  ETMD0093
      IF(KDP(I4).EQ.IKT(1)) GOTO 903                                       ETMD0094
902   CONTINUE                                                          ETMD0095
903   IDT=I4-LP                                                         ETMD0096
      I0(2,1)=IDATA/100                                                 ETMD0097
      I0(3,1)=IDATA-IDATA/100*100                                       ETMD0098
      DO 10 I=1,50000                                                   ETMD0099
      READ(ND,12,end=17) (TPD(J),J=1,14),ITN,JJJ,NZY                    ETMD0100
      IF(N0.LE.1) GO TO 13                                              ETMD0102
      DO 14 J=1,4                                                       ETMD0103
      IF(TPD(J).NE.I0(J,N0)) GOTO 15                                    ETMD0104
   14 CONTINUE                                                          ETMD0105
      GO TO 10                                                          ETMD0106
   15 I0(12,N0)=I-1                                                     ETMD0107
   13 N0=N0+1                                                           ETMD0108
      DO 16 J=1,4                                                       ETMD0109
   16 I0(J,N0)=TPD(J)                                                   ETMD0110
      I0(11,N0)=I                                                       ETMD0111
      I0(13,N0)=0                                                       ETMD0112
   10 CONTINUE                                                          ETMD0113
   17 I0(12,N0)=I-1                                                     ETMD0114
      CALL PEREPS(N1,N2,NU,U,LP,KDP,NXZ,IW,A,S,P,TQ,QZ,TQC,N0,M0,I0,    ETMD0115
     *REGIST,1,NW,UW,NI,U5,IDT)                                         ETMD0116
      READ(N1) ((ZG(I,J),I=1,NUC),J=1,LP),((F(I,J),I=1,NUC),J=1,3)      ETMD0117
      REWIND N1                                                         ETMD0118
      LHTR=0                                                            ETMD0119
      LHRS=0                                                            ETMD0120
      LHTRS=0                                                           ETMD0121
      LHRSS=0                                                           ETMD0122
      open(1,file='F:\ASUIPW\tek_INF\portfel.dat',form='unformatted')
      open(2,file='F:\ASUIPW\tek_INF\portfel1.dat',form='unformatted')
   85 READ(1) NXZ,(IW(I),I=1,NU),IQ,IP,ID,LZ,MP,NA,NS,NT                ETMD0129
      IF(NA.LT.0) GOTO 86                                               ETMD0130
      READ(1) ((A(I,J),I=1,26),J=1,NA)                                  ETMD0131
      READ(1) ((S(I,J),I=1,2),J=1,NS),((P(I,J),I=1,2),J=1,NS)           ETMD0132
      READ(1) ((TQ(I,J),I=1,6),J=1,NT),((QZ(I,J),I=1,4),J=1,NT)         ETMD0133
      READ(1) ((TQC(I,J),I=1,4),J=1,NT)                                 ETMD0134
      DO 92 I=2,N0                                                      ETMD0135
      IF(I0(13,I).EQ.1) GOTO 92                                         ETMD0136
      DO 93 J=1,4                                                       ETMD0137
      IF(I0(J,I).NE.NXZ(J,1)) GOTO 92                                   ETMD0138
   93 CONTINUE                                                          ETMD0139
      I0(13,I)=1                                                        ETMD0140
      I1=I0(11,I)                                                       ETMD0141
      I2=I0(12,I)                                                       ETMD0142
      DO 94 I3=I1,I2                                                    ETMD0143
      READ(ND,12,rec=i3) (TPD(K),K=1,14),ITN,JJJ,NZY                    ETMD0144
      IF(TPD(5).NE.MET) GOTO 88888                                      ETMD0145
      IF(IPRCP.EQ.1) GOTO 94                                            ETMD0146
88888 IPRCP=0                                                           ETMD0147
      IPR=ITN/100000                                                    ETMD0148
      INTN=ITN-ITN/100000*100000                                        ETMD0149
      IF(IP.NE.9) GOTO 110                                              ETMD0150
      WRITE(ND1,4) (TPD(M),M=9,10),TPD(8),NZY,TPD(14),(TPD(M),M=1,4),   ETMD0151
     *IPR,INTN,TPD(11),TPD(12),H21,H13                                  ETMD0152
      NI=NI+1                                                           ETMD0153
      IPRCP=1                                                           ETMD0154
      GOTO 94                                                           ETMD0155
  110 CONTINUE                                                          ETMD0156
      IF(TPD(8).NE.A5.AND.TPD(8).NE.A6.AND.TPD(8).NE.A8) GOTO 500       ETMD0157
  503 CONTINUE                                                          ETMD0158
      DO 501 IK=1,NW                                                    ETMD0159
      IF(UW(1,IK).EQ.NZY) GOTO 502                                      ETMD0160
  501 CONTINUE                                                          ETMD0161
      GOTO 87                                                           ETMD0162
  502 NZ1=NZY/100                                                       ETMD0163
      IF(TPD(8).EQ.A4) GOTO 1022                                        ETMD0164
      U(9,UW(2,IK))=U(9,UW(2,IK))+TPD(13)/100                           ETMD0165
      U(10,UW(2,IK))=U(10,UW(2,IK))+(TPD(13)-TPD(13)/100*100)           ETMD0166
      GOTO 1021                                                         ETMD0167
1022  U(9,UW(2,IK))=U(9,UW(2,IK))-TPD(13)/100                           ETMD0168
      U(10,UW(2,IK))=U(10,UW(2,IK))-(TPD(13)-TPD(13)/100*100)           ETMD0169
 1021 INZ=NZY/10000                                                     ETMD0170
      INM=NZY/100-INZ*100                                               ETMD0171
      LTR=TPD(13)-(TPD(13)*U5(UW(2,IK)))/100                            ETMD0172
      LRS=JJJ-(JJJ*U5(UW(2,IK)))/100                                    ETMD0173
      WRITE(N3,21) INZ,INM,ITN,TPD(13),JJJ,(TPD(K),K=1,4),TPD(8),       ETMD0174
     *TPD(11),TPD(12),(TPD(M),M=9,10),U(8,UW(2,IK)),LTR,LRS,LHTR,LHRS,  ETMD0175
     1LHTRS,LHRSS                                                       ETMD0176
      GOTO 94                                                           ETMD0177
  500 IF(TPD(11).NE.1) GOTO 6000                                        ETMD0178
      IF(TPD(12).NE.1) GOTO 6000                                        ETMD0179
      IF(TQ(2,2).NE.101) GOTO 6000                                      ETMD0180
      IF(NZY.EQ.150101) GOTO 6001                                       ETMD0181
      WRITE(ND1,4) (TPD(M),M=9,10),TPD(8),NZY,TPD(14),(TPD(M),M=1,4),   ETMD0182
     *IPR,INTN,TPD(11),TPD(12),H23,H13                                  ETMD0183
      NI=NI+1                                                           ETMD0184
      IPRCP=1                                                           ETMD0185
      GOTO 94                                                           ETMD0186
 6001 IF(TPD(8).NE.A0.AND.TPD(8).NE.A1.AND.TPD(8).NE.A4.AND.TPD(8).NE.A2ETMD0187
     *) GOTO 6000                                                       ETMD0188
      TPD(11)=99                                                        ETMD0189
      TPD(12)=1                                                         ETMD0190
 6000 DO 95 I4=1,NA                                                     ETMD0191
      IF(A(9,I4).NE.TPD(11)) GOTO 95                                    ETMD0192
      IF(TPD(12).GT.A(13,I4).OR.TPD(12).LT.1) GOTO 88                   ETMD0193
      I5=A(12,I4)+TPD(12)-1                                             ETMD0194
      IF(NZY-NZY/100*100.EQ.0.AND.NZY.NE.0) NZY=NZY+(TQ(2,I5)-TQ(2,I5)/1ETMD0195
     100*100)                                                           ETMD0196
      IF(TPD(13).NE.0.OR.JJJ.NE.0) GOTO 8010                            ETMD0197
      IF(NXZ(10,1).NE.METZ) GOTO 99990                                  ETMD0198
      IF(TQC(2,I5).EQ.0) GOTO 99990                                     ETMD0199
      IF(TQC(1,I5).LE.-1000) GOTO 99990                                 ETMD0200
      GOTO 99991                                                        ETMD0201
99990 IF(QZ(1,I5).GT.32000) GOTO 801                                    ETMD0202
      IF(TPD(13).EQ.0) TPD(13)=QZ(1,I5)                                 ETMD0203
  801 IF(JJJ.EQ.0) JJJ=QZ(2,I5)                                         ETMD0204
      GOTO 8011                                                         ETMD0205
 8010 IF(TPD(13).EQ.0) TPD(13)=1                                        ETMD0206
      IF((TPD(8).EQ.A3.OR.TPD(8).EQ.A7).AND.TQC(2,I5).NE.0) GOTO 99996  ETMD0207
      IF(TPD(8).EQ.A3.OR.TPD(8).EQ.A4.OR.TPD(8).EQ.A7) GOTO 8011        ETMD0208
      IF(TPD(8).EQ.A2) GOTO 8013                                        ETMD0209
      IF(TQC(2,I5).EQ.0.AND.TQC(3,I5).NE.0.AND.TPD(5).NE.MET) GOTO 99999ETMD0210
      GOTO 8014                                                         ETMD0211
99999 J11=TQC(1,I5)+LP                                                  ETMD0212
      IF(J11.LE.0) IDAT=9999                                            ETMD0213
      IF(J11.LE.0) GOTO 99998                                           ETMD0214
      IDAT=KDP(J11)/10                                                  ETMD0215
99998 INTN=TQC(3,I5)+32000                                              ETMD0216
      WRITE(ND1,4) (TPD(M),M=9,10),TPD(8),NZY,TPD(14),(TPD(M),M=1,4),   ETMD0217
     *IPR,INTN,TPD(11),TPD(12),H20,H13                                  ETMD0218
      NI=NI+1                                                           ETMD0219
      IPRCP=1                                                           ETMD0220
      GOTO 94                                                           ETMD0221
99996 INTN=TQ(3,I5)+32000                                               ETMD0222
      WRITE(ND1,4) (TPD(M),M=9,10),TPD(8),NZY,TPD(14),(TPD(M),M=1,4),   ETMD0223
     *IPR,INTN,TPD(11),TPD(12),H22,H13                                  ETMD0224
      NI=NI+1                                                           ETMD0225
      IPRCP=1                                                           ETMD0226
      GOTO 94                                                           ETMD0227
 8013 IF(IDT.LT.NKP(1)-2.AND.NZY-NZY/100*100.EQ.83.AND.NZY/10000.EQ.15)    ETMD0228
     *GOTO 904                                                          ETMD0229
      IF(IDT.LT.NKP(1)-31.AND.NZY-NZY/100*100.EQ.83.AND.NZY/10000.EQ.10)   ETMD0230
     *GOTO 904                                                          ETMD0231
      IF(TPD(13).EQ.1) GOTO 90400                                       ETMD0232
      IF(QZ(1,I5).LT.TPD(13)) GOTO 904                                  ETMD0233
90400 IF(NXZ(5,1).EQ.MET) GOTO 904                                      ETMD0234
      QZ(1,I5)=QZ(1,I5)-TPD(13)                                         ETMD0235
      IF(NXZ(10,1).EQ.METZ) GOTO 80111                                  ETMD0236
      QZ(2,I5)=QZ(2,I5)-JJJ                                             ETMD0237
      GOTO 80111                                                        ETMD0238
 8014 IF(QZ(1,I5).GE.TPD(13)) GOTO 80111                                ETMD0239
      IF(NXZ(5,1).EQ.MET) GOTO 80111                                    ETMD0240
99991 WRITE(ND1,4) (TPD(M),M=9,10),TPD(8),NZY,TPD(14),(TPD(M),M=1,4),   ETMD0241
     *IPR,INTN,TPD(11),TPD(12),H18,H13                                  ETMD0242
      NI=NI+1                                                           ETMD0243
      IPRCP=1                                                           ETMD0244
      GOTO 94                                                           ETMD0245
 8011 IF(TQC(2,I5).EQ.0.AND.TQC(3,I5).NE.0.AND.TPD(8).EQ.A0) GOTO 99999 ETMD0246
      IF(TQC(2,I5).EQ.0.AND.TQC(3,I5).NE.0.AND.TPD(8).EQ.A1) GOTO 99999 ETMD0247
80111 IF(NZY.EQ.0) GOTO 800                                             ETMD0248
      DO 81 IK=1,NW                                                     ETMD0249
      IF(UW(1,IK).EQ.NZY) GOTO 82                                       ETMD0250
   81 CONTINUE                                                          ETMD0251
      GOTO 87                                                           ETMD0252
  800 U(9,TQ(1,I5))=U(9,TQ(1,I5))+QZ(1,I5)/100                          ETMD0253
      U(10,TQ(1,I5))=U(10,TQ(1,I5))+(QZ(1,I5)-QZ(1,I5)/100*100)         ETMD0254
      INZ=U(1,TQ(1,I5))                                                 ETMD0255
      ITM=TQ(2,I5)/100                                                  ETMD0256
      TQ15=TQ(1,I5)                                                     ETMD0257
99997 IF(QZ(1,I5).EQ.1) QZ(1,I5)=0                                      ETMD0258
      LTR=QZ(1,I5)-(QZ(1,I5)*U5(TQ15))/100                              ETMD0293
      LRS=QZ(2,I5)-(QZ(2,I5)*U5(TQ15))/100                              ETMD0294
      WRITE(N3,21) INZ,ITM,ITN,QZ(1,I5),QZ(2,I5),(TPD(K),K=1,4          ETMD0295
     *),TPD(8),TPD(11),TPD(12),(TPD(M),M=9,10),U(8,TQ15),LTR,LRS,       ETMD0296
     1LHTR,LHRS,LHTRS,LHRSS                                             ETMD0297
      GOTO 83                                                           ETMD0300
  82  IF(TPD(8).NE.A4) GOTO 820                                         ETMD0301
      IF(TPD(13).EQ.0) GOTO 8282                                        ETMD0302
      U(9,UW(2,IK))=U(9,UW(2,IK))-TPD(13)/100                           ETMD0303
      U(10,UW(2,IK))=U(10,UW(2,IK))-(TPD(13)-TPD(13)/100*100)           ETMD0304
      GOTO 821                                                          ETMD0305
 8282 U(9,UW(2,IK))=U(9,UW(2,IK))-QZ(1,I5)/100                          ETMD0306
      U(10,UW(2,IK))=U(10,UW(2,IK))-(QZ(1,I5)-QZ(1,I5)/100*100)         ETMD0307
      GOTO 821                                                          ETMD0308
  820 IF(TPD(8).EQ.A7) GOTO 400                                         ETMD0309
  401 IF(TPD(13).EQ.0) GOTO 4010                                        ETMD0310
      U(9,UW(2,IK))=U(9,UW(2,IK))+TPD(13)/100                           ETMD0311
      U(10,UW(2,IK))=U(10,UW(2,IK))+(TPD(13)-TPD(13)/100*100)           ETMD0312
      GOTO 4011                                                         ETMD0313
 4010 U(9,UW(2,IK))=U(9,UW(2,IK))+QZ(1,I5)/100                          ETMD0314
      U(10,UW(2,IK))=U(10,UW(2,IK))+(QZ(1,I5)-QZ(1,I5)/100*100)         ETMD0315
 4011 CONTINUE                                                          ETMD0316
  821 INZ=NZY/10000                                                     ETMD0317
      INM=NZY/100-INZ*100                                               ETMD0318
      ITM=INM                                                           ETMD0319
      TQ15=UW(2,IK)                                                     ETMD0320
      IF(TPD(13).EQ.0) GOTO 99997                                       ETMD0321
      IF(TPD(13).EQ.1) TPD(13)=0                                        ETMD0322
      IF(TPD(8).EQ.A0.AND.TPD(13).EQ.QZ(1,I5).AND.JJJ.EQ.QZ(2,I5)) GOTO ETMD0323
     *99997                                                             ETMD0324
      IF(TPD(8).EQ.A1.AND.TPD(13).EQ.QZ(1,I5).AND.JJJ.EQ.QZ(2,I5)) GOTO ETMD0325
     *99997                                                             ETMD0326
      LTR=TPD(13)-(TPD(13)*U5(UW(2,IK)))/100                            ETMD0327
      LRS=JJJ-(JJJ*U5(UW(2,IK)))/100                                    ETMD0328
      WRITE(N3,21) INZ,INM,ITN,TPD(13),JJJ,(TPD(K),K=1,4),TPD(8),       ETMD0329
     *TPD(11),TPD(12),(TPD(M),M=9,10),U(8,UW(2,IK)),LTR,LRS,LHTR,LHRS,  ETMD0330
     1LHTRS,LHRSS                                                       ETMD0331
      GOTO 83                                                           ETMD0332
 4012 LTR=QZ(1,I5)-(QZ(1,I5)*U5(UW(2,IK)))/100                          ETMD0333
      LRS=JJJ-(JJJ*U5(UW(2,IK)))/100                                    ETMD0334
      WRITE(N3,21) INZ,INM,ITN,QZ(1,I5),JJJ,(TPD(K),K=1,4),TPD(8),      ETMD0335
     *TPD(11),TPD(12),(TPD(M),M=9,10),U(8,UW(2,IK)),LTR,LRS,LHTR,LHRS,  ETMD0336
     1LHTRS,LHRSS                                                       ETMD0337
   83 IF(JJJ.GE.100000) JJJ=99999                                       ETMD0338
      IF(NXZ(5,1).EQ.MET) GOTO 94                                       ETMD0340
      IF(TPD(8).NE.A0.AND.TPD(8).NE.A1) GOTO 94                         ETMD0341
      TQC(2,I5)=0                                                       ETMD0342
      TQC(3,I5)=INTN-32000                                              ETMD0343
      TQC(1,I5)=IDT                                                     ETMD0344
      GOTO 94                                                           ETMD0345
  400 CONTINUE                                                          ETMD0346
      ITN=IPR*100000+INTN                                               ETMD0347
      GOTO 401                                                          ETMD0348
  88  IF(NZY.EQ.0) GOTO 202                                             ETMD0349
      WRITE(ND1,4) (TPD(M),M=9,10),TPD(8),NZY,TPD(14),(TPD(M),M=1,4),   ETMD0350
     *IPR,INTN,TPD(11),TPD(12),H5,H13                                   ETMD0351
      NI=NI+1                                                           ETMD0352
      IPRCP=1                                                           ETMD0353
      GOTO 94                                                           ETMD0354
  202 N=A(10,1)                                                         ETMD0355
      N=S(1,N)                                                          ETMD0356
      N=A(12,N)+A(13,N)-1                                               ETMD0357
      N=TQ(1,N)                                                         ETMD0358
      N=U(1,N)                                                          ETMD0359
      NZY=N*10000                                                       ETMD0360
  201 WRITE(ND1,4) (TPD(M),M=9,10),TPD(8),NZY,TPD(14),(TPD(M),M=1,4),   ETMD0361
     *IPR,INTN,TPD(11),TPD(12),H6,H13                                   ETMD0362
      NI=NI+1                                                           ETMD0363
      GOTO 94                                                           ETMD0364
   95 CONTINUE                                                          ETMD0365
      IF(NZY.EQ.0) GOTO 203                                             ETMD0366
      WRITE(ND1,4) (TPD(M),M=9,10),TPD(8),NZY,TPD(14),(TPD(M),M=1,4),   ETMD0367
     *IPR,INTN,TPD(11),TPD(12),H3,H13                                   ETMD0368
      NI=NI+1                                                           ETMD0369
      IPRCP=1                                                           ETMD0370
      GOTO 94                                                           ETMD0371
  203 N=A(10,1)                                                         ETMD0372
      N=S(1,N)                                                          ETMD0373
      N=A(12,N)+A(13,N)-1                                               ETMD0374
      N=TQ(1,N)                                                         ETMD0375
      N=U(1,N)                                                          ETMD0376
      NZY=N*10000                                                       ETMD0377
  200 WRITE(ND1,4) (TPD(M),M=9,10),TPD(8),NZY,TPD(14),(TPD(M),M=1,4),   ETMD0378
     *IPR,INTN,TPD(11),TPD(12),H4,H13                                   ETMD0379
      NI=NI+1                                                           ETMD0380
      GOTO 94                                                           ETMD0381
904   WRITE(ND1,4) (TPD(M),M=9,10),TPD(8),NZY,TPD(14),(TPD(M),M=1,4),   ETMD0382
     *IPR,INTN,TPD(11),TPD(12),H19,H13                                  ETMD0383
      IPRCP=1                                                           ETMD0384
      NI=NI+1                                                           ETMD0385
      GOTO 94                                                           ETMD0386
   87 WRITE(ND1,4) (TPD(M),M=9,10),TPD(8),NZY,TPD(14),(TPD(M),M=1,4),   ETMD0387
     *IPR,INTN,TPD(11),TPD(12),H7,H13                                   ETMD0388
      NI=NI+1                                                           ETMD0389
      IPRCP=1                                                           ETMD0390
   94 CONTINUE                                                          ETMD0391
      IPRCP=0                                                           ETMD0392
   92 CONTINUE                                                          ETMD0393
      write(2) NXZ,(IW(I),I=1,NU),IQ,IP,ID,LZ,MP,NA,NS,NT               ETMD0129
      write(2) ((A(I,J),I=1,26),J=1,NA)                                 ETMD0131
      write(2) ((S(I,J),I=1,2),J=1,NS),((P(I,J),I=1,2),J=1,NS)          ETMD0132
      write(2) ((TQ(I,J),I=1,6),J=1,NT),((QZ(I,J),I=1,4),J=1,NT)        ETMD0133
      write(2) ((TQC(I,J),I=1,4),J=1,NT)                                ETMD0134
      GOTO 85                                                           ETMD0394
  86  write(2) NXZ,(IW(I),I=1,NU),IQ,IP,ID,LZ,MP,NA,NS,NT               ETMD0129
      CONTINUE                                                          ETMD0395
      REWIND 1                                                          ETMD0396
      rewind 2
      n0=n0-1
      DO 1 I=2,N0                                                       ETMD0397
      IF(I0(13,I).EQ.1) GOTO 1                                          ETMD0398
      I1=I0(11,I)                                                       ETMD0399
      I2=I0(12,I)                                                       ETMD0400
      DO 35 J=I1,I2                                                     ETMD0401
      READ(ND,12,rec=J) (TPD(K),K=1,14),ITN,JJJ,NZY                     ETMD0402
      IF(TPD(5).NE.MET) GOTO 88887                                      ETMD0403
      IF(IPRCP.EQ.1) GOTO 35                                            ETMD0404
88887 IPRCP=0                                                           ETMD0405
      IPR=ITN/100000                                                    ETMD0406
      INTN=ITN-ITN/100000*100000                                        ETMD0407
      GOTO 700                                                          ETMD0408
      IF(TPD(13)) 116,115,116                                           ETMD0409
116   IF(JJJ) 117,115,117                                               ETMD0410
  117 IF(TPD(13).EQ.32000) GOTO 115                                     ETMD0411
118   WRITE(ND1,4) (TPD(M),M=9,10),TPD(8),NZY,TPD(14),(TPD(M),M=1,4),   ETMD0412
     *IPR,INTN,TPD(11),TPD(12),H14,H12                                  ETMD0413
      NI=NI+1                                                           ETMD0414
      GOTO 35                                                           ETMD0415
115   DO 41 K=1,NW                                                      ETMD0416
      IF(UW(1,K).EQ.NZY) GOTO 22                                        ETMD0417
   41 CONTINUE                                                          ETMD0418
700   IF(NZY.NE.0) GOTO 1010                                            ETMD0419
      IF(NZY.EQ.0) GOTO 1009                                            ETMD0420
      IF(TPD(13).EQ.0.AND.JJJ.EQ.0) GOTO 1010                           ETMD0421
      WRITE(ND1,4) (TPD(M),M=9,10),TPD(8),NZY,TPD(14),(TPD(M),M=1,4),   ETMD0422
     *IPR,INTN,TPD(11),TPD(12),H7,H12                                   ETMD0423
      NI=NI+1                                                           ETMD0424
      IPRCP=1                                                           ETMD0425
      GOTO 35                                                           ETMD0426
 1009 WRITE(ND1,4) (TPD(M),M=9,10),TPD(8),NZY,TPD(14),(TPD(M),M=1,4),   ETMD0427
     *IPR,INTN,TPD(11),TPD(12),H16,H12                                  ETMD0428
      NI=NI+1                                                           ETMD0429
      GOTO 35                                                           ETMD0430
 1010 WRITE(ND1,4) (TPD(M),M=9,10),TPD(8),NZY,TPD(14),(TPD(M),M=1,4),   ETMD0431
     *IPR,INTN,TPD(11),TPD(12),H17,H12                                  ETMD0432
      NI=NI+1                                                           ETMD0433
      IPRCP=1                                                           ETMD0434
      GOTO 35                                                           ETMD0435
   22 IF(NZY.EQ.150101) GOTO 1011                                       ETMD0436
      IF(TPD(13).EQ.0.AND.JJJ.EQ.0) GOTO 1010                           ETMD0437
 1011 CONTINUE                                                          ETMD0438
      IF(TPD(8).EQ.A4) GOTO 5000                                        ETMD0439
      IF(TPD(8).EQ.A7) GOTO 402                                         ETMD0440
  405 NZ1=NZY/100                                                       ETMD0441
      IF(NZ1.EQ.1501) GOTO 1026                                         ETMD0442
      U(9,UW(2,K))=U(9,UW(2,K))+TPD(13)/100                             ETMD0443
      U(10,UW(2,K))=U(10,UW(2,K))+(TPD(13)-TPD(13)/100*100)             ETMD0444
      GOTO 5001                                                         ETMD0445
  402 CONTINUE                                                          ETMD0446
      ITN=IPR*100000+INTN                                               ETMD0447
      GOTO 405                                                          ETMD0448
 1026 U(5,UW(2,K))=U(5,UW(2,K))+TPD(13)/100                             ETMD0449
      U(6,UW(2,K))=U(6,UW(2,K))+(TPD(13)-TPD(13)/100*100)               ETMD0450
      GOTO 5001                                                         ETMD0451
 5000 NZ1=NZY/100                                                       ETMD0452
      IF(NZ1.EQ.1501) GOTO 1027                                         ETMD0453
      U(9,UW(2,K))=U(9,UW(2,K))-TPD(13)/100                             ETMD0454
      U(10,UW(2,K))=U(10,UW(2,K))-(TPD(13)-TPD(13)/100*100)             ETMD0455
      GOTO 5001                                                         ETMD0456
 1027 U(5,UW(2,K))=U(5,UW(2,K))-TPD(13)/100                             ETMD0457
      U(6,UW(2,K))=U(6,UW(2,K))-(TPD(13)-TPD(13)/100*100)               ETMD0458
 5001 INZ=NZY/10000                                                     ETMD0459
      INM=NZY/100-INZ*100                                               ETMD0460
      LTR=TPD(13)-(TPD(13)*U5(UW(2,K)))/100                             ETMD0461
      LRS=JJJ-(JJJ*U5(UW(2,K)))/100                                     ETMD0462
      WRITE(N3,21) INZ,INM,ITN,TPD(13),JJJ,(TPD(M),M=1,4),TPD(8),       ETMD0463
     *TPD(11),TPD(12),(TPD(M),M=9,10),U(8,UW(2,K)),LTR,LRS,LHTR,LHRS,   ETMD0464
     1LHTRS,LHRSS                                                       ETMD0465
      IF(NZY.NE.150101) GOTO 35                                         ETMD0466
      IF(TPD(8).NE.A0.AND.TPD(8).NE.A1) GOTO 35                         ETMD0467
      IF(TPD(11).EQ.1) TPD(11)=99                                       ETMD0468
      IF(TPD(12).EQ.1) TPD(12)=1                                        ETMD0469
   35 CONTINUE                                                          ETMD0471
    1 CONTINUE                                                          ETMD0472
      TPD(1)=EDF                                                        ETMD0473
      KOH=NKP(1)                                                           ETMD0476
      KD=IDT                                                            ETMD0477
      IF(IDT.NE.NKP(1)) KD=IDT-1                                           ETMD0478
      J=1                                                               ETMD0479
      DO 23 I=1,NU                                                      ETMD0480
      IF(U(7,I).EQ.0) GOTO 23                                           ETMD0481
      PL=0                                                              ETMD0482
      TPL=0                                                             ETMD0483
      IFP=F(J,1)+F(J,2)+IFP                                             ETMD0484
      IF(PRIZ.EQ.2) GOTO 2424                                           ETMD0485
      F(J,1)=F(J,1)+U(5,I)*100+U(6,I)                                   ETMD0486
      F(J,2)=F(J,2)+U(9,I)*100+U(10,I)                                  ETMD0487
      F(J,3)=F(J,3)+U(11,I)*100+U(12,I)                                 ETMD0488
 2424 CONTINUE                                                          ETMD0489
      IFT=F(J,1)+F(J,2)+IFT                                             ETMD0490
      DO 24 M=1,KOH                                                     ETMD0491
   24 PL=PL+ZG(J,M)*10                                                  ETMD0492
      IF(PL.EQ.0) PL=1                                                  ETMD0493
      DO 26 M=1,KD                                                      ETMD0494
   26 TPL=TPL+ZG(J,M)*10                                                ETMD0495
      IF(TPL.EQ.0) TPL=1                                                ETMD0496
      MF=F(J,1)+F(J,2)                                                  ETMD0497
      CYT=U(5,I)*100+U(6,I)+U(9,I)*100+U(10,I)                          ETMD0498
      ICYT=ICYT+CYT                                                     ETMD0499
      RES=((U(4,I)*U(3,I)+50)/100*KOH+50)/100                           ETMD0500
      MF=(MF+50)/100                                                    ETMD0501
      F1=(F(J,1)+50)/100                                                ETMD0502
      F2=(F(J,2)+50)/100                                                ETMD0503
      F3=(F(J,3)+50)/100                                                ETMD0504
      CYT=(CYT+50)/100                                                  ETMD0505
      PL=(PL+50)/100                                                    ETMD0506
      TPL=(TPL+50)/100                                                  ETMD0507
      WRITE(13,300) (U(M,I),M=13,20),RES,PL,MF,TPL,F1,F2,F3,CYT         ETMD0508
      NIJ=NIJ+1                                                         ETMD0509
      J=J+1                                                             ETMD0510
   23 CONTINUE                                                          ETMD0511
      end file 13
   29 WRITE(N2) ((ZG(I,J),I=1,NUC),J=1,LP),((F(I,J),I=1,NUC),J=1,3)     ETMD0512
      REWIND N2                                                         ETMD0513
      REWIND N3                                                         ETMD0514
      open(6,file='prn')
      WRITE (*,3000) IDATA                                              ETMD0515
      WRITE (*,3001) IFP,ICYT,IFT                                       ETMD0516
      pause'çàïèøè äàííûå è íàæìè êë."ENTER"'  
  103 STOP                                                              ETMD0517
      END                                                               ETMD0518
      SUBROUTINE REGIST(NU,U,LP,KDP,NXZ,IW,IP,LZ,LF,MP,MS,NA,NS,NT,     ETMD0519
     *A,S,P,TQ,QZ,TQC,N0,M0,I0,IND1,NW,UW,NI,NKP,KDPG,U5,IDT)           ETMD0520
      INTEGER*2 A(26,1),TQ(6,1),TQC(4,1),NXZ(10,4),U(20,100),KDP(1),    ETMD0521
     1I0(13,1),IKDP(1),TPD(17),S(2,1)                                   ETMD0522
      INTEGER*2 U5(1),KDPG(1)                                           ETMD0523
      INTEGER*2 NKP(1)                                                  ETMD0524
      INTEGER QZ(4,1),UW(2,200)                                         ETMD0525
      INTEGER H3(6),H4(6),H5(6),H6(6),H7(6),H11(2)                      ETMD0526
      INTEGER H14(6),H15(6),H16(6),H17(6)                               ETMD0527
      INTEGER*2 A0/'  '/,A1/' 1'/,A2/' 2'/,A3/' 3'/                     ETMD0528
      INTEGER*2 A5/' 4'/,A6/' 5'/                                       ETMD0529
      INTEGER*2 A7/' 6'/,A8/' 7'/                                       ETMD0530
      INTEGER H18(6),H19(6),H20(6),H21(6),H22(6)                        ETMD0531
      INTEGER*2 K13,KR13                                               ETMD0532
      INTEGER*2 A4/' 0'/                                                ETMD0533
      INTEGER*2 MET/'ÑÏ'/,METZ/'2 '/                                    ETMD0534
    4 FORMAT(2X,2A2,3X,A2,1X,I6,2X,I4,1X,4A2,3X,I1,2X,I5,1X,I4,1H-,I3,1XETMD0535
     *,6A4,1X,2A4)                                                      ETMD0536
   12 FORMAT(10A2,2I2,I5,I4,I6,I7,I6)                                   ETMD0537
   21 FORMAT(I2,I2,I6,I6,I7,4A2,A2,I2,I2,2A2,I2,i6,i7,i6,i7,i6,i7)      ETMD0538
   22 FORMAT(I4,I6,I5,I5,4A2)                                           ETMD0539
      DATA N3/9/                                                        ETMD0540
      DATA H17/'HEBE','PHA ','PACÖ','EHKA',' PL-','ÃP  '/               ETMD0541
      DATA H16/'HEBE','PHA ','PACÖ','EHKA',' COÓ','    '/               ETMD0542
      DATA H18/'ÏP-K','KOP.','7 B ','ÇAK.','ÈÇ Ï','ËAHA'/               ETMD0543
      DATA H3/'HEBE','PHA ',' MTK','    ',' COÓ','    '/                ETMD0544
      DATA H4/'HEBE','PHA ',' MTK','    ',' ÏË-','ÃP  '/                ETMD0545
      DATA H5/'HEBE','PHA ','  TO','    ',' COÓ','    '/                ETMD0546
      DATA H6/'HEBE','PHA ','  TO','    ',' ÏË-','ÃP  '/                ETMD0547
      DATA H7/'HEBE','PEH ','Ø.OÁ','.   ',' COÓ','    '/                ETMD0548
      DATA H14/'KOPP','EKÖÈ','ß HE','BÛÏ.','OÏ-È',' COÓ'/               ETMD0549
      DATA H15/'OÏEP','AÖÈß',' BÛÏ','OËHE','HA  ','    '/               ETMD0550
      DATA H19/'HEBE','PHO ','ÇAÏO','ËHEH','ÈE  ',' COÓ'/               ETMD0551
      DATA H20/'HEBE','PHO ','ABAH','-HÈE','    ',' COÓ'/               ETMD0552
      DATA H21/'ÇAKA','Ç 9 ','ÏPÈO','PÈTE','TA  ',' COÓ'/               ETMD0553
      DATA H22/'HEBE','PHO ','ÇAKP','ÛTÈE','ÇAÃ.',' COÓ'/               ETMD0554
      DATA H11/'ÏËAH','OBÛÉ'/                                           ETMD0555
      DATA ND1/11/                                                      ETMD0556
      IND1=1                                                            ETMD0557
      LP1=LP+LP                                                         ETMD0558
      KD=IDT-1                                                          ETMD0559
      ND=I0(1,1)                                                        ETMD0560
      DO 3 I=2,N0                                                       ETMD0561
      IF(I0(13,I).EQ.1) GOTO 3                                          ETMD0562
      DO 50 J=1,4                                                       ETMD0563
      IF(I0(J,I).NE.NXZ(J,1)) GOTO 3                                    ETMD0564
   50 CONTINUE                                                          ETMD0565
      I0(13,I)=1                                                        ETMD0566
      I1=I0(11,I)                                                       ETMD0567
      I2=I0(12,I)                                                       ETMD0568
      DO 6 J=I1,I2                                                      ETMD0569
      READ(ND,12,rec=J) (TPD(K),K=1,14),ITN,JJJ,NZY                     ETMD0570
      IF(TPD(5).NE.MET) GOTO 88888                                      ETMD0571
      IF(IPRCP.EQ.1) GOTO 6                                             ETMD0572
88888 IPRCP=0                                                           ETMD0573
      LHTR=0                                                            ETMD0574
      LHRS=0                                                            ETMD0575
      LHTRS=0                                                           ETMD0576
      LHRSS=0                                                           ETMD0577
      I4=TPD(14)*100+I0(3,1)                                            ETMD0578
      IF(TPD(14).GE.I0(2,1)) GOTO 903                                   ETMD0579
 13   CALL KALDNI(I4,0,IKDP)                                            ETMD0580
      DO 900 I4=LP,LP1                                                  ETMD0581
      IF(KDP(I4).EQ.IKDP(1)) GOTO 5                                     ETMD0582
 900  CONTINUE                                                          ETMD0583
      I4=I0(2,1)*100+I0(3,1)                                            ETMD0584
      GOTO 13                                                           ETMD0585
 5    KD=I4-LP                                                          ETMD0586
 903  IF(KD.EQ.1) KD=KD+1                                               ETMD0587
      JIK=10                                                            ETMD0588
      IPR=ITN/100000                                                    ETMD0589
      INTN=ITN-ITN/100000*100000                                        ETMD0590
      IF(IP.NE.9) GOTO 110                                              ETMD0591
      WRITE(ND1,4) (TPD(M),M=9,10),TPD(8),NZY,TPD(14),(TPD(M),M=1,4),   ETMD0592
     *IPR,INTN,TPD(11),TPD(12),H21,H11                                  ETMD0593
      NI=NI+1                                                           ETMD0594
      IPRCP=1                                                           ETMD0595
      GOTO 6                                                            ETMD0596
  110 IF(TPD(8).NE.A5.AND.TPD(8).NE.A6.AND.TPD(8).NE.A7) GOTO 200       ETMD0597
      DO 250 IK=1,NW                                                    ETMD0598
      IF(UW(1,IK).EQ.NZY) GOTO 260                                      ETMD0599
  250 CONTINUE                                                          ETMD0600
      GOTO 17                                                           ETMD0601
260   U(5,UW(2,IK))=U(5,UW(2,IK))+TPD(13)/100                           ETMD0602
      U(6,UW(2,IK))=U(6,UW(2,IK))+TPD(13)-TPD(13)/100*100               ETMD0603
      INZ=NZY/10000                                                     ETMD0604
      INM=NZY/100-INZ*100                                               ETMD0605
      LTR=TPD(13)-(TPD(13)*U5(UW(2,IK)))/100                            ETMD0606
      LRS=JJJ-(JJJ*U5(UW(2,IK)))/100                                    ETMD0607
      WRITE(N3,21) INZ,INM,ITN,TPD(13),JJJ,(TPD(K),K=1,4),TPD(8),TPD(11 ETMD0608
     *),TPD(12),(TPD(M),M=9,10),U(8,UW(2,IK)),LTR,LRS,LHTR,LHRS,LHTRS,  ETMD0609
     1LHRSS                                                             ETMD0610
      NZ1=NZY/100                                                       ETMD0611
      GOTO 6                                                            ETMD0612
  200 IF(TPD(11).EQ.1.AND.TPD(12).EQ.1) GOTO 22000                      ETMD0613
      GOTO 8013                                                         ETMD0614
22000 IF(TQ(2,2).NE.101) GOTO 8013                                      ETMD0615
      IF(NZY.EQ.150101) GOTO 2000                                       ETMD0616
      WRITE(ND1,4) (TPD(M),M=9,10),TPD(8),NZY,TPD(14),(TPD(M),M=1,4),   ETMD0617
     *IPR,INTN,TPD(11),TPD(12),H22,H11                                  ETMD0618
      NI=NI+1                                                           ETMD0619
      IPRCP=1                                                           ETMD0620
      GOTO 6                                                            ETMD0621
2000  IF(TPD(8).NE.A4.AND.TPD(8).NE.A0.AND.TPD(8).NE.A1.AND.TPD(8).NE.A3ETMD0622
     *) GOTO 8013                                                       ETMD0623
      TPD(11)=99                                                        ETMD0624
      TPD(12)=1                                                         ETMD0625
8013  DO 7 J1=1,NA                                                      ETMD0626
      IF(A(9,J1).NE.TPD(11)) GO TO 7                                    ETMD0627
      J2=A(12,J1)+TPD(12)-1                                             ETMD0628
      IF(TPD(12).GT.A(13,J1).OR.TPD(12).LT.1) GO TO 2                   ETMD0629
      IF(NZY-NZY/100*100.EQ.0.AND.NZY.NE.0) NZY=NZY+(TQ(2,J2)-TQ(2,J2)/1ETMD0630
     100*100)                                                           ETMD0631
      INTN1=INTN-32000                                                  ETMD0632
      IF(TQC(2,J2).EQ.0) GOTO 1                                         ETMD0633
      IF(TPD(8).NE.A0.AND.TPD(8).NE.A4) GOTO 8                          ETMD0634
  135 CONTINUE                                                          ETMD0635
      IF(TPD(13).NE.0.OR.JJJ.NE.0) GOTO 8010                            ETMD0636
      IF(NXZ(10,1).NE.METZ) GOTO 8011                                   ETMD0637
      IF(TQC(2,J2).EQ.0) GOTO 8011                                      ETMD0638
      IF(TQC(1,J2).LE.-1000) GOTO 8011                                  ETMD0639
      GOTO 18011                                                        ETMD0640
 8010 IF(TPD(13).EQ.1) GOTO 8011                                        ETMD0641
      IF(TPD(13).EQ.0) TPD(13)=1                                        ETMD0642
      IF(QZ(1,J2).GE.TPD(13)) GOTO 8011                                 ETMD0643
      IF(NXZ(5,1).EQ.MET) GOTO 8011                                     ETMD0644
18011 WRITE(ND1,4) (TPD(M),M=9,10),TPD(8),NZY,TPD(14),(TPD(M),M=1,4),   ETMD0645
     *IPR,INTN,TPD(11),TPD(12),H19,H11                                  ETMD0646
      NI=NI+1                                                           ETMD0647
      IPRCP=1                                                           ETMD0648
      GOTO 6                                                            ETMD0649
 8011 CONTINUE                                                          ETMD0650
  23  IF(NZY.EQ.0) GOTO 25                                              ETMD0651
      DO 24 IK=1,NW                                                     ETMD0652
      IF(UW(1,IK).EQ.NZY) GOTO 26                                       ETMD0653
   24 CONTINUE                                                          ETMD0654
      GOTO 17                                                           ETMD0655
   25 U(5,TQ(1,J2))=U(5,TQ(1,J2))+QZ(1,J2)/100                          ETMD0656
      U(6,TQ(1,J2))=U(6,TQ(1,J2))+(QZ(1,J2)-QZ(1,J2)/100*100)           ETMD0657
      INZ=U(1,TQ(1,J2))                                                 ETMD0658
      INM=TQ(2,J2)/100                                                  ETMD0659
      IWR=U(8,TQ(1,J2))                                                 ETMD0660
      IWP=U5(TQ(1,J2))                                                  ETMD0661
 3004 CONTINUE                                                          ETMD0662
      IF(NXZ(5,1).NE.MET) GOTO 3000                                     ETMD0663
      IF(TQ(4,J2).GT.NKP(1)) GOTO 3002                                     ETMD0664
      IF(TQ(5,J2).LE.NKP(1)) GOTO 3007                                     ETMD0665
      IU2=U(2,TQ(1,J2))                                                 ETMD0666
      IF(IP.EQ.0.AND.U(2,TQ(1,J2)).LE.820) IU2=IU2*2                    ETMD0667
      KQZ=(NKP(1)-TQ(4,J2)+1)*IU2                                          ETMD0668
      LHTR=KQZ-QZ(3,J2)                                                 ETMD0669
      LHRS=QZ(1,J2)                                                     ETMD0670
      IF(QZ(1,J2).NE.0) LHRS=QZ(1,J2)*LHTR/QZ(1,J2)                     ETMD0671
      QZ(3,J2)=QZ(3,J2)+KQZ                                             ETMD0672
      GOTO 3008                                                         ETMD0673
 3007 LHTR=QZ(1,J2)-QZ(3,J2)                                            ETMD0674
      LHRS=QZ(2,J2)                                                     ETMD0675
      IF(QZ(1,J2).NE.0) LHRS=QZ(2,J2)*LHTR/QZ(1,J2)                     ETMD0676
      QZ(3,J2)=QZ(3,J2)+QZ(1,J2)                                        ETMD0677
 3008 IF(LHTR.LE.0) LHTR=0                                              ETMD0678
      IF(LHRS.LE.0) LHRS=0                                              ETMD0679
      GOTO 3002                                                         ETMD0680
 3000 IF(JIK.EQ.15.AND.TQC(1,J2).GT.1.AND.TQ(4,J2).LE.NKP(1)) GOTO 3001    ETMD0681
      IF(JIK.EQ.15) LHTR=0                                              ETMD0682
      IF(JIK.EQ.15) LHRS=0                                              ETMD0683
      IF(JIK.EQ.15) GOTO 3002                                           ETMD0684
      IF(TQ(4,J2).GT.NKP(1)) LHTR=0                                        ETMD0685
      IF(TQ(4,J2).GT.NKP(1)) LHRS=0                                        ETMD0686
      IF(TQ(4,J2).GT.NKP(1)) GOTO 3002                                     ETMD0687
 3001 IF(TQ(4,J2).LE.NKP(1).AND.TQ(5,J2).GT.NKP(1)) GOTO 3003                 ETMD0688
      LHTR=QZ(1,J2)                                                     ETMD0689
      LHRS=QZ(2,J2)                                                     ETMD0690
      GOTO 3002                                                         ETMD0691
 3003 IU2=U(2,TQ(1,J2))                                                 ETMD0692
      IF(IP.EQ.0.AND.U(2,TQ(1,J2)).LE.820) IU2=IU2*2                    ETMD0693
      LHTR=(NKP(1)-TQ(4,J2)+1)*IU2                                         ETMD0694
      LHRS=QZ(2,J2)                                                     ETMD0695
      IF(QZ(1,J2).NE.0) LHRS=QZ(2,J2)*LHTR/QZ(1,J2)                     ETMD0696
 3002 U(11,TQ(1,J2))=U(11,TQ(1,J2))+LHTR/100                            ETMD0697
      U(12,TQ(1,J2))=U(12,TQ(1,J2))+(LHTR-LHTR/100*100)                 ETMD0698
      IF(QZ(1,J2).EQ.1) QZ(1,J2)=0                                      ETMD0699
      LTR=QZ(1,J2)-(QZ(1,J2)*IWP)/100                                   ETMD0746
      LRS=QZ(2,J2)-(QZ(2,J2)*IWP)/100                                   ETMD0747
      LHTRS=LHTR-(LHTR*IWP)/100                                         ETMD0748
      LHRSS=LHRS-(LHRS*IWP)/100                                         ETMD0749
      WRITE(N3,21) INZ,INM,ITN,QZ(1,J2),QZ(2,J2),(TPD(K),K=1,4          ETMD0750
     *),TPD(8),TPD(11),TPD(12),(TPD(M),M=9,10),IWR,LTR,LRS,LHTR,LHRS,   ETMD0751
     1LHTRS,LHRSS                                                       ETMD0752
      TQC(1,J2)=KD                                                      ETMD0755
      TQC(3,J2)=INTN1                                                   ETMD0756
      IF(NXZ(5,1).NE.MET) TQC(2,J2)=0                                   ETMD0757
      GOTO 6                                                            ETMD0758
    1 IF(TPD(8).EQ.A2.OR.TPD(8).EQ.A8) GOTO 9                           ETMD0759
      IF(TPD(8).EQ.A3) GOTO 10                                          ETMD0760
      IF(TQC(3,J2).EQ.0) JIK=15                                         ETMD0761
      IF(TQC(3,J2).EQ.0.AND.TPD(8).EQ.A1) GOTO 1111                     ETMD0762
      IF(TQC(3,J2).EQ.0) GOTO 135                                       ETMD0763
      IF(TPD(5).EQ.MET) GOTO 135                                        ETMD0764
      N=A(10,1)                                                         ETMD0765
      N=S(1,N)                                                          ETMD0766
      N=A(12,N)+A(13,N)-1                                               ETMD0767
      N=TQ(1,N)                                                         ETMD0768
      N=U(1,N)                                                          ETMD0769
      IF(NZY.NE.0) GOTO 94                                              ETMD0770
      NZY=N*10000                                                       ETMD0771
   94 CONTINUE                                                          ETMD0772
      J11=TQC(1,J2)+LP                                                  ETMD0773
      IF(J11.LE.0) IDAT=9999                                            ETMD0774
      IF(J11.LE.0) GOTO 222                                             ETMD0775
      IDAT=KDP(J11)/10                                                  ETMD0776
  222 INTN=TQC(3,J2)+32000                                              ETMD0777
      WRITE(ND1,4) (TPD(M),M=9,10),TPD(8),NZY,IDAT,(TPD(M),M=1,4),IPR   ETMD0778
     *,INTN,TPD(11),TPD(12),H15,H11                                     ETMD0779
      NI=NI+1                                                           ETMD0780
      IPRCP=1                                                           ETMD0781
      GOTO 6                                                            ETMD0782
    9 DO 300 IK=1,NW                                                    ETMD0783
      IF(UW(1,IK).EQ.NZY) GOTO 301                                      ETMD0784
  300 CONTINUE                                                          ETMD0785
      GOTO 17                                                           ETMD0786
  301 INTN=INTN-32000                                                   ETMD0787
      U(5,UW(2,IK))=U(5,UW(2,IK))+TPD(13)/100                           ETMD0788
      U(6,UW(2,IK))=U(6,UW(2,IK))+(TPD(13)-TPD(13)/100*100)             ETMD0789
      INZ=NZY/10000                                                     ETMD0790
      INM=NZY/100-INZ*100                                               ETMD0791
      LTR=TPD(13)-(TPD(13)*U5(UW(2,IK)))/100                            ETMD0792
      LRS=JJJ-(JJJ*U5(UW(2,IK)))/100                                    ETMD0793
      WRITE(N3,21) INZ,INM,ITN,TPD(13),JJJ,(TPD(K),K=1,4),TPD(          ETMD0794
     *8),TPD(11),TPD(12),(TPD(M),M=9,10),U(8,UW(2,IK)),LTR,LRS,LHTR,LHRSETMD0795
     1,LHTRS,LHRSS                                                      ETMD0796
      GOTO 6                                                            ETMD0797
    8 IF(TPD(8).EQ.A1) GOTO 1111                                        ETMD0798
      IF(TPD(8).EQ.A3) GOTO 10                                          ETMD0799
      WRITE(ND1,4) (TPD(M),M=9,10),TPD(8),NZY,TPD(14),(TPD(M),M=1,4),   ETMD0800
     *IPR,INTN,TPD(11),TPD(12),H14,H11                                  ETMD0801
      NI=NI+1                                                           ETMD0802
      IPRCP=1                                                           ETMD0803
      GOTO 6                                                            ETMD0804
 1111 IF(NXZ(5,1).EQ.MET) IDAT=TPD(14)                                  ETMD0805
      IF(NXZ(5,1).EQ.MET) GOTO 1112                                     ETMD0806
      IF(QZ(1,J2).GE.TPD(13)) GOTO 11                                   ETMD0807
      J11=TQC(1,J2)+LP                                                  ETMD0808
      IF(J11.LE.0) IDAT=9999                                            ETMD0809
      IF(J11.LE.0) GOTO 1113                                            ETMD0810
      IDAT=KDP(J11)/10                                                  ETMD0811
 1113 INTN=TQC(3,J2)+32000                                              ETMD0812
 1112 WRITE(ND1,4) (TPD(M),M=9,10),TPD(8),NZY,IDAT,(TPD(M),M=1,4),      ETMD0813
     *IPR,INTN,TPD(11),TPD(12),H20,H11                                  ETMD0814
      NI=NI+1                                                           ETMD0815
      IPRCP=1                                                           ETMD0816
      GOTO 6                                                            ETMD0817
   11 IF(NZY.NE.0) GOTO 8014                                            ETMD0818
      NZY=U(1,TQ(1,J2))*10000+TQ(2,J2)                                  ETMD0819
8014  IF(IDT.LT.NKP(1)-2.AND.NZY-NZY/100*100.EQ.83.AND.NZY/10000.EQ.15)    ETMD0820
     *GOTO 1112                                                         ETMD0821
      IF(IDT.LT.NKP(1)-31.AND.NZY-NZY/100*100.EQ.83.AND.NZY/10000.EQ.10)   ETMD0822
     *GOTO 1112                                                         ETMD0823
      DO 302 IK=1,NW                                                    ETMD0824
      IF(UW(1,IK).EQ.NZY) GOTO 303                                      ETMD0825
  302 CONTINUE                                                          ETMD0826
      GOTO 17                                                           ETMD0827
  303 U(5,UW(2,IK))=U(5,UW(2,IK))+TPD(13)/100                           ETMD0828
      U(6,UW(2,IK))=U(6,UW(2,IK))+(TPD(13)-TPD(13)/100*100)             ETMD0829
  501 CONTINUE                                                          ETMD0830
      IF(NXZ(10,1).EQ.METZ) GOTO 5501                                   ETMD0831
      QZ(2,J2)=QZ(2,J2)-JJJ                                             ETMD0832
 5501 KQZ=QZ(1,J2)                                                      ETMD0833
      QZ(1,J2)=QZ(1,J2)-TPD(13)                                         ETMD0834
      IF(QZ(2,J2).LE.0) QZ(2,J2)=0                                      ETMD0835
      IF(QZ(1,J2).LE.0) QZ(1,J2)=0                                      ETMD0836
      TQC(3,J2)=INTN1                                                   ETMD0837
      IF(JIK.EQ.15) TQC(3,J2)=0                                         ETMD0838
      TQC(1,J2)=KD                                                      ETMD0839
      INZ=NZY/10000                                                     ETMD0840
      INM=NZY/100-INZ*100                                               ETMD0841
 4016 CONTINUE                                                          ETMD0842
      IF(TQ(4,J2).GT.NKP(1)) GOTO 3005                                     ETMD0843
      IF(TQ(4,J2).LE.NKP(1).AND.TQ(5,J2).GT.NKP(1)) GOTO 402                  ETMD0844
      IF(TPD(13).LE.KQZ) GOTO 422                                       ETMD0845
      LHTR=KQZ                                                          ETMD0846
      LHRS=JJJ                                                          ETMD0847
      IF(TPD(13).NE.0) LHRS=JJJ*KQZ/TPD(13)                             ETMD0848
      GOTO 3005                                                         ETMD0849
  422 LHTR=TPD(13)                                                      ETMD0850
      LHRS=JJJ                                                          ETMD0851
      GOTO 3005                                                         ETMD0852
  402 IU2=U(2,TQ(1,J2))                                                 ETMD0853
      IF(IP.EQ.0.AND.IU2.LE.820) IU2=IU2*2                              ETMD0854
      KQZ=(NKP(1)-TQ(4,J2)+1)*IU2                                          ETMD0855
      IF(KQZ.GT.TPD(13)) LHTR=TPD(13)                                   ETMD0856
      IF(KQZ.GT.TPD(13)) LHRS=JJJ                                       ETMD0857
      IF(KQZ.LE.TPD(13)) LHTR=KQZ                                       ETMD0858
      LHRS=JJJ                                                          ETMD0859
      IF(TPD(13).NE.0.AND.KQZ.LE.TPD(13)) LHRS=JJJ*KQZ/TPD(13)          ETMD0860
 3005 LTR=TPD(13)-(TPD(13)*U5(UW(2,IK)))/100                            ETMD0861
      LRS=JJJ-(JJJ*U5(UW(2,IK)))/100                                    ETMD0862
      LHTRS=LHTR-(LHTR*U5(UW(2,IK)))/100                                ETMD0863
      LHRSS=LHRS-(LHRS*U5(UW(2,IK)))/100                                ETMD0864
      WRITE(N3,21) INZ,INM,ITN,TPD(13),JJJ,(TPD(K),K=1,4),TPD(8),TPD(11)ETMD0865
     *,TPD(12),(TPD(M),M=9,10),U(8,UW(2,IK)),LTR,LRS,LHTR,LHRS,LHTRS,   ETMD0866
     1LHRSS                                                             ETMD0867
      IF(JIK.EQ.15) GOTO 6                                              ETMD0868
      U(11,TQ(1,J2))=U(11,TQ(1,J2))+LHTR/100                            ETMD0869
      U(12,TQ(1,J2))=U(12,TQ(1,J2))+(LHTR-LHTR/100*100)                 ETMD0870
      GOTO 6                                                            ETMD0871
   10 DO 304 IK=1,NW                                                    ETMD0872
      IF(UW(1,IK).EQ.NZY) GOTO 305                                      ETMD0873
  304 CONTINUE                                                          ETMD0874
      GOTO 17                                                           ETMD0875
  305 INZ=NZY/10000                                                     ETMD0876
      INM=NZY/100-INZ*100                                               ETMD0877
  400 U(5,UW(2,IK))=U(5,UW(2,IK))-TPD(13)/100                           ETMD0878
      U(6,UW(2,IK))=U(6,UW(2,IK))-(TPD(13)-TPD(13)/100*100)             ETMD0879
      IF(TQ(4,J2).GT.NKP(1)) GOTO 401                                      ETMD0880
      IF(TQ(5,J2).LE.NKP(1)) GOTO 404                                      ETMD0881
      IF(NXZ(5,1).EQ.MET) GOTO 404                                      ETMD0882
      IU2=U(2,TQ(1,J2))                                                 ETMD0883
      IF(IP.EQ.0.AND.IU2.LE.820) IU2=IU2*2                              ETMD0884
      KQZ=(NKP(1)-TQ(4,J2)+1)*IU2                                          ETMD0885
      IF(KQZ.GT.TPD(13)) LHTR=TPD(13)                                   ETMD0886
      IF(KQZ.GT.TPD(13)) LHRS=JJJ                                       ETMD0887
      IF(KQZ.LE.TPD(13)) LHTR=KQZ                                       ETMD0888
      LHRS=JJJ                                                          ETMD0889
      IF(TPD(13).NE.0.AND.KQZ.LE.TPD(13)) LHRS=JJJ*KQZ/TPD(13)          ETMD0890
      GOTO 401                                                          ETMD0891
  404 LHTR=TPD(13)                                                      ETMD0892
      LHRS=JJJ                                                          ETMD0893
      IF(NXZ(5,1).EQ.MET) GOTO 4015                                     ETMD0894
  401 IF(TQC(2,J2).NE.0.AND.TQC(3,J2).NE.0) QZ(1,J2)=QZ(1,J2)+TPD(13)   ETMD0895
      IF(TQC(2,J2).NE.0.AND.TQC(3,J2).NE.0) QZ(2,J2)=QZ(2,J2)+JJJ       ETMD0896
      IF(TQC(2,J2).EQ.0) TQC(1,J2)=-1000                                ETMD0897
      IF(TQC(2,J2).EQ.0) TQC(3,J2)=0                                    ETMD0898
      IF(TQC(2,J2).EQ.0) TQC(2,J2)=1                                    ETMD0899
      GOTO 3006                                                         ETMD0900
 4015 IF(QZ(3,J2).LE.TPD(13)) LHTR=QZ(3,J2)                             ETMD0901
      LHRS=JJJ                                                          ETMD0902
      IF(TPD(13).NE.0.AND.QZ(3,J2).LE.TPD(13)) LHRS=(JJJ*QZ(3,J2))/     ETMD0903
     *TPD(13)                                                           ETMD0904
      IF(QZ(3,J2).LE.TPD(13)) QZ(3,J2)=0                                ETMD0905
      IF(QZ(3,J2).LE.TPD(13)) GOTO 3006                                 ETMD0906
      QZ(3,J2)=QZ(3,J2)-TPD(13)                                         ETMD0907
 3006 LTR=TPD(13)-(TPD(13)*U5(UW(2,IK)))/100                            ETMD0908
      LRS=JJJ-(JJJ*U5(UW(2,IK)))/100                                    ETMD0909
      LHTRS=LHTR-(LHTR*U5(UW(2,IK)))/100                                ETMD0910
      LHRSS=LHRS-(LHRS*U5(UW(2,IK)))/100                                ETMD0911
      U(11,TQ(1,J2))=U(11,TQ(1,J2))-LHTR/100                            ETMD0912
      U(12,TQ(1,J2))=U(12,TQ(1,J2))-(LHTR-LHTR/100*100)                 ETMD0913
      WRITE(N3,21) INZ,INM,ITN,TPD(13),JJJ,(TPD(K),K=1,4),TPD(          ETMD0914
     *8),TPD(11),TPD(12),(TPD(M),M=9,10),U(8,UW(2,IK)),LTR,LRS,LHTR,    ETMD0915
     1LHRS,LHTRS,LHRSS                                                  ETMD0916
      GOTO 6                                                            ETMD0917
   26 IF(TPD(13).EQ.0) GOTO 4010                                        ETMD0918
      U(5,UW(2,IK))=U(5,UW(2,IK))+TPD(13)/100                           ETMD0919
      U(6,UW(2,IK))=U(6,UW(2,IK))+(TPD(13)-TPD(13)/100*100)             ETMD0920
      GOTO 4011                                                         ETMD0921
 4010 U(5,UW(2,IK))=U(5,UW(2,IK))+QZ(1,J2)/100                          ETMD0922
      U(6,UW(2,IK))=U(6,UW(2,IK))+(QZ(1,J2)-QZ(1,J2)/100*100)           ETMD0923
 4011 CONTINUE                                                          ETMD0924
      INZ=NZY/10000                                                     ETMD0925
      INM=NZY/100-INZ*100                                               ETMD0926
      IWP=U5(UW(2,IK))                                                  ETMD0927
      IWR=U(8,UW(2,IK))                                                 ETMD0928
      IF(TPD(13).EQ.0) GOTO 3004                                        ETMD0929
      IF(TPD(13).EQ.1) TPD(13)=0                                        ETMD0930
  27  IF(NXZ(5,1).EQ.MET) GOTO 2700                                     ETMD0931
      GOTO 270                                                          ETMD0932
 2700 KQZ3=TPD(13)                                                      ETMD0933
      IF(QZ(3,J2)+KQZ3.GT.QZ(1,J2)) GOTO 2701                           ETMD0934
      QZ(3,J2)=QZ(3,J2)+KQZ3                                            ETMD0935
      LHTR=TPD(13)                                                      ETMD0936
      LHRS=JJJ                                                          ETMD0937
      GOTO 270                                                          ETMD0938
 2701 KQZ3=QZ(1,J2)-QZ(3,J2)                                            ETMD0939
      QZ(3,J2)=QZ(3,J2)+TPD(13)                                         ETMD0940
      LHTR=KQZ3                                                         ETMD0941
      LHRS=JJJ                                                          ETMD0942
      IF(TPD(13).NE.0) LHRS=JJJ*LHTR/TPD(13)                            ETMD0943
      IF(LHTR.LE.0) LHTR=0                                              ETMD0944
      IF(LHRS.LE.0) LHRS=0                                              ETMD0945
      GOTO 3010                                                         ETMD0946
  270 IF(TQ(4,J2).GT.NKP(1)) LHTR=0                                        ETMD0947
      IF(TQ(4,J2).GT.NKP(1)) LHRS=0                                        ETMD0948
      IF(TQ(4,J2).GT.NKP(1)) GOTO 3010                                     ETMD0949
      IF(JIK.EQ.15.AND.TQC(1,J2).GT.1.AND.TQ(4,J2).LE.NKP(1)) GOTO 403     ETMD0950
      IF(JIK.EQ.15) LHTR=0                                              ETMD0951
      IF(JIK.EQ.15) LHRS=0                                              ETMD0952
      IF(JIK.EQ.15) GOTO 3010                                           ETMD0953
  403 CONTINUE                                                          ETMD0954
      IF(TQ(4,J2).LE.NKP(1).AND.TQ(5,J2).GT.NKP(1))                           ETMD0955
     *GOTO 600                                                          ETMD0956
      LHTR=TPD(13)                                                      ETMD0957
      LHRS=JJJ                                                          ETMD0958
      GOTO 3010                                                         ETMD0959
  600 IU2=U(2,TQ(1,J2))                                                 ETMD0960
      IF(IP.EQ.0.AND.IU2.LE.820) IU2=IU2*2                              ETMD0961
      KQZ3=(NKP(1)-TQ(4,J2)+1)*IU2                                         ETMD0962
      IF(TPD(13).GT.KQZ3) LHTR=KQZ3                                     ETMD0963
      LHRS=JJJ                                                          ETMD0964
      IF(TPD(13).NE.0.AND.TPD(13).GT.KQZ3) LHRS=JJJ*LHTR/TPD(13)        ETMD0965
      IF(TPD(13).LE.KQZ3) LHTR=TPD(13)                                  ETMD0966
      IF(TPD(13).LE.KQZ3) LHRS=JJJ                                      ETMD0967
 3010 U(11,TQ(1,J2))=U(11,TQ(1,J2))+LHTR/100                            ETMD0968
      U(12,TQ(1,J2))=U(12,TQ(1,J2))+(LHTR-LHTR/100*100)                 ETMD0969
      LTR=TPD(13)-(TPD(13)*U5(UW(2,IK)))/100                            ETMD0970
      LRS=JJJ-(JJJ*U5(UW(2,IK)))/100                                    ETMD0971
      LHTRS=LHTR-(LHTR*U5(UW(2,IK)))/100                                ETMD0972
      LHRSS=LHRS-(LHRS*U5(UW(2,IK)))/100                                ETMD0973
      WRITE(N3,21) INZ,INM,ITN,TPD(13),JJJ,(TPD(K),K=1,4),TPD(8),       ETMD0974
     *TPD(11),TPD(12),(TPD(M),M=9,10),U(8,UW(2,IK)),LTR,LRS,LHTR,LHRS,  ETMD0975
     1LHTRS,LHRSS                                                       ETMD0976
      IF(NXZ(5,1).NE.MET) TQC(2,J2)=0                                   ETMD0977
      TQC(3,J2)=INTN1                                                   ETMD0978
      TQC(1,J2)=KD                                                      ETMD0979
      GOTO 6                                                            ETMD0980
    2 N=A(10,1)                                                         ETMD0981
      N=S(1,N)                                                          ETMD0982
      N=A(12,N)+A(13,N)-1                                               ETMD0983
      N=TQ(1,N)                                                         ETMD0984
      N=U(1,N)                                                          ETMD0985
      IF(NZY.EQ.0) GOTO 93                                              ETMD0986
      WRITE(ND1,4) (TPD(M),M=9,10),TPD(8),NZY,TPD(14),(TPD(M),M=1,4),   ETMD0987
     *IPR,INTN,TPD(11),TPD(12),H5,H11                                   ETMD0988
      NI=NI+1                                                           ETMD0989
      IPRCP=1                                                           ETMD0990
      GOTO 6                                                            ETMD0991
  93  NZY=N*10000                                                       ETMD0992
   91 WRITE(ND1,4) (TPD(M),M=9,10),TPD(8),NZY,TPD(14),(TPD(M),M=1,4),   ETMD0993
     *IPR,INTN,TPD(11),TPD(12),H6,H11                                   ETMD0994
      NI=NI+1                                                           ETMD0995
      GO TO 6                                                           ETMD0996
    7 CONTINUE                                                          ETMD0997
      N=A(10,1)                                                         ETMD0998
      N=S(1,N)                                                          ETMD0999
      N=A(12,N)+A(13,N)-1                                               ETMD1000
      N=TQ(1,N)                                                         ETMD1001
      N=U(1,N)                                                          ETMD1002
      IF(NZY.EQ.0) GOTO 92                                              ETMD1003
      WRITE(ND1,4) (TPD(M),M=9,10),TPD(8),NZY,TPD(14),(TPD(M),M=1,4),   ETMD1004
     *IPR,INTN,TPD(11),TPD(12),H3,H11                                   ETMD1005
      NI=NI+1                                                           ETMD1006
      IPRCP=1                                                           ETMD1007
      GOTO 6                                                            ETMD1008
  92  NZY=N*10000                                                       ETMD1009
   90 WRITE(ND1,4) (TPD(M),M=9,10),TPD(8),NZY,TPD(14),(TPD(M),M=1,4),   ETMD1010
     *IPR,INTN,TPD(11),TPD(12),H4,H11                                   ETMD1011
      NI=NI+1                                                           ETMD1012
      GO TO 6                                                           ETMD1013
   17 WRITE(ND1,4) (TPD(M),M=9,10),TPD(8),NZY,TPD(14),(TPD(M),M=1,4),   ETMD1014
     *IPR,INTN,TPD(11),TPD(12),H7,H11                                   ETMD1015
      NI=NI+1                                                           ETMD1016
      IPRCP=1                                                           ETMD1017
    6 CONTINUE                                                          ETMD1018
      IPRCP=0                                                           ETMD1019
    3 CONTINUE                                                          ETMD1020
   15 RETURN                                                            ETMD1021
      END                                                               ETMD1022
      SUBROUTINE PEREPS(N1,N2,NU,U,LP,KDP,NXZ,IW,A,S,P,TQ,QZ,TQC,       ETMD1023
     *N0,M0,I0,BETA,IND,NW,UW,NI,U5,IDT)                                ETMD1024
      INTEGER*2 A(26,1),S(2,1),P(2,1),TQ(6,1),TQC(4,1),NXZ(10,4),       ETMD1025
     1U(20,1),KDP(1),I0(13,N0)                                          ETMD1026
      INTEGER QZ(4,1),IW(NU),UW(2,200)                                  ETMD1027
      INTEGER*2 NKP(1)                                                     ETMD1028
      INTEGER*2 KDPG(500),U5(NU)                                        ETMD1029
      IND1=1                                                            ETMD1030
      READ(N1) LP,NN,NK                                                 ETMD1031
      LQ=LP+LP                                                          ETMD1032
      READ(N1) (KDP(I),I=1,LQ)                                          ETMD1033
      CALL KALDNI(NK,0,NKP)                                             ETMD1034
      DO 900 I4=LP,LQ                                                   ETMD1035
      IF(KDP(I4).EQ.NKP(1)) GOTO 901                                       ETMD1036
  900 CONTINUE                                                          ETMD1037
  901 NKP=I4-LP                                                         ETMD1038
      CALL KALDNI(NN,250,KDPG)                                          ETMD1039
      IF(IND.LT.0) GO TO 1                                              ETMD1040
      WRITE (N2) LP,NN,NK                                               ETMD1041
      WRITE (N2) (KDP(I),I=1,LQ)                                        ETMD1042
    1 READ(N1) NXZ,IW,IQ,IP,ID,LD,LZ,LF,MP,MS,NA,NS,NT                  ETMD1043
      IF(NA.LT.0) GO TO 2                                               ETMD1044
      READ (N1) ((A(I,J),I=1,26),J=1,NA)                                ETMD1045
      READ (N1) ((S(I,J),I=1,2),J=1,NS),((P(I,J),I=1,2),J=1,NS)         ETMD1046
      READ (N1) ((TQ(I,J),I=1,6),J=1,NT),((QZ(I,J),I=1,4),J=1,NT)       ETMD1047
      READ (N1) ((TQC(I,J),I=1,4),J=1,NT)                               ETMD1048
      IF(IND.NE.0) CALL BETA(NU,U,LP,KDP,NXZ,IW,IP,LZ,LF,MP,MS,NA,NS,   ETMD1049
     1NT,A,S,P,TQ,QZ,TQC,N0,M0,I0,IND1,NW,UW,NI,NKP,KDPG,U5,IDT)        ETMD1050
      IF(IND.LT.0.OR.IND1.LT.0) GO TO 1                                 ETMD1051
      WRITE (N2) NXZ,IW,IQ,IP,ID,LD,LZ,LF,MP,MS,NA,NS,NT                ETMD1052
      WRITE (N2) ((A(I,J),I=1,26),J=1,NA)                               ETMD1053
      WRITE (N2) ((S(I,J) ,I=1,2),J=1,NS),((P(I,J),I=1,2),J=1,NS)       ETMD1054
      WRITE (N2) ((TQ(I,J),I=1,6),J=1,NT),((QZ(I,J),I=1,4),J=1,NT)      ETMD1055
      WRITE (N2) ((TQC(I,J),I=1,4),J=1,NT)                              ETMD1056
      GO TO 1                                                           ETMD1057
    2 NA=-1000                                                          ETMD1058
      IF(IND.LT.0) GO TO 3                                              ETMD1059
      WRITE (N2) NXZ,IW,IQ,IP,ID,LD,LZ,LF,MP,MS,NA,NS,NT                ETMD1060
    3 RETURN                                                            ETMD1061
      END                                                               ETMD1062
 
