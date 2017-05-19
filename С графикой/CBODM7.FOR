      INCLUDE  'FGRAPH.FI'
      INCLUDE  'FGRAPH.FD'
      REAL*8 RASZ,SDEN,IDEN2                                            BODM0006
      INTEGER XIFR(2000)                                                BODM0007
      INTEGER*2      NOMZ(4),XVOST2,NOMA,A17(10)                                     BODM0008
      INTEGER CZAK,NUL/'0000'/                                          BODM0009
      LOGICAL*1 ZAKK(8),XVOST(2),DD(2),edin/'1'/,dva/'2'/,nol/'0'/      BODM0010
      EQUIVALENCE(NOMZ(1),ZAKK(1)),(XVOST(1),XVOST2),(CZAK,NXZ(8,4)),   BODM0011
     *(DD,NXZ(10,2))                                                    BODM0012
      INTEGER*2 FU(10)/'0 ','1 ','2 ','3 ','4 ','5 ','6 ','7 ','8 ','9 'BODM0013
     */                                                                 BODM0014
      INTEGER*2 MIN(500),MB/'С '/,PROB/'  '/,SPEZ/'С '/                 BODM0015
      LOGICAL*1 D/' '/,CONST/'D'/,CONST1/'C'/,CONST2/'B'/,CONST3/'E'/,  BODM0016
     *CONST4/'F'/,CONST5/'K'/,CONST6/'A'/,CONST7/'P'/                   BODM0017
      INTEGER NTF,NTP,NOMTF,TTF                                         BODM0018
      INTEGER*2 NM,NZ                                                   BODM0019
      INTEGER KOT(13)/13*0/,BOT(6),KT/0/                                BODM0020
      INTEGER KON/'CO  '/                                               BODM0021
      INTEGER*2 A(26,200),S(2,200),P(2,200),TQ(6,500),TQC(4,500),NXZ(10,BODM0022
     *4),KDP(180),U(20,100),NIN(5,500),DET(8,500),IZ(500),KOL(500),KFT  BODM0023
      INTEGER*2 KLAS(2,500)                                             BODM0024
      INTEGER F(100,3)                                                  BODM0025
      INTEGER*2 ZG(100,60)                                              BODM0026
      INTEGER QZ(4,500),IW(100),UW(2,200),DA(500)                       BODM0027
      INTEGER*2 NPR(500),PR,PRI                                         BODM0028
      INTEGER NAC(500)                                                  BODM0029
      INTEGER*2 C/'FF'/,KOH1/'CO'/                                      BODM0030
      INTEGER*2 ZAK(4,500)/2000*'FF'/                                   BODM0031
      INTEGER*2 CP/'СП'/                                                BODM0032
      INTEGER*2 MET/'1 '/,KP0/' 0'/,KP1/' 1'/,KP2/' 2'/,METZ/'2 '/      BODM0033
      INTEGER*2 GR(1100),KLASIN(2,1100),PROZ(1100),PROZS(1100)          BODM0034
      character*5 zakzao/'20460'/
      character*5 zakaz
      equivalence(nxz(1,1),zakaz)
      LOGICAL*1 OBOZ(20)                                                RZOC0948
      character*1 zakn(2),an/'А'/
      EQUIVALENCE (NOMA,ZAKn(1)),(A17,OBOZ)
      record / rccoord / curpos
      integer*2 dummy2
      integer*4 dummy4,dummy
      dummy=setvideomode( $ERESCOLOR )
      dummy=remappalette( 0,$BLUE )
      call clearscreen( $GCLEARSCREEN )
  900 FORMAT(3A2,I2,3X,I2)                                              BODM0035
    1 FORMAT(A4,I6,I2,5A2,2A2,I4,4A2,8A2,A2,24X)                        BODM0036
  202 FORMAT(1X,A4,I6,I2,5A2,I4,4A2,8A2)                                BODM0037
  150 FORMAT(I8,64X,I9)                                                 BODM0038
      open(4,file='f:\asyip\u.dat',form='unformatted')
      open(unit=1,file='f:\asyipk\plan.dat',form='unformatted')
      open(unit=2,file='f:\asyipk\portfel.dat',form='unformatted')
      open(unit=3,file='f:\asyip\plan.dat',form='unformatted')
      open(unit=8,file='f:\asyip\portfel.dat',form='unformatted')
      open(9,file='f:\asyip\zagz.dat')
      open(10,file='f:\asyip\cmatin.dat',RECL=81,access='direct',
     *form='formatted')
      open(12,file='f:\asyip\vipz.dat')
      open(11,file='f:\asyip\dtb.dat')
      open(13,file='f:\asyip\mnfz.dat')
      open(14,file='f:\asyip\klasin.dat')
      dummy2=settextcolor(2)
      call settextposition( 10,1, curpos ) 
      call outtext( 'задай вид месяца:текущий(до расчета плана)-1,предыд
     1ущий(после расчета плана)-0' )
      read(*,777) nn
  777 format(i1)
      DO 151 J=1,2000                                                   BODM0039
      READ(10,150,rec=j) XIFR(J),I2                                     BODM0040
      IF(I2.GT.999990000) GOTO 152                                      BODM0041
  151 CONTINUE                                                          BODM0042
  152 KOLM=J-1                                                          BODM0043
      DO 901 J=1,1100                                                   BODM0044
      READ(14,900) GR(J),(KLASIN(I,J),I=1,2),PROZ(J),PROZS(J)           BODM0045
      IF(GR(J).EQ.KOH1) GOTO 902                                        BODM0046
  901 CONTINUE                                                          BODM0047
  902 KKLAS=J-1                                                         BODM0048
      READ(4) NW,NU                                                     BODM0049
      READ(4) ((UW(I,J),I=1,2),J=1,NW),((U(I,J),I=1,20),J=1,NU)         BODM0050
      REWIND 4                                                          BODM0051
      DO 945 I=1,NU                                                     BODM0052
      IF(U(7,I).EQ.0) GOTO 945                                          BODM0053
      IF(U(8,I).EQ.6) NZAG1=I                                           BODM0054
      IF(U(8,I).EQ.13) NZAG5=I                                          BODM0055
  945 CONTINUE                                                          BODM0056
      LU=1                                                              BODM0057
      LI=1                                                              BODM0058
      LK=1                                                              BODM0059
      LB=1                                                              BODM0060
      LA=1                                                              BODM0061
      NUC=0                                                             BODM0062
      DO 90 I=1,NU                                                      BODM0063
      IF(U(7,I).LE.0) GOTO 90                                           BODM0064
      NUC=NUC+1                                                         BODM0065
   90 CONTINUE                                                          BODM0066
      PR=1                                                              BODM0067
      KOEF=0                                                            BODM0068
      NTP=0                                                             BODM0069
      KPL=0                                                             BODM0070
      NTF=0                                                             BODM0071
      NOMTF=0                                                           BODM0072
      TTF=0                                                             BODM0073
      RASZ=0.0                                                          BODM0074
      TRF=0                                                             BODM0075
      CZAK=NUL                                                          BODM0076
      IF(NN.EQ.0) ND=1
      IF(NN.EQ.1) ND=3
C      ND=1                                                              BODM0077
      READ(ND) LP,N,NK                                                  BODM0078
      LP1=LP*2                                                          BODM0079
      READ(ND) (KDP(I),I=1,LP1)                                         BODM0080
      IDAT2=NK                                                          BODM0081
      IDATA=IDAT2/10000                                                   BODM0082
      IDAT2=IDAT2-IDATA*10000                                             BODM0083
      IDAT=IDATA/100*100                                                BODM0084
      IDAT1=IDAT/100                                                    BODM0085
      IDATA=IDATA-IDAT                                                  BODM0086
      CALL KALDNI(NK,0,KDP1)                                            BODM0087
      DO 700 K=LP,LP1                                                   BODM0088
      IF(KDP(K).EQ.KDP1) GOTO 401                                       BODM0089
  700 CONTINUE                                                          BODM0090
  401 NK=K-LP                                                           BODM0091
   21 IF(ND.NE.1.AND.ND.NE.3) GOTO 3                                    BODM0092
      READ(ND) NXZ,(IW(I),I=1,NU),IQ,IP,ID,LD,LZ,LF,MP,MS,NA,NS,NT      BODM0093
      IF(NA.LT.0) GOTO 4                                                BODM0094
      GOTO 5                                                            BODM0095
    3 READ(ND) NXZ,(IW(I),I=1,NU),IQ,IP,ID,LZ,MP,NA,NS,NT               BODM0096
      IF(NA.LT.0) GOTO 6                                                BODM0097
    5 READ(ND) ((A(I,J),I=1,26),J=1,NA)                                 BODM0098
      READ(ND) ((S(I,J),I=1,2),J=1,NS),((P(I,J),I=1,2),J=1,NS)          BODM0099
      READ(ND) ((TQ(I,J),I=1,6),J=1,NT),((QZ(I,J),I=1,4),J=1,NT)        BODM0100
      READ(ND) ((TQC(I,J),I=1,4),J=1,NT)                                BODM0101
      IF(IQ.EQ.0) GOTO 158                                              BODM0102
      DO 2525 K=2,NA                                                      RZOC1013
      NOMA=A(17,K)                                                      RZOC1014
C     ZAK(2)=ZAK(1)                                                     RZOC1015
      if(zakn(1).eq.an) goto 2525
      P(2,a(14,k))=P(2,a(14,k))*(IQ-QZ(2,1))/IQ
 2525 continue
C      DO 157 J=1,NS                                                     BODM0103
C  157 P(2,J)=P(2,J)/IQ*(IQ-QZ(2,1))                                     BODM0104
  158 CONTINUE                                                          BODM0105
      DO 154 I=1,4                                                      BODM0106
  154 NOMZ(I)=NXZ(I,1)                                                  BODM0107
      GOTO 155                                                          BODM0108
      XVOST2=PROB                                                       BODM0109
      XVOST(2)=ZAKK(8)                                                  BODM0110
      IF(XVOST2.NE.PROB) GOTO 155                                       BODM0111
      DO 156 J=1,7                                                      BODM0112
  156 ZAKK(9-J)=ZAKK(8-J)                                               BODM0113
      XVOST(2)=ZAKK(2)                                                  BODM0114
      NOMZ(1)=PROB                                                      BODM0115
      ZAKK(2)=XVOST(2)                                                  BODM0116
  155 CONTINUE                                                          BODM0117
      JIK=A(10,1)                                                       BODM0118
      JIK=S(1,JIK)                                                      BODM0119
      JIK=A(12,JIK)+A(13,JIK)-1                                         BODM0120
      NM=TQ(2,JIK)/100                                                  BODM0121
      JIK=TQ(1,JIK)                                                     BODM0122
      NZ=U(1,JIK)                                                       BODM0123
      MP1=MP/10000                                                        BODM0124
      MP2=MP-MP1*10000                                                    BODM0125
      MP1=MP1-MP1/100*100                                               BODM0126
      IF(TQC(1,1).LT.0) TQC(2,1)=0                                      BODM0127
      IF(ND.NE.1.AND.ND.NE.3) GOTO 153
      IF(LF.GT.NK) GOTO 153                                             BODM0129
      KPL=IQ-QZ(2,1)                                                    BODM0130
      IF(TQC(2,1).NE.0) GOTO 153                                        BODM0131
      WRITE(12,70) CZAK,NZ,NM,NOMZ,(NXZ(I,2),I=1,10),(NXZ(I,3),I=1,10), BODM0132
     1(NXZ(I,4),I=1,5),IQ,IP,NTF,IDATA,IDAT2,KPL,TQC(2,1),TTF,RASZ,     BODM0133
     2KOEF,prob,prob                                                              BODM0134
  153 CONTINUE                                                          BODM0135
   36 CONTINUE                                                          BODM0136
      IF(NXZ(5,1).EQ.CP) GOTO 30                                        BODM0137
      IF(LF.LE.NK.AND.IP.EQ.0.AND.(ND.EQ.1.OR.ND.EQ.3)) NXZ(5,1)=KP0                 BODM0138
      IF(LF.LE.NK.AND.IP.EQ.1.AND.(ND.EQ.1.OR.ND.EQ.3)) NXZ(5,1)=KP1                 BODM0139
      IF(LF.LE.NK.AND.IP.EQ.2.AND.(ND.EQ.1.OR.ND.EQ.3)) NXZ(5,1)=KP2 
      IF(IP.GE.2.or.ZAKAZ.EQ.ZAKZAO) GOTO 30                                               BODM0141
      IF(MP2.NE.IDAT2) GOTO 30                                          BODM0142
      IF(MP1.NE.IDATA) GOTO 30                                          BODM0143
  333 CONTINUE                                                          BODM0144
C      IF(IP.EQ.0) ID1=ID*15                                             BODM0145
C      IF(IP.EQ.1) ID1=ID*13                                             BODM0146
C      NXZ(10,2)=SPEZ                                                    BODM0147
C      ID1=ID1/10                                                        BODM0148
C      GOTO 31                                                           BODM0149
   30 ID1=ID                                                            BODM0150
   31 CONTINUE                                                          BODM0151
   91 IF(ND.NE.1.AND.ND.NE.3) GOTO 52
      IF(MS.EQ.-1) GOTO 800                                             BODM0153
      IF(LF.GT.NK) GOTO 52                                              BODM0154
  800 NTP=ID*10                                                         BODM0155
      IF(IQ.EQ.0) GOTO 101                                              BODM0156
      NTP=NTP*QZ(2,1)/IQ                                                BODM0157
      NTP=NTP/10                                                        BODM0158
      NTP=ID-NTP                                                        BODM0159
      IF(NTP.LE.0) NTP=0                                                BODM0160
      IF(TQC(2,1).EQ.0)   GOTO 53                                       BODM0161
      GOTO 52                                                           BODM0162
  101 NTP=ID                                                            BODM0163
      IF(TQC(2,1).EQ.0) GOTO 53                                         BODM0164
   52 IF(TQC(2,1).EQ.0) GOTO 94                                         BODM0165
      NTF=ID1                                                           BODM0166
      NTFN=ID1                                                          BODM0167
      IF(IQ.EQ.0) GOTO 84                                               BODM0168
      NTF=NTF*TQC(2,1)/IQ                                               BODM0169
   84 DO 54 I=1,NU                                                      BODM0170
      KT=KT+IW(I)                                                       BODM0171
      KOT(U(8,I))=KOT(U(8,I))+IW(I)                                     BODM0172
      IW(I)=IW(I)/10                                                    BODM0173
      TTF=TTF+IW(I)                                                     BODM0174
   54 CONTINUE                                                          BODM0175
      KT=KT*10                                                          BODM0176
      DO 2002 I=1,6                                                     BODM0177
 2002 BOT(I)=(KOT(I)+KOT(I+7))*100                                      BODM0178
      IF(IQ.EQ.0) GOTO 103                                              BODM0179
      TTF=TTF*TQC(2,1)/IQ                                               BODM0180
      KT=KT*TQC(2,1)/IQ                                                 BODM0181
      DO 2003 I=1,6                                                     BODM0182
 2003 BOT(I)=BOT(I)*TQC(2,1)/IQ                                         BODM0183
  103 KT=(KT+5)/10                                                      BODM0184
      DO 2004 I=1,6                                                     BODM0185
 2004 BOT(I)=(BOT(I)+50)/100                                            BODM0186
      IF(ND.NE.1.AND.ND.NE.3) GOTO 802                                              BODM0187
      IF(MS.EQ.-1) GOTO 801                                             BODM0188
      IF(LF.GT.NK) GOTO 802                                             BODM0189
  801 IF(TQC(2,1).LE.IQ-QZ(2,1)) NOMTF=(NTFN*TQC(2,1)*10/IQ+5)/10       BODM0190
      IF(TQC(2,1).GT.IQ-QZ(2,1)) NOMTF=(NTFN*(IQ-QZ(2,1))*10/IQ+5)/10   BODM0191
  802 CALL  PACMAT(A,NA,NXZ,P,XIFR,KOLM,10,RASZ)                        BODM0192
c     IF(RASZ.GE.1000000.00) RASZ=999999.99                             BODM0193
      IDEN=0                                                            BODM0194
      IHAC=0                                                            BODM0195
      DO 500 I=2,NT                                                     BODM0196
      IF(QZ(1,I).EQ.1.OR.QZ(1,I).EQ.0) GOTO 500                         BODM0197
      IF(TQ(1,I).EQ.NU) GOTO 500                                        BODM0198
      IHAC=IHAC+QZ(1,I)                                                 BODM0199
  500 IDEN=IDEN+QZ(2,I)                                                 BODM0200
      IF(NXZ(10,1).EQ.MET) KOEF=IDEN*100/IHAC                           BODM0201
      KOEF=(KOEF*10*TQC(2,1)/IQ+5)/10                                   BODM0202
      IDEN1=IDEN                                                        BODM0203
      IDEN2=IDEN                                                        BODM0204
      IF(IQ.EQ.0) GOTO 501                                              BODM0205
      KOLSH=2147000000/IDEN                                             BODM0206
      IZIKL=TQC(2,1)/KOLSH                                              BODM0207
      TQC21=TQC(2,1)-IZIKL*KOLSH                                        BODM0208
      IDEN=0                                                            BODM0209
      IF(IZIKL.EQ.0) GOTO 1000                                          BODM0210
      DO 1001 I=1,IZIKL                                                 BODM0211
      IDEN=IDEN+IDEN1*KOLSH/IQ                                          BODM0212
 1001 CONTINUE                                                          BODM0213
 1000 IDEN=IDEN+IDEN1*TQC21/IQ                                          BODM0214
  501 CONTINUE                                                          BODM0215
      dd(2)=nol
      IF(NXZ(10,1).NE.MET) GOTO 5022                                    BODM0216
      IF(QZ(2,1).NE.0) GOTO 502                                         BODM0217
      DD(2)=edin                                                        BODM0218
c     IF(IDEN.GE.10000000) DD(2)=CONST6                                 BODM0219
c     IF(IDEN.GE.10000000) GOTO 556                                     BODM0220
c     IF(IDEN.GE.1000000) DD(2)=CONST2                                  BODM0221
c     IF(IDEN.GE.1000000) GOTO 555                                      BODM0222
c     IF(IDEN.GE.100000) DD(2)=CONST1                                   BODM0223
c     IF(IDEN.GE.100000) IDEN=(IDEN+5)/10                               BODM0224
      GOTO 502                                                          BODM0225
 5022 IF(NXZ(10,1).NE.METZ) GOTO 502                                    BODM0226
      DD(2)=dva                                                         BODM0227
c     IF(IDEN.GE.10000000) DD(2)=CONST7                                 BODM0228
c     IF(IDEN.GE.10000000) GOTO 556                                     BODM0229
c     IF(IDEN.GE.1000000) DD(2)=CONST5                                  BODM0230
c     IF(IDEN.GE.1000000) GOTO 555                                      BODM0231
c     IF(IDEN.GE.100000) DD(2)=CONST4                                   BODM0232
c     IF(IDEN.GE.100000) IDEN=(IDEN+5)/10                               BODM0233
c     GOTO 502                                                          BODM0234
c 555 IDEN=(IDEN+50)/100                                                BODM0235
c     GOTO 502                                                          BODM0236
c 556 IDEN=(IDEN+50)/1000                                               BODM0237
c     GOTO 502                                                          BODM0238
  502 WRITE(12,70) CZAK,NZ,NM,NOMZ,(NXZ(I,2),I=1,10),(NXZ(I,3),I=1,10), BODM0239
     1(NXZ(I,4),I=1,5),IDEN,IP,NTF,IDATA,IDAT2,KPL,TQC(2,1),TTF,RASZ,   BODM0240
     2KOEF,nxz(6,4),nxz(7,4)                                                              BODM0241
   70 FORMAT(A4,2I2,29A2,I8,I1,I5,I2,i4,3I5,F9.0,I4,2a2)                      BODM0242
 7777 FORMAT(A4,2I2,29A2,I8,I1,I5,I2,i4,3I5,F9.0)                         BODM0243
      WRITE(13,11) NZ,(NXZ(I,1),I=1,4),(NXZ(I,4),I=1,5),KT,(BOT(I),I=1,6BODM0244
     1),NTF,NM,TQC(2,1)                                                 BODM0245
   11 FORMAT(I2,4A2,5A2,I6,6I6,I5,I2,I5)                                BODM0246
  53  IF(NM.EQ.5.OR.NM.EQ.6) NMOB=5                                     BODM0252
      IF(NM.EQ.1) NMOB=1                                                BODM0253
      IF(NM.EQ.2) NMOB=2                                                BODM0254
      IF(NM.EQ.3) NMOB=3                                                BODM0255
      IF(NM.EQ.4) NMOB=4                                                BODM0256
  533 DO 903 J=1,KKLAS                                                  BODM0258
      IF(KLASIN(1,J).NE.NXZ(2,2).OR.KLASIN(2,J).NE.NXZ(3,2)) GOTO 903   BODM0259
      KPROZ=PROZ(J)                                                     BODM0260
      KPROZS=PROZS(J)                                                   BODM0261
      GOTO 904                                                          BODM0263
  903 CONTINUE                                                          BODM0264
      KPROZ=0                                                           BODM0265
      KPROZS=0                                                          BODM0266
  904 NSTF=NTF-NTF*KPROZ/1000                                           BODM0267
      NSOMTF=NOMTF-NOMTF*KPROZ/1000                                     BODM0268
      NTF=NTF*100/(100-KPROZS)                                          BODM0269
      NOMTF=NOMTF*100/(100-KPROZS)                                      BODM0270
      NTP=NTP*100/(100-KPROZS)                                          BODM0271
      WRITE(11,60) NZ,NM,(NXZ(I,1),I=1,4),NXZ(5,1),NTP,NTF,NOMTF,TTF,   BODM0272
     *QZ(2,1),TQC(2,1),NMOB,NSTF,NSOMTF                                 BODM0273
   60 FORMAT(2I2,4A2,A2,6I5,I1,2I5)                                     BODM0274
   94 IF(TQ(2,2)/100.NE.15.AND.TQ(2,NT).NE.101) GOTO 940                BODM0275
      KOEF=0                                                            BODM0276
      NMOB=4                                                            BODM0277
      NTF=0                                                             BODM0278
      NTP=0                                                             BODM0279
      NOMTF=0                                                           BODM0280
      TTF=0                                                             BODM0281
      NSTF=0                                                            BODM0282
      NSOMTF=0                                                          BODM0283
      KPL=0                                                             BODM0284
      CZAK=NUL                                                          BODM0285
      RASZ=0.0                                                          BODM0286
      KFT=0                                                             BODM0287
      nm=5
      IF(TQ(2,2)/100.EQ.5.AND.TQC(2,2).NE.0) GOTO 942                   BODM0290
      IF(TQ(2,2)/100.EQ.5.AND.TQC(1,2).LE.0) GOTO 940                   BODM0291
      IF(TQ(2,2)/100.EQ.5.AND.TQC(1,2).GT.0) GOTO 944                   BODM0292
      IF(TQ(2,NT).EQ.501.AND.TQC(2,NT).NE.0) GOTO 942                   BODM0293
      IF(TQ(2,NT).EQ.501.AND.TQC(1,NT).LE.0) GOTO 940                   BODM0294
944   IF(ND.NE.1.AND.ND.NE.3) GOTO 943                                              BODM0295
      IF(TQ(2,2)/100.EQ.5.AND.TQ(5,2).GE.30) GOTO 943                   BODM0296
      IF(TQ(2,NT).EQ.501.AND.TQ(5,NT).GE.30) GOTO 943                   BODM0297
      IF(IP.EQ.0) NXZ(5,1)=KP0                                          BODM0298
      IF(IP.EQ.1) NXZ(5,1)=KP1                                          BODM0299
      IF(IP.EQ.2) NXZ(5,1)=KP2                                          BODM0300
      NTP=ID1                                                           BODM0301
      NOMTF=ID1                                                         BODM0302
      KPL=IQ-QZ(2,1)                                                    BODM0303
 943  NTF=ID1                                                           BODM0304
      KFT=IQ-QZ(2,1)                                                    BODM0305
      TTF=(IW(NZAG1)+IW(NZAG5)+5)/10                                    BODM0306
      CALL PACMAT(A,NA,NXZ,P,XIFR,KOLM,10,RASZ)                         BODM0307
      GOTO 941                                                          BODM0308
  942 IF(ND.NE.1.AND.ND.NE.3) GOTO 940                                               BODM0309
      NTP=ID1                                                           BODM0310
      IF(TQ(2,2)/100.EQ.5.AND.TQ(5,2).GT.LP.AND.TQ(4,2).LE.LP) NTP=ID1* BODM0311
     1(LP+1-TQ(4,2))/(TQ(5,2)-TQ(4,2)+1)                                BODM0312
      IF(TQ(2,2)/100.EQ.5.AND.TQ(4,2).GT.LP) NTP=0                      BODM0313
      IF(TQ(2,NT).EQ.501.AND.TQ(5,NT).GT.LP.AND.TQ(4,NT).LE.LP) NTP=ID1*BODM0314
     1(LP+1-TQ(4,NT))/(TQ(5,NT)-TQ(4,NT)+1)                             BODM0315
      IF(TQ(2,NT).EQ.501.AND.TQ(4,NT).GT.LP) NTP=0                      BODM0316
      IF(IP.EQ.0) NXZ(5,1)=KP0                                          BODM0317
      IF(IP.EQ.1) NXZ(5,1)=KP1                                          BODM0318
      IF(IP.EQ.2) NXZ(5,1)=KP2                                          BODM0319
      KPL=(IQ-QZ(2,1))*NTP/ID1                                          BODM0320
941   WRITE(9,7777) CZAK,NZ,NM,NOMZ,(NXZ(I,2),I=1,10),(NXZ(I,3),I=1,10),BODM0321
     1(NXZ(I,4),I=1,5),IQ,IP,NTF,IDATA,IDAT2,KPL,KFT,TTF,RASZ           BODM0322
      WRITE(11,60) NZ,NM,(NXZ(I,1),I=1,4),NXZ(5,1),NTP,NTF,NOMTF,TTF,   BODM0323
     1QZ(2,1),TQC(2,1),NMOB,NSTF,NSOMTF                                 BODM0324
  940 PR=1                                                              BODM0325
      NTP=0                                                             BODM0326
      NTF=0                                                             BODM0327
      NOMTF=0                                                           BODM0328
      TTF=0                                                             BODM0329
      KPL=0                                                             BODM0330
      KOEF=0                                                            BODM0331
      CZAK=NUL                                                          BODM0332
      RASZ=0.0                                                          BODM0333
         NM=0                                                           BODM0334
         KT=0                                                           BODM0335
      DO 2001 I=1,13                                                    BODM0336
 2001 KOT(I)=0                                                          BODM0337
      DO 2010 I=1,6                                                     BODM0338
 2010 BOT(I)=0                                                          BODM0339
      GOTO 21                                                           BODM0340
    4 CONTINUE                                                          BODM0341
      IF(NN.EQ.0) ND=2
      IF(NN.EQ.1) ND=8
C     ND=2                                                              BODM0342
      GOTO 21                                                           BODM0343
    6 CONTINUE                                                          BODM0344
   81 REWIND 2                                                          BODM0345
      dummy=setvideomode($defaultmode)
      END                                                               BODM0346
