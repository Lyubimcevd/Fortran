      include  'fgraph.fi'
      include  'fgraph.fd'
      INTEGER*2 A(26,200),S(2,200),P(2,200),TQ(6,500),TQC(4,500),NXZ(10,OTPL0001
     14),F(10)/'0 ','1 ','2 ','3 ','4 ','5 ','6 ','7 ','8 ','9 '/,      OTPL0002
     2U(20,100),KDP(180),ZG(100,60)                                     OTPL0003
      INTEGER*2 M11/'11'/,M12/'12'/,DEK                                 OTPL0004
      INTEGER*2 GR(1100),KLASIN(2,1100),PROZ(1100)                      OTPL0005
      INTEGER*2 KOH1/'ëé'/                                              OTPL0006
      INTEGER*2 KDPG(500),PRVD,PVGR,TQ4,GOD,MET/'ëè'/                   OTPL0007
      INTEGER F1(100,3),STDEK(3,4)/12*0/,DATE,CSTDEK(3)/3*0/            OTPL0008
      INTEGER QZ(4,500),IW(100),KOT(13),BOT(6),UW(2,200)                OTPL0009
      INTEGER*2 ZAN/-1/                                                 OTPL0010
      INTEGER*2 PRV,ST/'òí'/,PF/'èê'/,PF1/'îé'/,DB/'Ñ-'/,DB1/'Ñ '/      OTPL0011
      LOGICAL*1 D/' '/                                                  OTPL0012
      INTEGER*2 INSTR(5)                                                OTPL0013
      character*4 zakzao/'2046'/
      character*4 zakaz
      equivalence(nxz(1,1),zakaz)
      record / rccoord / curpos
      integer*2 dummy2
      integer*4 dummy4,dummy
      dummy=setvideomode( $ERESCOLOR )
      dummy=remappalette( 0,$BLUE )
      call clearscreen( $GCLEARSCREEN )
900   FORMAT(3A2,I2)                                                    OTPL0014
      DATA INSTR/'áA','KA','á ','15','ñ.'/                              OTPL0015
      open(14,file='f:\asyip\klasin.dat')
      open(3,file='f:\asyip\progao.dat',access='direct',recl=174,form=
     1'formatted')
      open(2,file='f:\asyip\progzao.dat',access='direct',recl=174,form=
     1'formatted')
      open(8,file='f:\asyip\zagr.dat',form='unformatted')
      dummy2=settextcolor(2)
      call settextposition( 10,25, curpos ) 
      call outtext( 'ß†§†© §†‚„(ÁÁ¨¨££££)' )
      read(*,777) idata
  777 format(i8)
       DATE=IDATA                                                       OTPL0019
      IDATA=IDATA-IDATA/10000*10000                                         OTPL0020
      NK=1                                                              OTPL0021
      NKZ=1                                                             OTPL0022
      DO 901 J=1,1100                                                   OTPL0023
      READ(14,900,END=902) GR(J),(KLASIN(I,J),I=1,2),PROZ(J)                    OTPL0024
      IF(GR(J).EQ.KOH1) GOTO 902                                        OTPL0025
901   CONTINUE                                                          OTPL0026
902   KKLAS=J-1                                                         OTPL0027
      open(4,file='f:\asyip\u.dat',form='unformatted')
      READ(4) NW,NU                                                     OTPL0028
      READ(4,end=111) ((UW(I,J),I=1,2),J=1,NW),((U(I,J),I=1,20),J=1,NU)         OTPL0029
  111 continue
      NUC=0                                                             OTPL0030
      DO 14 I=1,NU                                                      OTPL0031
      IF(U(7,I).LE.0)GOTO14                                             OTPL0032
      NUC=NUC+1                                                         OTPL0033
   14 CONTINUE                                                          OTPL0034
      open(1,file='f:\asyip\plan.dat',form='unformatted')
      READ(1) LP,NN,NK1                                                 OTPL0035
      LP1=LP+LP                                                         OTPL0036
      READ(1) (KDP(I),I=1,LP1)                                          OTPL0037
      NKP=NK1                                                           OTPL0038
      CALL KALDNI(NKP,0,KLD)                                            OTPL0039
      NKP=KLD/10                                                        OTPL0040
      DO 68 I1=LP,LP1                                                   OTPL0041
      IF(NKP.EQ.KDP(I1)/10) GOTO 69                                     OTPL0042
  68  CONTINUE                                                          OTPL0043
  69  NKP=I1-LP                                                         OTPL0044
      CALL KALDNI(NN,250,KDPG)                                          OTPL0045
      LP1=250                                                           OTPL0046
      MES=NN/10000-NN/1000000*100                                           OTPL0047
      DAY=NN-NN/10000*10000                                                 OTPL0048
  1   READ(1) NXZ,(IW(I),I=1,NU),IQ,IP,ID,LD,LZ,LF,MP,MS,NA,NS,NT       OTPL0049
      IF(NA.LT.0) GOTO 21                                               OTPL0050
      READ(1) ((A(I,J),I=1,26),J=1,NA)                                  OTPL0051
      READ(1) ((S(I,J),I=1,2),J=1,NS),((P(I,J),I=1,2),J=1,NS)           OTPL0052
      READ(1) ((TQ(I,J),I=1,6),J=1,NT),((QZ(I,J),I=1,4),J=1,NT)         OTPL0053
      READ(1) ((TQC(I,J),I=1,4),J=1,NT)                                 OTPL0054
      LF2=LF                                                            OTPL0055
      LF1=KDP(LF+LP)/10                                                 OTPL0056
      IF(NXZ(5,1).EQ.MET) LF1=NK1/100                                   OTPL0057
      IF(LF1.LE.1000+MES) DEK=1                                         OTPL0058
      IF(LF1.LE.1000+MES) GOTO 905                                      OTPL0059
      IF(LF1.LE.2000+MES) DEK=2                                         OTPL0060
      IF(LF1.LE.2000+MES) GOTO 905                                      OTPL0061
      DEK=3                                                             OTPL0062
  905 CONTINUE                                                          OTPL0063
      DO 903 J=1,KKLAS                                                  OTPL0064
      IF(KLASIN(1,J).NE.NXZ(2,2).OR.KLASIN(2,J).NE.NXZ(3,2)) GOTO 903   OTPL0065
      KPROZ=PROZ(J)                                                     OTPL0066
      GOTO 904                                                          OTPL0067
903   CONTINUE                                                          OTPL0068
904   KPROZ=0                                                           OTPL0069
      ID=ID-ID*KPROZ/1000                                               OTPL0070
      MP1=MP/10000*100+((MP-MP/10000*10000)-(MP-MP/10000*10000)/100*100
     1)                                                                 OTPL0071
      KPR=0                                                             OTPL0072
      PRV=0                                                             OTPL0073
      PVGR=0                                                            OTPL0074
      IF(MS.EQ.-2.OR.MS.EQ.-3) GOTO 1                                   OTPL0075
      NKT=0                                                             OTPL0076
   3  DO 4 I=1,13                                                       OTPL0077
   4  KOT(I)=0                                                          OTPL0078
       KT=0                                                             OTPL0079
      MID=100000                                                        OTPL0080
      DO 50 I=2,NT                                                      OTPL0081
      IF(TQC(2,I).EQ.0) GOTO 50                                         OTPL0082
      IF(TQ(4,I).LE.MID) MID=TQ(4,I)                                    OTPL0083
      IF(U(7,TQ(1,I)).EQ.0) GOTO 50                                     OTPL0084
      KOT(U(8,TQ(1,I)))=KOT(U(8,TQ(1,I)))+QZ(1,I)                       OTPL0085
   50 CONTINUE                                                          OTPL0086
      IF(MID.EQ.100000) GOTO 200                                        OTPL0087
      GOD=DAY                                                           OTPL0088
      MID1=KDPG(MID+LP1)/10                                             OTPL0089
      IF(MID1-MID1/100*100.LT.MES) GOD=DAY+1                            OTPL0090
      MID=MID1*100+(GOD-GOD/100*100)                                                  OTPL0091
      GOTO 201                                                          OTPL0092
  200 MID=0                                                             OTPL0093
  201 DO 5 I=1,NU                                                       OTPL0094
    5 KT=KT+IW(I)                                                       OTPL0095
      N=A(10,1)                                                         OTPL0096
      N=S(1,N)                                                          OTPL0097
      N=A(12,N)+A(13,N)-1                                               OTPL0098
      NM=TQ(2,N)/100                                                    OTPL0099
      N=TQ(1,N)                                                         OTPL0100
      NZ=U(1,N)                                                         OTPL0101
      IF(LZ.EQ.0) GOTO 208                                              OTPL0103
      IF(TQ(2,NT).NE.501) GOTO 6                                        OTPL0104
      IF(TQC(2,NT).LE.0.AND.TQC(1,NT).LT.0) GOTO 208                    OTPL0105
      IF(TQC(2,NT).GT.0) KPR=1                                          OTPL0106
      IF(TQC(2,NT).GT.0) GOTO 208                                       OTPL0107
      KSCP=0                                                            OTPL0108
      KTQP=NT-1                                                         OTPL0109
      N11=A(10,NA)                                                      OTPL0110
      IF(N11.EQ.0) NZIK=2                                               OTPL0111
      IF(N11.NE.0) NZIK=A(12,S(1,N11))+A(13,S(1,N11))                   OTPL0112
      DO 209 J=NZIK,KTQP                                                OTPL0113
      IF(TQC(2,J).EQ.0.AND.TQC(1,J).LE.0) GOTO 209                      OTPL0114
      IF(TQ(4,J).LE.LP) KSCP=1                                          OTPL0115
  209 CONTINUE                                                          OTPL0116
      IF(KSCP.EQ.0) KPR=1                                               OTPL0117
  208 CONTINUE                                                          OTPL0118
      DO 7  I=1,6                                                       OTPL0119
      BOT(I)=KOT(I)                                                     OTPL0120
    7 NKT=NKT+BOT(I)                                                    OTPL0121
      GOTO 30                                                           OTPL0122
    6 DO 9 I=1,6                                                        OTPL0123
      BOT(I)=KOT(I)                                                     OTPL0124
    9 NKT=NKT+BOT(I)                                                    OTPL0125
      IF(LZ.EQ.0) GOTO 204                                              OTPL0126
      IF(TQ(2,2).NE.512.AND.TQ(2,2).NE.513) GOTO 204                    OTPL0127
      IF(TQC(2,2).LE.0.AND.TQC(1,2).LT.0) GOTO 204                      OTPL0128
      IF(TQC(2,2).GT.0) KPR=2                                           OTPL0129
      IF(TQC(2,2).GT.0) GOTO 204                                        OTPL0130
      KSCP=0                                                            OTPL0131
      DO 205 J=3,NT                                                     OTPL0132
      IF(TQC(2,J).EQ.0.AND.TQC(1,J).LE.0) GOTO 205                      OTPL0133
      IF(TQ(4,J).LE.LP) KSCP=1                                          OTPL0134
  205 CONTINUE                                                          OTPL0135
      IF(KSCP.EQ.0) KPR=2                                               OTPL0136
  204 CONTINUE                                                          OTPL0137
  30  LK=LF+LP                                                          OTPL0149
      IF(MS.EQ.-1) KPR=0                                                OTPL0150
      IF(KPR.EQ.0) GOTO 40                                              OTPL0151
      IF(KPR.EQ.2) NM=5                                                 OTPL0152
      IF(KPR.EQ.2) MMP=TQ(5,2)                                          OTPL0153
      IF(KPR.EQ.1) MMP=TQ(5,NT)                                         OTPL0155
      IF(KPR.EQ.1) NM=5                                                 OTPL0156
      IF(NXZ(1,4).EQ.ST) PRV=1                                          OTPL0157
      IF(NXZ(1,4).EQ.ST) GOTO 40                                        OTPL0158
      IF(NXZ(1,4).EQ.PF.OR.NXZ(1,4).EQ.PF1) PRV=2                       OTPL0159
      IF(NXZ(1,4).EQ.PF.OR.NXZ(1,4).EQ.PF1) GOTO 40                     OTPL0160
      IF(NXZ(1,4).EQ.DB.OR.NXZ(1,4).EQ.DB1) GOTO 41                     OTPL0161
      PRV=3                                                             OTPL0162
      GOTO 40                                                           OTPL0163
   41 IF(NXZ(2,4).EQ.ST) PRV=1                                          OTPL0164
      IF(NXZ(2,4).EQ.ST) GOTO 40                                        OTPL0165
      IF(NXZ(2,4).EQ.PF.OR.NXZ(2,4).EQ.PF1) PRV=2                       OTPL0166
      IF(NXZ(2,4).EQ.PF.OR.NXZ(2,4).EQ.PF1) GOTO 40                     OTPL0167
      PRV=3                                                             OTPL0168
   40 CONTINUE                                                          OTPL0169
      IF(KPR.EQ.1.OR.KPR.EQ.2) GOTO 210                                 OTPL0170
      MMP=32000                                                         OTPL0171
      IF(KPR.NE.0) GOTO 207                                             OTPL0172
      DO 206 I=2,NT                                                     OTPL0173
      I1=TQ(2,I)-TQ(2,I)/100*100                                        OTPL0174
      IF(I1.LE.82) GOTO 206                                             OTPL0175
      TQ4=TQ(4,I)                                                       OTPL0176
      IF(TQC(2,I).EQ.0.AND.TQC(1,I).LT.0) TQ4=TQC(1,I)                  OTPL0177
      IF(TQ(4,I).LE.MMP) MMP=TQ4                                        OTPL0178
  206 CONTINUE                                                          OTPL0179
  210 CONTINUE                                                          OTPL0180
      IF(MMP.EQ.32000) MP=777777                                        OTPL0181
      IF(MMP.EQ.32000) GOTO 207                                         OTPL0182
      IF(MMP.LT.-LP1) MP=999999                                         OTPL0183
      IF(MMP.LT.-LP1) GOTO 207                                          OTPL0184
      IF(MMP.GT.LP1) MP=888888                                          OTPL0185
      IF(MMP.GT.LP1) GOTO 207                                           OTPL0186
      MMP=MMP+LP1                                                       OTPL0187
      MP=KDPG(MMP)/10                                                   OTPL0188
      GOD=DAY                                                           OTPL0189
      IF(MP-MP/100*100.LE.MES.AND.MMP-LP1.GT.LP) GOD=DAY+1              OTPL0190
      IF(MP-MP/100*100.GE.MES.AND.MMP-LP1.LE.0) GOD=DAY-1               OTPL0191
      MP=MP*100+(GOD-GOD/100*100)                                                     OTPL0192
  207 CONTINUE                                                          OTPL0193
      IF(KPR.NE.0) PVGR=ZAN                                             OTPL0194
      IF(LF.GT.LP) PVGR=ZAN                                             OTPL0195
      IF(NXZ(5,1).EQ.MET) MS=1                                          OTPL0196
      IF(NXZ(5,1).EQ.MET.AND.LF.LE.LP) LF=LP                            OTPL0197
      LF=LF+LP1                                                         OTPL0198
      IF(LF.GT.LP1*2) LF=888888                                         OTPL0199
      IF(LF.EQ.888888) GOTO 10                                          OTPL0200
      LK=KDPG(LF)/10                                                    OTPL0201
      GOD=DAY                                                           OTPL0202
      IF(LK-LK/100*100.LE.MES.AND.LF-LP1.GT.LP) GOD=DAY+1               OTPL0203
      LF=LK*100+(GOD-GOD/100*100)                                                     OTPL0204
c      IF(LD-LD/1000*1000.LE.127) LD=LD/10                               OTPL0205
   10 DO 11 I=1,6                                                       OTPL0206
   11 BOT(I)=(BOT(I)+50)/100                                            OTPL0207
      IF(IQ.EQ.0) GOTO 100                                              OTPL0208
      ID=ID-(ID*QZ(2,1)*10/IQ+5)/10                                     OTPL0209
      IF(LF2.GT.NKP) GOTO 1000                                          OTPL0211
      IF(NM.EQ.1) NSTDEK=1                                              OTPL0212
      IF(NM.EQ.2) NSTDEK=2                                              OTPL0213
      IF(NM.EQ.3) NSTDEK=3                                              OTPL0214
      if(nm.eq.4) nstdek=4
      if(nm.gt.4) goto 1000
      STDEK(DEK,NSTDEK)=STDEK(DEK,NSTDEK)+ID                            OTPL0215
 1000 CONTINUE                                                          OTPL0216
      KT=KT-(KT*QZ(2,1)*100/IQ+50)/100                                  OTPL0217
      IQ=IQ-QZ(2,1)                                                     OTPL0218
      IF(IQ.LT.0) IQ=0                                                  OTPL0219
      IF(KT.LT.0) KT=0                                                  OTPL0220
      IF(ID.LT.0) ID=0                                                  OTPL0221
  100 CONTINUE                                                          OTPL0222
      KT=(KT+50)/100                                                    OTPL0223
      NKT=(NKT+50)/100                                                  OTPL0224
      IF(IQ.EQ.0.AND.KT.EQ.0.AND.ID.EQ.0) GOTO 1                        OTPL0225
   70 DO 2 J=1,10                                                       OTPL0262
      IF(NXZ(4,1).EQ.F(J)) GOTO 8                                       OTPL0263
    2 CONTINUE                                                          OTPL0264
      IF(ZAKAZ.EQ.ZAKZAO) GOTO 1234
      WRITE(3,33,rec=NK) NZ,(NXZ(I,1),I=1,5),(NXZ(I,4),I=1,5),(NXZ(I,3),OTPL0265
     1i=1,10),IQ,NM,KT,(BOT(I),I=1,6),NKT,LZ,LF,MP,                     OTPL0266
     2LD,IP,MS,PRV,ID,MP1,PVGR,DEK,MID                                  OTPL0267
      NK=NK+1
      GOTO 1
 1234 WRITE(2,33,rec=NKz) NZ,(NXZ(I,1),I=1,5),(NXZ(I,4),I=1,5),(NXZ(I,3)OTPL0265
     1,i=1,10),IQ,NM,KT,(BOT(I),I=1,6),NKT,LZ,LF,MP,                    OTPL0266
     2LD,IP,MS,PRV,ID,MP1,PVGR,DEK,MID                                  OTPL0267
   33 format(i2,20a2,22i6)
      NKZ=NKZ+1                                                           OTPL0268
      GOTO 1 
    8 IF(ZAKAZ.EQ.ZAKZAO) GOTO 1235
      WRITE(3,34,rec=NK) NZ,D,(NXZ(I,1),I=1,4),D,(NXZ(I,4),I=1,5),(NXZ(IOTPL0280
     1,3),I=1,10)                                                       OTPL0281
     2,IQ,NM,KT,(BOT(I),I=1,6),NKT,LZ,LF,MP,LD,IP,MS,PRV,ID,MP1,PVGR,   OTPL0282
     3DEK,MID                                                           OTPL0283
      NK=NK+1                                                           OTPL0284
   34 format(i2,a1,4a2,a1,15a2,22i6)
      GOTO 1                                                            OTPL0295
 1235 WRITE(2,34,rec=NKZ) NZ,D,(NXZ(I,1),I=1,4),D,(NXZ(I,4),I=1,5),(NXZ(OTPL0280
     1I,3),I=1,10)                                                      OTPL0281
     2,IQ,NM,KT,(BOT(I),I=1,6),NKT,LZ,LF,MP,LD,IP,MS,PRV,ID,MP1,PVGR,   OTPL0282
     3DEK,MID                                                           OTPL0283
      NKZ=NKZ+1
      GOTO 1
  21  WRITE(8) NN,NK1,LP                                                OTPL0296
      write(8) (KDP(i),i=1,lp1)
      READ(1) ((ZG(I,J),I=1,NUC),J=1,LP),((F1(I,J),I=1,NUC),J=1,3)      OTPL0297
  20  WRITE(8) ((ZG(I,J),I=1,NUC),J=1,LP),((F1(I,J),I=1,NUC),J=1,3)     OTPL0298
      DO 65 J=1,4                                                       OTPL0300
      STDEK(2,J)=STDEK(2,J)+STDEK(1,J)                                  OTPL0301
      STDEK(3,J)=STDEK(2,J)+STDEK(3,J)                                  OTPL0302
      STDEK(1,J)=STDEK(1,J)/10                                          OTPL0303
      STDEK(2,J)=STDEK(2,J)/10                                          OTPL0304
  65  STDEK(3,J)=STDEK(3,J)/10                                          OTPL0305
      open(9,file='prn')
      WRITE(9,60) DATE                                                  OTPL0306
  60  FORMAT(/////1X,'ÑEKAÑHõâ èãAH èO TOBAPHOMì BõèìCKì èO àè',10X     OTPL0307
     *,I8/)                                                             OTPL0308
      WRITE(9,61)                                                       OTPL0309
  61  FORMAT(4X,'HOM.ìó-KA 1 ÑEKAÑA 2 ÑEKAÑA 3 ÑEKAÑA'/)                OTPL0310
      WRITE(9,62) (STDEK(I,1),I=1,3)                                    OTPL0311
  62  FORMAT(8X,' 1',6X,I5,4X,I5,4X,I5/)                                OTPL0312
      WRITE(9,63) (STDEK(I,2),I=1,3)                                    OTPL0313
  63  FORMAT(8X,' 2',6X,I5,4X,I5,4X,I5/)                                OTPL0314
      WRITE(9,64) (STDEK(I,3),I=1,3)                                    OTPL0315
  64  FORMAT(8X,' 3',6X,I5,4X,I5,4X,I5/)                                OTPL0316
      write(9,664) (stdek(i,4),I=1,3)
 664  FORMAT(8X,' 4',6X,I5,4X,I5,4X,I5/)
      DO 66 J=1,3                                                       OTPL0317
      DO 66 I=1,4                                                       OTPL0318
  66  CSTDEK(J)=CSTDEK(J)+STDEK(J,I)                                    OTPL0319
      WRITE(9,67) (CSTDEK(J),J=1,3)                                     OTPL0320
  67  FORMAT(5X,'àTOÉO',6X,I5,4X,I5,4X,I5)                              OTPL0321
      DO 666 I=1,56                                                     OTPL0322
      WRITE(9,667)                                                      OTPL0323
 667  FORMAT(1X,' ')                                                    OTPL0324
 666  CONTINUE                                                          OTPL0325
      REWIND  4                                                         OTPL0326
      close(2)
      close(3)
      end file 8
      dummy=setvideomode($defaultmode)
      END                                                               OTPL0327
