      INCLUDE  'FGRAPH.FI'
      INCLUDE  'FGRAPH.FD'
      INTEGER*2 A(26,500),S(2,500),P(2,500),TQ(6,1000),TQC(4,1000),U(20,OHTP0001
     *100),NXZ(10,4),ZAK(10)                                            OHTP0002
      INTEGER*2 ZAK1(4,32)/128*'  '/,EDF/-1/,ZAK2(4,32),CON/'  '/,
     *KDTK(99)                                                          OHTP0003
      INTEGER QZ(4,1000),UW(2,200),IW(100)                              OHTP0004
      record / rccoord / curpos
      integer*2 dummy2
      integer*4 dummy4,dummy
      CHARACTER*1 F1/'1'/,F2/'2'/,F3/'3'/,NKMAX1/'/'/
      DATA L1,L2,NF/4,1,2/,LIMA,LIMS,LIMT/2*200,500/                    OHTP0005
      DATA L/71/,NKMAX/'/   '/                                          OHTP0006
      DATA NK/1/,JFM/0/                                                 OHTP0007
      dummy=setvideomode( $ERESCOLOR )
      dummy=remappalette( 0,$BLUE )
      call clearscreen( $GCLEARSCREEN )
      OPEN(2,FILE='D:\ASYIP\OMOIH.DAT',FORM='FORMATTED',RECL=101,ACCESS=OHTP0008
     1'DIRECT')
      OPEN(3,FILE='D:\ASYIP\HEP.DAT',FORM='FORMATTED',RECL=256,ACCESS='DOHTP0009
     1IRECT')
      OPEN(9,FILE='PRN')
   50 FORMAT(128A2)                                                     OHTP0010
    6 FORMAT(A1,20A2,I2,I5,20A2,2X,I5,I6)                               OHTP0011
    7 FORMAT(I8)                                                        OHTP0012
    9 FORMAT(10X,'áAKAá  ',10A2,'èPABàãúHõâ')                           OHTP0013
   10 FORMAT(A1,170X)                                                   OHTP0014
   20 FORMAT(//10X,'**** èPOBEPàTú ÑAHHõE Ñãü áAKAáA ',10A2,' (B èOPTîEãOHTP0015
     1ú HE èOMEôEH).')                                                  OHTP0016
   26 FORMAT(///)                                                       OHTP0017
 300  FORMAT(5X,'B áAKAáE ',10A2,'HE OÑHOáHAóHO OèPEÑEãEH HOMEP CTAHOóHOOHTP0018
     1É0 ìóACTKA')                                                      OHTP0019
   34 FORMAT(///5X,'B áAKAáE ',10A2,'OTCìTCTBìET áAüBKA')               OHTP0020
c     NK1=1                                                             OHTP0021
c  52 READ(3,50,REC=NK1,end=40) ((ZAK1(I,J),I=1,4),J=1,32)              OHTP0022
c     NK1=NK1+1                                                         OHTP0023
C     IF(ZAK1(1,1).EQ.EDF) GOTO 40                                      OHTP0024
      DO 51 J=1,32                                                      OHTP0025
      DO 53 I=1,4                                                       OHTP0026
   53 ZAK2(I,J)=ZAK1(I,J)                                               OHTP0027
   51 CONTINUE                                                          OHTP0028
   40 KZ=0                                                              OHTP0030
      OPEN(UNIT=L1,FILE='f:\ASYIP\U.DAT',FORM='UNFORMATTED')
      READ(L1) NW,NU                                                    OHTP0031
      READ(L1) ((UW(I,J),I=1,2),J=1,NW),((U(I,J),I=1,20),J=1,NU)        OHTP0032
      REWIND L1                                                         OHTP0033
      IERL=0                                                            OHTP0034
      IERL1=0                                                           OHTP0035
      IERL2=0                                                           OHTP0036
      IERL5=0
      ierl6=0
      n=0                                                               OHTP0037
      dummy2=settextcolor(2)
      call settextposition( 10,25, curpos ) 
      call outtext( 'ß†§†© §†‚„(ÁÁ¨¨££)' )
      read(*,777) idat
  777 format(i6)
      call settextposition( 11,25, curpos ) 
      call outtext( 'ß†§†© ≠Æ¨•‡ Ê•Â†(nnn) ' )
      read(*,778) NZZ
  778 format(i3)                                                        OHTP0040
c      WRITE(9,90) NZZ,IDAT                                              OHTP0041
   90 FORMAT(////5X,'èPOTOKOã ãOÉàóECKOÉO KOHTPOãü BXOÑHõX ÑOKìMEHTOB   OHTP0042
     1èO ñEXì ',I3,10X,I6)                                              OHTP0043
    4 CONTINUE                                                          OHTP0044
      READ(2,6,REC=NK,ERR=666,END=5)
     1JFM,((NXZ(I,J),I=1,10),J=1,2),IP,IQ,((NXZ(I,J)                    OHTP0045
     2,I=1,10),J=3,4),ID,MP                                             OHTP0046
      NK0=NK                                                            OHTP0047
      NK=NK+1                                                           OHTP0048
      IF(JFM.NE.F3) GOTO30                                              OHTP0050
      IF(KZ.EQ.0) GOTO33                                                OHTP0051
      KZ=0                                                              OHTP0052
  150 WRITE(9,34) (ZAK(I),I=1,10)                                       OHTP0053
      DO 101 J=1,32                                                     OHTP0054
      IF(ZAK2(1,J).EQ.CON) GOTO 102                                     OHTP0055
      DO 103 I=1,4                                                      OHTP0056
      IF(ZAK(I).NE.ZAK2(I,J)) GOTO 101                                  OHTP0057
 103  CONTINUE                                                          OHTP0058
      IF(KZ.EQ.0) GOTO 33                                               OHTP0059
      GOTO 31                                                           OHTP0060
  101 CONTINUE                                                          OHTP0061
  102 DO 104 I=1,4                                                      OHTP0062
      ZAK2(I,J)=ZAK(I)                                                  OHTP0063
  104 CONTINUE                                                          OHTP0064
      IF(KZ.NE.0) GOTO 31                                               OHTP0065
   33 WRITE(9,26)                                                       OHTP0066
      CALL STRSSV(NF,NA,NS,A,S,LIMA,LIMS,NK,NKMAX,IERL1,NXZ,ZAK2,CON,F2,OHTP0067
     *ierl6)
      IF(NA.LE.1) GOTO 80                                               OHTP0068
      if(ierl6.lt.0) goto 200
      CALL TCLZKZ(NF,LIMT,NW,NU,NA,NT,UW,U,A,TQ,TQC,QZ,NK,NKMAX,IERL,ZAKOHTP0069
     12,NXZ,IERL5,F1)                                                   OHTP0070
      CALL STRPRM(LIMS,NA,NS,NXZ,A,S,P,IQ,IERL1)                        OHTP0071 
      IND=0                                                             OHTP0072
      IF(IERL1.LT.0.OR.IERL.LT.0) IND=-1                                OHTP0073
      NZ=A(10,1)                                                        OHTP0074
      NZ=S(1,NZ)                                                        OHTP0075
      NZ=A(12,NZ)+A(13,NZ)-1                                            OHTP0076
      NM=TQ(2,NZ)/100                                                   OHTP0077
      NZ=U(1,TQ(1,NZ))                                                  OHTP0078
      IF(NZ.NE.10) GOTO 301                                             OHTP0079
      IF(NM.NE.11.AND.NM.NE.12.AND.NM.NE.13) GOTO 301                   OHTP0080
      IPR11=0                                                           OHTP0081
      IPR12=0                                                           OHTP0082
      DO 302 I=2,NT                                                     OHTP0083
      IF(U(1,TQ(1,I)).EQ.15) GOTO 302                                   OHTP0084
      IF(TQ(2,I)/100.EQ.11) IPR11=1                                     OHTP0085
      IF(TQ(2,I)/100.EQ.12) IPR12=1                                     OHTP0086
  302 CONTINUE                                                          OHTP0087
      IF(IPR11.EQ.1.AND.IPR12.EQ.1.AND.IND.GE.0)     IND=-1             OHTP0088
      IF(IPR11.EQ.1.AND.IPR12.EQ.1)   WRITE(9,300) (NXZ(I,1),I=1,10)    OHTP0089
  301 CONTINUE                                                          OHTP0090
      IND1=0                                                            OHTP0091
      DO 400 I=1,99                                                     OHTP0092
  400 KDTK(I)=0                                                         OHTP0093
      DO 401 I=1,NA                                                     OHTP0094
  401 KDTK(A(9,I))=KDTK(A(9,I))+1                                       OHTP0095
      DO 402 I=1,99                                                     OHTP0096
      IF(KDTK(I).GT.32) WRITE(9,403) (NXZ(J,1),J=1,10),I                OHTP0097
      IF(KDTK(I).GT.32) IND=-1                                          OHTP0098
      IF(KDTK(I).GT.32) IND1=-1                                         OHTP0099
  402 CONTINUE                                                          OHTP0100
  403 FORMAT(5X,'B áAKAáE ',10A2,'ÅOãúòE ÑOèìCTàMOÉO óàCãA ÑETAãEâ C OÑ OHTP0101
     1HOâ MTK N',I2)                                                    OHTP0102
      CALL CHKPRT(LIMT,NA,NT,A,S,P,TQ,QZ,TQC,U,NU,IND,IERL,IERL5)       OHTP0103
      IF(IERL1.LT.0.OR.IERL.LT.0) GOTO 200                              OHTP0104
      IF(IND1.NE.0) GOTO 200                                            OHTP0105
      CALL SGOKIZ(NA,NT,A,S,P,TQ,QZ,TQC,U,IERL2)                        OHTP0106
  200 IF(IERL1.LT.0.OR.IERL.LT.0.OR.IERL2.LT.0.OR.ierl6.lt.0) IND=-1    OHTP0107
      IF(IND.GE.0.AND.NA.GT.1) GOTO 80                                  OHTP0108
      WRITE(9,20) (NXZ(I,1),I=1,10)                                     OHTP0109
      GOTO 80                                                           OHTP0110
   8  WRITE(9,9) (NXZ(I,1),I=1,10)                                      OHTP0111
      GOTO 4                                                            OHTP0112
   80 DO 55 J=1,32                                                      OHTP0113
      IF(ZAK2(1,J).EQ.CON) GOTO 88                                      OHTP0114
      DO 54 I=1,4                                                       OHTP0115
      IF(NXZ(I,1).NE.ZAK2(I,J)) GOTO 55                                 OHTP0116
   54 CONTINUE                                                          OHTP0117
      IF(IND.LT.0) GOTO 4                                               OHTP0118
      WRITE(9,800) (NXZ(I,1),I=1,4)                                     OHTP0119
  800 FORMAT (//5X,'èéÇíéê HéåÖêÄ áÄäÄáÄ ',4a2)
      WRITE(9,20) (NXZ(I,1),I=1,10)                                     OHTP0122
      GOTO 4                                                            OHTP0123
   55 CONTINUE                                                          OHTP0124
   88 IF(IND.GE.0.AND.NA.GT.1) GOTO 8                                   OHTP0125
      DO 89 I=1,4                                                       OHTP0126
      ZAK2(I,J)=NXZ(I,1)                                                OHTP0127
   89 CONTINUE                                                          OHTP0128
      if(ierl6.lt.0) goto 105
      GOTO 4                                                            OHTP0129
  30  IF(KZ.EQ.0) GOTO 31                                               OHTP0130
      KZ=1                                                              OHTP0131
      DO 110 K=1,4                                                      OHTP0132
      IF(ZAK(K).NE.NXZ(K,1)) GOTO 150                                   OHTP0133
  110 CONTINUE                                                          OHTP0134
   31 DO 32 K=1,10                                                      OHTP0135
      ZAK(K)=NXZ(K,1)                                                   OHTP0136
   32 CONTINUE                                                          OHTP0137
      KZ=1                                                              OHTP0138
      GOTO 4                                                            OHTP0139
  666 KZ=1
  667 DO 668 K=1,10                                                     OHTP0135
      ZAK(K)=NXZ(K,1)                                                   OHTP0136
  668 CONTINUE                                                          OHTP0137
      goto 555
    5 CONTINUE                                                          OHTP0140
  555 IF(KZ.EQ.0) GOTO 105                                              OHTP0141
      WRITE(9,34) (ZAK(I),I=1,10)                                       OHTP0142
      DO 106 J=1,32                                                     OHTP0143
      IF(ZAK2(1,J).EQ.CON) GOTO 107                                     OHTP0144
      DO 108 I=1,4                                                      OHTP0145
      IF(ZAK(I).NE.ZAK2(I,J)) GOTO 106                                  OHTP0146
  108 CONTINUE                                                          OHTP0147
      GOTO 105                                                          OHTP0148
  106 CONTINUE                                                          OHTP0149
  107 DO 109 I=1,4                                                      OHTP0150
      ZAK2(I,J)=ZAK(I)                                                  OHTP0151
  109 CONTINUE                                                          OHTP0152
  105 NK1=1                                                             OHTP0153
      WRITE(3,50,REC=NK1) ((ZAK2(I,J),I=1,4),J=1,32)                    OHTP0154
      DUMMY=SETVIDEOMODE($DEFAULTMODE)
      END                                                               OHTP0155
      SUBROUTINE STRSSV(NF,NA,NS,A,S,LIMA,LIMS,NK,NKMAX,IERL1,NXZ,ZAK2  OHTP0156
     1,CON,F2,IERL6)
      INTEGER*2 ZAK2(4,32),CON                                          OHTP0158
      INTEGER*2 NXZ(10,1)                                               OHTP0159
      INTEGER*2 A(26,1),S(2,1),H(10),Q(9)                               OHTP0160
      INTEGER*2 H1(4)                                                   OHTP0161
      INTEGER*2 H2(10),H3(10,50),H4(50)                                 OHTP0162
      CHARACTER*1 F2
   1  FORMAT(A1,4A2,12X,10A2,10A2,I3,I2,2I3,3I5,I3,I2)                  OHTP0163
      DO 50 J=1,50                                                      OHTP0164
      DO 50 I=1,10                                                      OHTP0165
   50 H3(I,J)=0                                                         OHTP0166
      DO 51 I=1,50                                                      OHTP0167
   51 H4(I)=0                                                           OHTP0168
      IERL1=0                                                           OHTP0169
      IERL6=0
      KSP=0                                                             OHTP0170
      NKSP=0                                                            OHTP0171
      NA=1                                                              OHTP0172
      NS=0                                                              OHTP0173
      J0=1                                                              OHTP0174
      nknah=nk
      READ(NF,1,REC=NK,ERR=115,END=40) JFM,H1,H2,H,Q                    OHTP0175
      IF(JFM.NE.F2) GOTO 40                                             OHTP0176
      DO 52 J=1,50                                                      OHTP0177
      IF(H3(1,J).EQ.0) GOTO 54                                          OHTP0178
      DO 53 I=1,10                                                      OHTP0179
      IF(H3(I,J).NE.H2(I)) GOTO 52                                      OHTP0180
   53 CONTINUE                                                          OHTP0181
      IF(Q(1).EQ.0) H4(J)=1                                             OHTP0182
      GOTO 55                                                           OHTP0183
   52 CONTINUE                                                          OHTP0184
      GOTO 55                                                           OHTP0185
  54  DO 56 I=1,10                                                      OHTP0186
  56  H3(I,J)=H2(I)                                                     OHTP0187
      IF(Q(1).EQ.0) H4(J)=1                                             OHTP0188
   55 CONTINUE                                                          OHTP0189
      DO 42 I=1,4                                                       OHTP0190
      IF(NXZ(I,1).NE.H1(I)) GOTO 43                                     OHTP0191
   42 CONTINUE                                                          OHTP0192
      GOTO 48                                                           OHTP0193
   43 WRITE(9,44) (NXZ(I,1),I=1,4)                                      OHTP0194
   44 FORMAT(5X,'B áAKAáE',1X,4A2,1X,'OTCìTCTBìET CèEñàîàKAñàü')        OHTP0195
      GOTO 112                                                          OHTP0196
   28 READ(NF,1,REC=NK,ERR=115,END=40) JFM,H1,H2,H,Q                    OHTP0197
      IF(JFM.NE.F2) GOTO 40                                             OHTP0198
      DO 60 J=1,50                                                      OHTP0199
      IF(H3(1,J).EQ.0) GOTO 62                                          OHTP0200
      DO 61 I=1,10                                                      OHTP0201
      IF(H3(I,J).NE.H2(I)) GOTO 60                                      OHTP0202
   61 CONTINUE                                                          OHTP0203
      IF(Q(1).EQ.0) H4(J)=1                                             OHTP0204
      GOTO 63                                                           OHTP0205
   60 CONTINUE                                                          OHTP0206
      GOTO 63                                                           OHTP0207
  62  DO 64 I=1,10                                                      OHTP0208
  64  H3(I,J)=H2(I)                                                     OHTP0209
      IF(Q(1).EQ.0) H4(J)=1                                             OHTP0210
   63 CONTINUE                                                          OHTP0211
      DO 111 I=1,4                                                      OHTP0212
      IF(NXZ(I,1).NE.H1(I)) GOTO 112                                    OHTP0213
  111 CONTINUE                                                          OHTP0214
      GOTO 48                                                           OHTP0215
  112 DO 106 J=1,32                                                     OHTP0216
      IF(ZAK2(1,J).EQ.CON) GOTO 107                                     OHTP0217
      DO 108 I=1,4                                                      OHTP0218
      IF(ZAK2(I,J).NE.NXZ(I,1)) GOTO 106                                OHTP0219
  108 CONTINUE                                                          OHTP0220
      GOTO 45                                                           OHTP0221
  106 CONTINUE                                                          OHTP0222
  107 DO 109 I=1,4                                                      OHTP0223
      ZAK2(I,J)=NXZ(I,1)                                                OHTP0224
  109 CONTINUE                                                          OHTP0225
   45 IERL1=-1                                                          OHTP0226
      DO 46 I=1,4                                                       OHTP0227
      NXZ(I,1)=H1(I)                                                    OHTP0228
   46 CONTINUE                                                          OHTP0229
      WRITE(9,47) (NXZ(I,1),I=1,10)                                     OHTP0230
   47 FORMAT(///5X,'B áAKAáE ',10A2,'OTCìTCTBìET áAüBKA')               OHTP0231
   48 CONTINUE                                                          OHTP0232
C     ***** ARRANGEMENT:                                                OHTP0233
      IF(Q(1).EQ.0) KSP=1                                               OHTP0234
      IF(Q(9).EQ.0) NKSP=1                                              OHTP0235
      IF(NA.LE.1) GOTO 16                                               OHTP0236
      DO 17 J=2,NA                                                      OHTP0237
      DO 18 J1=1,10                                                     OHTP0238
      IF(A(J1+16,J).NE.H(J1)) GOTO 19                                   OHTP0239
   18 CONTINUE                                                          OHTP0240
      GOTO 20                                                           OHTP0241
   19 IF(Q(8).LE.0) GOTO 17                                             OHTP0242
      IF(A(2,J).EQ.Q(8)) GOTO 21                                        OHTP0243
   17 CONTINUE                                                          OHTP0244
   16 NA=NA+1                                                           OHTP0245
      IF(NA.GT.LIMA) IERL1=-1                                           OHTP0246
      IF(NA.GT.LIMA) KOLSP=NA-LIMA                                      OHTP0247
      IF(NA.GT.LIMA) WRITE(9,49) (NXZ(I,1),I=1,4),KOLSP                 OHTP0248
      IF(NA.GT.LIMA) LIMA=500                                           OHTP0249
      IF(NA.GT.LIMS) LIMS=500                                           OHTP0250
   49 FORMAT(5X,'B áAKAáE ',4A2,' ÅOãúòE ÑOèìCTàMOÉO óàCãA CTPOK B CèEñàOHTP0251
     1îàKAñàà HA',1X,I3)                                                OHTP0252
      J=NA                                                              OHTP0253
      DO 22 J1=1,10                                                     OHTP0254
   22 A(J1+16,J)=H(J1)                                                  OHTP0255
      A(2,J)=Q(8)                                                       OHTP0256
      DO 23 J1=2,7                                                      OHTP0257
   23 A(J1+1,J)=Q(J1)                                                   OHTP0258
      A( 9,J)=Q(9)                                                      OHTP0259
      A(11,J)=0                                                         OHTP0260
      A(13,J)=0                                                         OHTP0261
      A(16,J)=NK                                                        OHTP0262
      GOTO 24                                                           OHTP0263
   20 IF(A(2,J).EQ.Q(8).OR.A(2,J)*Q(8).LE.0) GOTO 32                    OHTP0264
   21 WRITE(9,100) A(2,J),(A(J1,J),J1=17,26),Q(8),H                     OHTP0265
      IERL1=-1                                                          OHTP0266
   32 IF(A(2,J).LE.0) A(2,J)=Q(8)                                       OHTP0267
      IF(A(3,J)+A(4,J)+A(5,J).GT.0) GOTO 25                             OHTP0268
      DO 26 J1=2,7                                                      OHTP0269
   26 A(J1+1,J)=Q(J1)                                                   OHTP0270
   25 IF(A(9,J).LE.0) A(9,J)=Q(9)                                       OHTP0271
   24 IF(Q(1).GT.0) GOTO 27                                             OHTP0272
      J0=J                                                              OHTP0273
      IF(A(11,J).GT.0) J0=1                                             OHTP0274
      A(10,J0)=NS+1                                                     OHTP0275
      A(11,J0)=0                                                        OHTP0276
      GOTO 29                                                           OHTP0277
   27 A(11,J0)=A(11,J0)+1                                               OHTP0278
      NS=NS+1                                                           OHTP0279
      IF(NS.GT.LIMS) IERL1=-1                                           OHTP0280
      IF(NS.GT.LIMS) WRITE(9,200) (NXZ(I,1),I=1,4)                      OHTP0281
  200 FORMAT(5X,' B áAKAáE ',4A2,'ÅOãúòE ÑOèìCTàMOÉO óàCãO èOÑCÅOPOK B COHTP0282
     1èEñàîàKAñàà')                                                     OHTP0283
      IF(NS.GT.LIMS) LIMS=500                                           OHTP0284
      S(1,NS)=J                                                         OHTP0285
      S(2,NS)=Q(1)                                                      OHTP0286
   29 NK=NK+1                                                           OHTP0287
      GOTO 28                                                           OHTP0288
   40 IF(KSP.NE.0) GOTO 57                                              OHTP0289
      IERL1=-1                                                          OHTP0290
      IF(NA.LE.1) WRITE(9,44) (NXZ(I,1),I=1,4)                          OHTP0291
  57  DO 58 J=1,50                                                      OHTP0292
      IF(H3(1,J).EQ.0) GOTO 35                                          OHTP0293
      IF(H4(J).NE.0) GOTO 58                                            OHTP0294
      WRITE(9,101) (NXZ(I,1),I=1,4),(H3(I,J),I=1,10)                    OHTP0295
      IERL1=-1                                                          OHTP0296
   58 CONTINUE                                                          OHTP0297
  35  IF(NKSP.EQ.0) GOTO 15                                             OHTP0298
      WRITE(9,102)                                                      OHTP0299
      IERL1=-1                                                          OHTP0300
      goto 15
  115 NKCP=NK-NKNAH+1
      write(9,116) nkcp
      ierl6=-1
  116 format(5x,'éòàÅäÄ óíÖHàü ëèÖñàîàäÄñàà Ç áÄèàëà   N',i3) 
   15 RETURN                                                            OHTP0301
  100 FORMAT(5X,'HECOOTBETCTBàE:    HOMEPOB ÑETAãEâ  à  OÅOáHAóEHàâ',   OHTP0302
     12(/36X,I5,5X,10A2))                                               OHTP0303
102   FORMAT(5X,'HìãEBOâ HOMEP MAPò-TEXH. KAPTõ B CèEñàîàKAñàà')        OHTP0304
  101 FORMAT(5X,'B CèEñ-ñàà áAKAáA',1X,4A2,1X,'èO CÅOPKE',1X,10A2,1X,'OTOHTP0305
     1CìTCTBìET áAÉãABHAü CTPOKA àãà B HEâ HEHìãEBOE KOã-BO')           OHTP0306
      END                                                               OHTP0307
      SUBROUTINE CHKPRT(LIMT,NA,NT,A,S,P,TQ,QZ,TQC,UG,NUG,IP,IERL,IERL5)OHTP0308
      INTEGER*2 A(26,1),S(2,1),TQ(6,1),TQC(4,1)                         OHTP0309
      INTEGER*2 P(2,1),UG(20,1)                                         OHTP0310
      INTEGER QZ(4,1),Q(40)                                             OHTP0311
      NA1=NA+1                                                          OHTP0312
      DO 1 J=2,NA                                                       OHTP0313
      IF(A(13,J).GT.0) GOTO 1                                           OHTP0314
      NT=NT+1                                                           OHTP0315
      NA1=NA1-1                                                         OHTP0316
      A(1,NA1)=J                                                        OHTP0317
      A(12,J)=NT                                                        OHTP0318
      A(13,J)=1                                                         OHTP0319
      IF(NT.GT.LIMT) STOP 41                                            OHTP0320
      IQ=0                                                              OHTP0321
      J1=A(14,J)                                                        OHTP0322
      J2=A(15,J)+J1-1                                                   OHTP0323
      DO 2 J3=J1,J2                                                     OHTP0324
    2 IQ=IQ+P(2,J3)                                                     OHTP0325
      TQ(1,NT)=NUG                                                      OHTP0326
      TQ(2,NT)=0                                                        OHTP0327
      TQ(3,NT)=NA1                                                      OHTP0328
      TQ(4,NT)=-1                                                       OHTP0329
      TQ(5,NT)=-1                                                       OHTP0330
      TQ(6,NT)=-1                                                       OHTP0331
      QZ(1,NT)=1                                                        OHTP0332
      QZ(2,NT)=0                                                        OHTP0333
      TQC(1,NT)=-1000                                                   OHTP0334
      TQC(2,NT)=IQ                                                      OHTP0335
      TQC(3,NT)=0                                                       OHTP0336
      TQC(4,NT)=-1                                                      OHTP0337
      WRITE(9,109) A(9,J)                                               OHTP0338
      IERL=-1                                                           OHTP0339
    1 CONTINUE                                                          OHTP0340
      J1=0                                                              OHTP0341
      J2=0                                                              OHTP0342
      J3=0                                                              OHTP0343
      DO 3 J=2,NT                                                       OHTP0344
      IF(QZ(1,J).LE.0) QZ(1,J)=1                                        OHTP0345
      J1=J1+QZ(1,J)                                                     OHTP0346
      IF(TQ(1,J).LT.NUG) GOTO 3                                         OHTP0347
      J2=J2+QZ(1,J)                                                     OHTP0348
      J3=J3+1                                                           OHTP0349
    3 CONTINUE                                                          OHTP0350
      RERL=IP/100.0                                                     OHTP0351
      IP=1                                                              OHTP0352
      IF((J3+0.0)/NT.GT.RERL.OR.(J2+0.0)/J1.GT.RERL) IP=-1              OHTP0353
      IF(IP.GE.0) GOTO 4                                                OHTP0354
      IF(IERL5.NE.-1) GOTO 4                                            OHTP0355
C     ***** PRINT FOR CHECK:                                            OHTP0356
      WRITE(9,37)                                                       OHTP0357
      WRITE(9,100)                                                      OHTP0358
      DO 29 J=1,NA                                                      OHTP0359
      J1=A(10,J)                                                        OHTP0360
      J2=A(11,J)+J1-1                                                   OHTP0361
      IF(J2.LT.J1) GOTO 29                                              OHTP0362
      WRITE(9,105)                                                      OHTP0363
      WRITE(9,101) (A(I,J),I=17,26),A(2,J),A(9,J)                       OHTP0364
      J5=0                                                              OHTP0365
      DO 30 J3=J1,J2                                                    OHTP0366
      J5=J5+1                                                           OHTP0367
      J4=S(1,J3)                                                        OHTP0368
      D1=A(6,J4)/10.                                                    OHTP0369
      D2=A(7,J4)/10.                                                    OHTP0370
      D3=A(8,J4)/10.                                                    OHTP0371
      L=(A(3,J4)*1000+A(4,J4))*1000+A(5,J4)                             OHTP0372
      WRITE(9,102) J5,(A(I,J4),I=17,26),S(2,J3),L,D1,D2,D3,             OHTP0373
     1A(2,J4),A(9,J4)                                                   OHTP0374
   30 CONTINUE                                                          OHTP0375
   29 CONTINUE                                                          OHTP0376
      PRINT 105                                                         OHTP0377
C     *****PRINT FOR CHECK:                                             OHTP0378
   10 I1=0                                                              OHTP0379
      WRITE(9,38)                                                       OHTP0380
      WRITE(9,107)                                                      OHTP0381
      DO 32 J=1,NT                                                      OHTP0382
      IF(TQ(3,J).EQ.I1) GOTO 33                                         OHTP0383
      WRITE(9,108)                                                      OHTP0384
      JJ=0                                                              OHTP0385
      I1=TQ(3,J)                                                        OHTP0386
      IF(I1.LE.0) GOTO 50                                               OHTP0387
      NK=I1/1000                                                        OHTP0388
      NN=I1-NK*1000                                                     OHTP0389
      NK=NK+NN                                                          OHTP0390
      J1=0                                                              OHTP0391
      DO 35 J2=NN,NK                                                    OHTP0392
      J1=J1+1                                                           OHTP0393
      J3=A(1,J2)                                                        OHTP0394
   35 Q(J1)=A(2,J3)                                                     OHTP0395
      GOTO 36                                                           OHTP0396
   50 J1=1                                                              OHTP0397
      Q(1)=0                                                            OHTP0398
      DO 51 KK=2,NA                                                     OHTP0399
      IF(A(12,KK).EQ.J) GOTO 52                                         OHTP0400
   51 CONTINUE                                                          OHTP0401
   52 J3=KK                                                             OHTP0402
   36 WRITE(9,104) A(9,J3),(Q(J2),J2=1,J1)                              OHTP0403
   33 J1=TQ(1,J)                                                        OHTP0404
      IO=UG(1,J1)*10000+TQ(2,J)                                         OHTP0405
      R=QZ(2,J)/100.0                                                   OHTP0406
      T=QZ(1,J)/100.0                                                   OHTP0407
      JJ=JJ+1                                                           OHTP0408
      WRITE(9,103) JJ,IO,TQC(2,J),T,R                                   OHTP0409
   32 CONTINUE                                                          OHTP0410
      WRITE(9,108)                                                      OHTP0411
    4 RETURN                                                            OHTP0412
   37 FORMAT(//7X,'CèEñàîàKAñàü áAKAáA:')                               OHTP0413
   38 FORMAT(//7X,'MA4òPìTHõE KAPTõ áAKAáA:')                           OHTP0414
  100 FORMAT(//5X,92(1H-)/5X,8H:      :,20X,8H:      :,15X,41H: PAáMEPõ OHTP0415
     1áAÉOTOBOK (MM)  :     :       :/5X,52H:HOMEP :    OÅOáHAóEHàE     OHTP0416
     2:KOã-BO:     òàîP      :,25(1H-),15H:HOMEP: HOMEP :/5X,8H: è/è  :,OHTP0417
     320X,64H:      :   MATEPàAãA   : ÑãàHA : òàPàHA  :TOãôàHA: ÑET.:M/KOHTP0418
     4APTõ:)                                                            OHTP0419
  101 FORMAT(5X,8H:CÅOPKA:,10A2,8H:      :,15X,27H:       :         :   OHTP0420
     1    !,I5,3H!  ,I5,1H!)                                            OHTP0421
  102 FORMAT(5X,2H! ,I4,2H !,10A2,2H! ,I5,1H!,I15,1H!,F7.1,1H!,F9.1,1H!,OHTP0422
     1F7.1,1H!,I5,3H!  ,I5,1H!)                                         OHTP0423
  103 FORMAT(5X,1H!,55X,2H! ,I3,3H ! ,I8,4H   !,I5,12H !      !   ,F6.2,OHTP0424
     14H   !,F9.2)                                                      OHTP0425
  104 FORMAT(5X,10H: M/KAPTA ,I4,10H Ñãü ÑET.:,5I6/(5X,1H:,23X,5I6))    OHTP0426
  105 FORMAT(5X,8H!------!,20(1H-),8H!------!,15(1H-),41H!-------!------OHTP0427
     1---!-------!-----!-------!)                                       OHTP0428
  107 FORMAT(//61X,56(1H-)/61X,56H:HOMEP:    òàîP    :KOã-BO:PAáPüÑ:TPìÑOHTP0429
     1OEMKOCTú:PACñEHKA:/61X,56H: è/è :OÅOPìÑOBAHàü:      :PAÅOTõ:   (H/OHTP0430
     2ó.)   : (PìÅ.) :)                                                 OHTP0431
  108 FORMAT(5X,1H!,55(1H-),56H!-----!------------!------!------!-------OHTP0432
     1-----!--------!)                                                  OHTP0433
  109 FORMAT(5X,'B TEXHOãOÉàà OTCìTCTBìET MAPòPìTHAü KAPTA ',I2)        OHTP0434
      END                                                               OHTP0435
      SUBROUTINE TCLZKZ(NF,LIMT,NU,NUG,NA,NT,U,UG,A,TQ,TQC,QZ,NK,NKMAX, OHTP0436
     1IERL,ZAK2,NXZ,IERL5,F1)
      CHARACTER*1 F1                                                    OHTP0437
      INTEGER*2 NXZ(10,1)                                               OHTP0438
      INTEGER*2 ZAK2(4,32)                                              OHTP0439
      INTEGER*2 CON/'  '/                                               OHTP0440
      INTEGER*2 A(26,1),TQ(6,1),TQC(4,1)                                OHTP0441
      INTEGER*2 UG(20,1)                                                OHTP0442
      INTEGER U(2,1)                                                    OHTP0443
      INTEGER QZ(4,1)                                                   OHTP0444
      INTEGER*2 ZAK1(10),ZAK(10)/10*0/                                  OHTP0445
    1 FORMAT(A1,20X,I2,4X,I2,I6,I5,I5,I9)                               OHTP0446
    2 FORMAT(A1,10A2,I2,4X,I2,I6,I5,I5,I9,47X)                          OHTP0447
      nk=nk+1    
      IERL=0                                                            OHTP0448
      IERL5=0                                                           OHTP0449
      A(1,1)=1                                                          OHTP0450
      TQ(1,1)=NUG                                                       OHTP0451
      TQ(2,1)=0                                                         OHTP0452
      TQ(3,1)=1                                                         OHTP0453
      TQ(4,1)=-1                                                        OHTP0454
      TQ(5,1)=-1                                                        OHTP0455
      TQ(6,1)=-1                                                        OHTP0456
      QZ(1,1)=1                                                         OHTP0457
      QZ(2,1)=0                                                         OHTP0458
      TQC(1,1)=-1000                                                    OHTP0459
      TQC(2,1)=1                                                        OHTP0460
      TQC(3,1)=0                                                        OHTP0461
      TQC(4,1)=-1                                                       OHTP0462
      NT=1                                                              OHTP0463
      NT1=1                                                             OHTP0464
      IB=0                                                              OHTP0465
      MK=0                                                              OHTP0466
      JTM=0                                                             OHTP0467
      KOB=0                                                             OHTP0468
      DO 70 I=1,10                                                      OHTP0469
   70 ZAK(I)=0                                                          OHTP0470
   35 MK1=MK                                                            OHTP0471
      JTM1=JTM                                                          OHTP0472
      DO 40 I=1,10                                                      OHTP0473
   40 ZAK1(I)=ZAK(I)                                                    OHTP0474
   22 READ(NF,2,REC=NK,ERR=10,END=10) JFM,ZAK,MK,JTM,IOB,IQ,ITR,IRC     OHTP0475
      IF(JFM.NE.F1) GOTO 10                                             OHTP0476
      IF(JTM.NE.1) GOTO 36                                              OHTP0477
C     *****ARRANGEMENT:                                                 OHTP0478
      DO 302 I=1,4                                                      OHTP0479
      IF(ZAK1(I).NE.0) GOTO 214                                         OHTP0480
  302 CONTINUE                                                          OHTP0481
      DO 207 I=1,4                                                      OHTP0482
      IF(NXZ(I,1).NE.ZAK(I)) GOTO 208                                   OHTP0483
  207 CONTINUE                                                          OHTP0484
      GOTO 214                                                          OHTP0485
  208 WRITE(9,209) (NXZ(I,1),I=1,4)                                     OHTP0486
  209 FORMAT(5X,'B áAKAáE',1X,4A2,1X,'OTCìTCTBìET TEXHOãOÉàü')          OHTP0487
      DO 106 J=1,32                                                     OHTP0488
      IF(ZAK2(1,J).EQ.CON) GOTO 107                                     OHTP0489
      DO 108 I=1,4                                                      OHTP0490
      IF(ZAK2(I,J).NE.NXZ(I,1)) GOTO 106                                OHTP0491
  108 CONTINUE                                                          OHTP0492
      GOTO 210                                                          OHTP0493
  106 CONTINUE                                                          OHTP0494
  107 DO 109 I=1,4                                                      OHTP0495
      ZAK2(I,J)=NXZ(I,1)                                                OHTP0496
  109 CONTINUE                                                          OHTP0497
  210 IERL=-1                                                           OHTP0498
      DO 211 I=1,4                                                      OHTP0499
      NXZ(I,1)=ZAK(I)                                                   OHTP0500
  211 CONTINUE                                                          OHTP0501
      WRITE(9,212) (NXZ(I,1),I=1,10)                                    OHTP0502
  212 FORMAT(///5X,'B áAKAáE ',10A2,'OTCìTCTBìET CèEñàîàKAñàü')         OHTP0503
  214 CONTINUE                                                          OHTP0504
      IF(MK1.EQ.MK) GOTO 60                                             OHTP0505
   11 DO 41 I=1,10                                                      OHTP0506
      IF(ZAK1(I).NE.0) GOTO 42                                          OHTP0507
   41 CONTINUE                                                          OHTP0508
      GOTO 45                                                           OHTP0509
   42 DO 43 I=1,4                                                       OHTP0510
      IF(ZAK1(I).NE.ZAK(I)) GOTO 151                                    OHTP0511
   43 CONTINUE                                                          OHTP0512
   45 NT2=NT1+1                                                         OHTP0513
      DO 13 J=1,NA                                                      OHTP0514
      IF(A(13,J).GT.0.OR.A(9,J).NE.MK) GOTO 13                          OHTP0515
   14 NT1=NT1+1                                                         OHTP0516
      A(1,NT1)=J                                                        OHTP0517
      A(12,J)=NT+1                                                      OHTP0518
   13 CONTINUE                                                          OHTP0519
      IF(NT1.GE.NT2) GOTO 19                                            OHTP0520
      IERL=-1                                                           OHTP0521
   17 WRITE(9,101) MK                                                   OHTP0522
   21 NK=NK+1                                                           OHTP0523
      IF(NK.GT.NKMAX) GOTO 10                                           OHTP0524
      MK1=MK                                                            OHTP0525
      DO 303 I=1,10                                                     OHTP0526
  303 ZAK1(I)=ZAK(I)                                                    OHTP0527
      READ (NF,2,REC=NK,ERR=10,END=10) JFM,ZAK,MK,JTM,IOB,IQ,ITR,IRC    OHTP0528
      IF(JFM.NE.F1) GOTO 10                                             OHTP0529
      IF(JTM.EQ.1) GOTO11                                               OHTP0530
      GOTO37                                                            OHTP0531
   19 IB=(NT1-NT2)*1000+NT2                                             OHTP0532
   12 IF(IB.LE.0) GOTO 17                                               OHTP0533
      IF(JTM.EQ.1) ICP=NT                                               OHTP0534
      IF(JTM.EQ.1) GOTO 24                                              OHTP0535
      IF((NT+1-ICP-(NT+1-ICP)/10*10).EQ.0) GOTO 24                      OHTP0536
      IF(JTM1+1.EQ.JTM) GOTO 24                                         OHTP0537
      JTM2=JTM                                                          OHTP0539
  404 DO 400 J=1,32                                                     OHTP0541
      IF(ZAK2(1,J).EQ.CON) GOTO 401                                     OHTP0542
      DO 402 I=1,4                                                      OHTP0543
      IF(ZAK(I).NE.ZAK2(I,J)) GOTO 400                                  OHTP0544
  402 CONTINUE                                                          OHTP0545
      GOTO 403                                                          OHTP0546
  400 CONTINUE                                                          OHTP0547
  401 WRITE(9,301)MK,JTM1,JTM2                                          OHTP0548
      IERL=-1                                                           OHTP0549
C     IERL5=-1                                                          OHTP0550
      GOTO 24                                                           OHTP0551
  403 WRITE(9,301) MK,JTM1,JTM2                                         OHTP0552
      IERL=-1                                                           OHTP0553
   24 NT=NT+1                                                           OHTP0554
      IF(NT.GT.LIMT) IERL=-1                                            OHTP0555
      IF(NT.GT.LIMT) KOLTX=NT-LIMT                                      OHTP0556
      IF(NT.GT.LIMT) WRITE(9,213) (NXZ(I,1),I=1,4),KOLTX                OHTP0557
      IF(NT.GT.LIMT) LIMT=1000                                          OHTP0558
  213 FORMAT(5X,'B áAKAáE ',4A2,' ÅOãúòE ÑOèìCTàMOÉO óàCãA CTPOK B TEXHOOHTP0559
     1ãOÉàà HA',1X,I3)                                                  OHTP0560
   28 DO 30 J1=NT2,NT1                                                  OHTP0561
      J2=A(1,J1)                                                        OHTP0562
   30 A(13,J2)=A(13,J2)+1                                               OHTP0563
   29 TQ(1,NT)=NUG-1                                                    OHTP0564
      TQ(2,NT)=IOB-IOB/10000*10000                                      OHTP0565
      TQ(3,NT)=IB                                                       OHTP0566
      TQ(4,NT)=-1                                                       OHTP0567
      TQ(5,NT)=-1                                                       OHTP0568
      TQ(6,NT)=-1                                                       OHTP0569
      TQC(1,NT)=-1000                                                   OHTP0570
      TQC(3,NT)=0                                                       OHTP0571
      TQC(4,NT)=-1                                                      OHTP0572
      DO 31 J1=1,NU                                                     OHTP0573
      IF(IOB.EQ.U(1,J1)) TQ(1,NT)=U(2,J1)                               OHTP0574
   31 CONTINUE                                                          OHTP0575
      IF(TQ(1,NT).EQ.NUG) KOB=1                                         OHTP0576
      TQC(2,NT)=IQ                                                      OHTP0577
      QZ(1,NT)=ITR                                                      OHTP0578
      QZ(2,NT)=IRC                                                      OHTP0579
      NK=NK+1                                                           OHTP0580
      GOTO 35                                                           OHTP0581
  36  IF(MK1.EQ.MK) GOTO 12                                             OHTP0582
      DO 80 I=1,4                                                       OHTP0583
      IF(ZAK1(I).NE.ZAK(I)) GOTO 81                                     OHTP0584
   80 CONTINUE                                                          OHTP0585
      WRITE(9,102) MK                                                   OHTP0586
      IERL=-1                                                           OHTP0587
      GOTO 12                                                           OHTP0588
   81 WRITE(9,104) MK,(ZAK(I),I=1,10)                                   OHTP0589
      WRITE(9,111) ZAK                                                  OHTP0590
      GOTO 152                                                          OHTP0591
  37  IF(MK1.EQ.MK) GOTO 21                                             OHTP0592
      WRITE(*,102) MK                                                   OHTP0593
      IERL=-1                                                           OHTP0594
      GOTO 21                                                           OHTP0595
   60 DO 200 I=1,4                                                      OHTP0596
      IF(ZAK(I).NE.ZAK1(I)) GOTO 201                                    OHTP0597
 200  CONTINUE                                                          OHTP0598
      GOTO 202                                                          OHTP0599
 201  WRITE(9,111) ZAK                                                  OHTP0600
      DO 206 J=1,32                                                     OHTP0601
      IF(ZAK2(1,J).EQ.CON) GOTO 203                                     OHTP0602
      DO 204 I=1,4                                                      OHTP0603
      IF(ZAK(I).NE.ZAK2(I,J)) GOTO 206                                  OHTP0604
 204  CONTINUE                                                          OHTP0605
      GOTO 12                                                           OHTP0606
 206  CONTINUE                                                          OHTP0607
 203  DO 205 I=1,4                                                      OHTP0608
 205  ZAK2(I,J)=ZAK(I)                                                  OHTP0609
      GOTO 12                                                           OHTP0610
 202  WRITE(9,103) MK                                                   OHTP0611
      IERL=-1                                                           OHTP0612
C     IERL5=-1                                                          OHTP0613
      GOTO 12                                                           OHTP0614
  151 WRITE(9,111) ZAK                                                  OHTP0615
  152 DO 153 J=1,32                                                     OHTP0616
      IF(ZAK2(1,J).EQ.CON) GOTO 156                                     OHTP0617
      DO 154 I=1,4                                                      OHTP0618
      IF(ZAK(I).NE.ZAK2(I,J)) GOTO 153                                  OHTP0619
  154 CONTINUE                                                          OHTP0620
      GOTO 21                                                           OHTP0621
  153 CONTINUE                                                          OHTP0622
  156 DO 155 I=1,4                                                      OHTP0623
  155 ZAK2(I,J)=ZAK(I)                                                  OHTP0624
      GOTO 21                                                           OHTP0625
   10 IF(KOB.EQ.0) GOTO 50                                              OHTP0626
      WRITE(9,110)                                                      OHTP0627
   50 RETURN                                                            OHTP0628
  101 FORMAT(5X,'B CèEñàîàKAñàà OTCìTCTBìET MAPòPìTHAü KAPTA',I2)       OHTP0629
  104 FORMAT(5X,'B MAPòPìTHOâ KAPTE ',I3,' OTCìTCTBìET 1-Aü OèEPAñàü èO OHTP0630
     1áAKAáì ',10A2)                                                    OHTP0631
  103 FORMAT(5X,'B MAPòPìTHOâ KAPTE',I3,' HEBEPEH èOPüÑOK OèEPAñàâ')    OHTP0632
  102 FORMAT(5X,'B MAPòPìTHOâ KAPTE',I3,' OTCìTCTBìET 1-Aü OèEPAñàü')   OHTP0633
  110 FORMAT(5X'èPOBEPàTú èPABàãúHOCTú OèEPAñàâ C òàîPOM OÅOPìÑOBAHàü 99OHTP0634
     1....')                                                            OHTP0635
  111 FORMAT(//10X,'****èPOBEPàTú ÑAHHõE Ñãü áAKAáA ',10A2)             OHTP0636
  301 FORMAT(5X,'B MAPòPìTHOâ KAPTE',I3,' HEBEPHA èOCãEÑOBATEãúHOCTú OèEOHTP0637
     1PAñàâ èO HOMEPAM',I4,I4)                                          OHTP0638
      END                                                               OHTP0639
      SUBROUTINE STRPRM(LIMS,NA,NS,NXZ,A,S,P,IS,IERL1)                  OHTP0640
      INTEGER*2 A(26,1),S(2,1),P(2,1),W1(100),W2(100)                   OHTP0641
      INTEGER*2 NXZ(10)                                                 OHTP0642
      DO 14 J=2,26                                                      OHTP0643
      A(J,1)=0                                                          OHTP0644
      IF(J.GT.16) A(J,1)=NXZ(J-16)                                      OHTP0645
   14 CONTINUE                                                          OHTP0646
      A(10,1)=NS+1                                                      OHTP0647
      A(12,1)=1                                                         OHTP0648
      A(13,1)=1                                                         OHTP0649
      NP=0                                                              OHTP0650
      DO 11 JA=2,NA                                                     OHTP0651
      A(15,JA)=0                                                        OHTP0652
      IP=NP+1                                                           OHTP0653
      DO 12 J=2,NA                                                      OHTP0654
      J1=A(10,J)                                                        OHTP0655
      J2=A(11,J)+J1-1                                                   OHTP0656
      IF(J1.GT.J2) GOTO 12                                              OHTP0657
      DO 13 JS=J1,J2                                                    OHTP0658
      IF(S(1,JS).NE.JA) GOTO 13                                         OHTP0659
      A(14,JA)=IP                                                       OHTP0660
      A(15,JA)=A(15,JA)+1                                               OHTP0661
      NP=NP+1                                                           OHTP0662
      P(1,NP)=J                                                         OHTP0663
      P(2,NP)=S(2,JS)                                                   OHTP0664
   13 CONTINUE                                                          OHTP0665
   12 CONTINUE                                                          OHTP0666
      IF(A(15,JA).GT.0) GOTO 11                                         OHTP0667
      NS=NS+1                                                           OHTP0668
      IF(NS.GT.LIMS) IERL1=-1                                           OHTP0669
      IF(NS.GT.LIMS) WRITE(9,200) (NXZ(I),I=1,4)                        OHTP0670
  200 FORMAT(5X,' B áAKAáE ',4A2,'ÅOãúòE ÑOèìCTàMOÉO óàCãO èOÑCÅOPOK B COHTP0671
     1èEñàîàKAñàà')                                                     OHTP0672
      IF(NS.GT.LIMS) LIMS=500                                           OHTP0673
      S(1,NS)=JA                                                        OHTP0674
   15 S(2,NS)=IS                                                        OHTP0675
      A(11,1)=A(11,1)+1                                                 OHTP0676
      NP=NP+1                                                           OHTP0677
      A(14,JA)=NP                                                       OHTP0678
      A(15,JA)=1                                                        OHTP0679
      P(1,NP)=1                                                         OHTP0680
      P(2,NP)=IS                                                        OHTP0681
   11 CONTINUE                                                          OHTP0682
      DO 1 J1=1,NA                                                      OHTP0683
    1 A(16,J1)=A(15,J1)                                                 OHTP0684
      J0=1                                                              OHTP0685
      J=0                                                               OHTP0686
    2 J=J+1                                                             OHTP0687
      W1(J)=J0                                                          OHTP0688
      W2(J)=A(11,J0)                                                    OHTP0689
      J1=A(14,J0)                                                       OHTP0690
      J2=A(15,J0)+J1-1                                                  OHTP0691
      JM=-1                                                             OHTP0692
      IF(J2.LT.J1) GOTO 3                                               OHTP0693
      JM=0                                                              OHTP0694
      DO 4 J3=J1,J2                                                     OHTP0695
      J4=P(1,J3)                                                        OHTP0696
      P(2,J3)=-P(2,J3)*A(16,J4)                                         OHTP0697
    4 JM=JM-P(2,J3)                                                     OHTP0698
    3 A(16,J0)=JM                                                       OHTP0699
    5 W2(J)=W2(J)-1                                                     OHTP0700
      IF(W2(J).LT.0) GOTO 6                                             OHTP0701
      J0=A(10,J0)+W2(J)                                                 OHTP0702
      J0=S(1,J0)                                                        OHTP0703
      A(16,J0)=A(16,J0)-1                                               OHTP0704
      IF(A(16,J0).LE.0) GOTO 2                                          OHTP0705
    7 J0=W1(J)                                                          OHTP0706
      GOTO 5                                                            OHTP0707
    6 J=J-1                                                             OHTP0708
      IF(J.GT.0) GOTO 7                                                 OHTP0709
      RETURN                                                            OHTP0710
      END                                                               OHTP0711
      SUBROUTINE SGOKIZ(N,NT,A,S,P,TQ,QZ,TQC,U,IERL2)                   OHTP0712
      INTEGER*2 A(26,1),S(2,1),P(2,1),TQ(6,1),TQC(4,1),W1(100),W2(100)  OHTP0713
      INTEGER*2 U(20,1)                                                 OHTP0714
      INTEGER QZ(4,1)                                                   OHTP0715
      IERL2=0                                                           OHTP0716
      IND=1                                                             OHTP0717
C     *****LATE STARTS:                                                 OHTP0718
      DO 1 J=1,N                                                        OHTP0719
    1 A(16,J)=A(11,J)                                                   OHTP0720
      J=0                                                               OHTP0721
      J0=1                                                              OHTP0722
      JT=1                                                              OHTP0723
    2 J=J+1                                                             OHTP0724
      NN=TQ(3,J0)                                                       OHTP0725
      NK=NN/1000                                                        OHTP0726
      W2(J)=NK+1                                                        OHTP0727
      NN=NN-NK*1000                                                     OHTP0728
      W1(J)=NN                                                          OHTP0729
      NK=NN+NK                                                          OHTP0730
      JN=0                                                              OHTP0731
      JM=-1                                                             OHTP0732
      IF(J.LE.1) GOTO 3                                                 OHTP0733
      DO 21 J7=1,2                                                      OHTP0734
      DO 4 J1=NN,NK                                                     OHTP0735
      J4=A(1,J1)                                                        OHTP0736
      J2=A(14,J4)                                                       OHTP0737
      J3=A(15,J4)+J2-1                                                  OHTP0738
      DO 5 J4=J2,J3                                                     OHTP0739
      J5=P(1,J4)                                                        OHTP0740
      J5=A(12,J5)                                                       OHTP0741
      IF(J5.EQ.J0) GOTO 5                                               OHTP0742
      J6=TQ(4,J5)                                                       OHTP0743
      IF(J6.LT.0) GOTO 6                                                OHTP0744
      IF(J7.GT.1) GOTO 42                                               OHTP0745
      QZ(4,J5)=1                                                        OHTP0746
      GOTO 5                                                            OHTP0747
   42 IF(QZ(4,J5).LT.0) GOTO 5                                          OHTP0748
      QZ(4,J5)=-1                                                       OHTP0749
      IF(J6.GT.JM) JM=J6                                                OHTP0750
      JN=JN+QZ(3,J5)                                                    OHTP0751
    5 CONTINUE                                                          OHTP0752
    4 CONTINUE                                                          OHTP0753
   21 CONTINUE                                                          OHTP0754
    3 J1=J0+JT                                                          OHTP0755
      DO 7 J2=1,JT                                                      OHTP0756
      J1=J1-1                                                           OHTP0757
      QZ(3,J1)=JN+J2                                                    OHTP0758
      JM=JM+1                                                           OHTP0759
      TQ(6,J1)=JM                                                       OHTP0760
      J3=TQ(1,J1)                                                       OHTP0761
      JM=JM+U(6,J3)                                                     OHTP0762
      TQ(5,J1)=JM                                                       OHTP0763
      R=(QZ(1,J1)+0.0)/U(2,J3)                                          OHTP0764
      J4=INT(R)                                                         OHTP0765
      IF(R-J4.LE.0.0) J4=J4-1                                           OHTP0766
      JM=JM+J4                                                          OHTP0767
    7 TQ(4,J1)=JM                                                       OHTP0768
    8 W2(J)=W2(J)-1                                                     OHTP0769
      IF(W2(J).LT.0) GOTO 6                                             OHTP0770
   11 J0=NN+W2(J)                                                       OHTP0771
      J1=A(1,J0)                                                        OHTP0772
    9 A(16,J1)=A(16,J1)-1                                               OHTP0773
      IF(A(16,J1).LT.0) GOTO 8                                          OHTP0774
      J0=A(10,J1)+A(16,J1)                                              OHTP0775
      J0=S(1,J0)                                                        OHTP0776
      JT=A(13,J0)                                                       OHTP0777
      J0=A(12,J0)                                                       OHTP0778
      IF(TQ(4,J0)) 2,9,9                                                OHTP0779
    6 J=J-1                                                             OHTP0780
      IF(J.LE.0) GOTO 10                                                OHTP0781
      NN=W1(J)                                                          OHTP0782
      GOTO 11                                                           OHTP0783
C     *****EMPTY BLOCKS CANCELLATION:                                   OHTP0784
   10 J=J+1                                                             OHTP0785
   31 IF(J.GT.N) GOTO 12                                                OHTP0786
      J2=A(13,J)                                                        OHTP0787
      J1=A(12,J)+J2-1                                                   OHTP0788
      IF(TQ(6,J1).GE.0) GOTO 10                                         OHTP0789
      IF(IND.GT.0) WRITE(9,100)                                         OHTP0790
      IF(IND.GT.0) IERL2=-1                                             OHTP0791
      IND=-1                                                            OHTP0792
      NN=TQ(3,J1)                                                       OHTP0793
      NK=NN/1000                                                        OHTP0794
      NN=NN-NK*1000                                                     OHTP0795
      NK=NN+NK                                                          OHTP0796
      J3=0                                                              OHTP0797
      DO 13 J4=NN,NK                                                    OHTP0798
      J3=J3+1                                                           OHTP0799
      J5=A(1,J4)                                                        OHTP0800
      W1(J3)=J5                                                         OHTP0801
   13 W2(J3)=A(2,J5)                                                    OHTP0802
      WRITE(9,101) A(9,J),(W2(J4),J4=1,J3)                              OHTP0803
      DO 14 J4=1,N                                                      OHTP0804
      J5=0                                                              OHTP0805
      J6=A(1,J4)                                                        OHTP0806
      DO 15 J7=1,J3                                                     OHTP0807
      IF(J6-W1(J7)) 16,16,15                                            OHTP0808
   15 J5=J5+1                                                           OHTP0809
   16 J7=J4                                                             OHTP0810
      IF(J7.GT.NK) J7=J7-J3                                             OHTP0811
   14 A(1,J7)=J6-J5                                                     OHTP0812
      DO 17 J4=1,NT                                                     OHTP0813
      J5=J4                                                             OHTP0814
      IF(J4.LE.J1) GOTO 18                                              OHTP0815
      J5=J4-J2                                                          OHTP0816
      DO 19 J6=1,4                                                      OHTP0817
      TQ(J6,J5)=TQ(J6,J4)                                               OHTP0818
      QZ(J6,J5)=QZ(J6,J4)                                               OHTP0819
   19 TQC(J6,J5)=TQC(J6,J4)                                             OHTP0820
      TQ(5,J5)=TQ(5,J4)                                                 OHTP0821
      TQ(6,J5)=TQ(6,J4)                                                 OHTP0822
   18 J6=TQ(3,J5)                                                       OHTP0823
      J7=J6-J6/1000*1000                                                OHTP0824
      IF(J7.GT.NK) J6=J6-J3                                             OHTP0825
   17 TQ(3,J5)=J6                                                       OHTP0826
      J5=-1                                                             OHTP0827
      J6=0                                                              OHTP0828
      NK=J3+1                                                           OHTP0829
      DO 20 J7=1,NK                                                     OHTP0830
      J4=J5+2                                                           OHTP0831
      J5=N                                                              OHTP0832
      IF(J7.LT.NK) J5=W1(J7)-1                                          OHTP0833
      IF(J4.GT.J5) GOTO 20                                              OHTP0834
      DO 22 J8=J4,J5                                                    OHTP0835
      J6=J6+1                                                           OHTP0836
      IF(J6.GE.J8) GOTO 23                                              OHTP0837
      DO 24 J9=2,26                                                     OHTP0838
   24 A(J9,J6)=A(J9,J8)                                                 OHTP0839
   23 IF(A(12,J6).GT.J1) A(12,J6)=A(12,J6)-J2                           OHTP0840
      K1=A(10,J6)                                                       OHTP0841
      K2=A(11,J6)+K1-1                                                  OHTP0842
      IF(K1.GT.K2) GOTO 25                                              OHTP0843
      K3=K1-1                                                           OHTP0844
      DO 26 K4=K1,K2                                                    OHTP0845
      K5=0                                                              OHTP0846
      K6=S(1,K4)                                                        OHTP0847
      DO 27 K7=1,J3                                                     OHTP0848
      IF(W1(K7)-K6) 27,26,28                                            OHTP0849
   27 K5=K5+1                                                           OHTP0850
   28 K3=K3+1                                                           OHTP0851
      S(1,K3)=K6-K5                                                     OHTP0852
      S(2,K3)=S(2,K4)                                                   OHTP0853
   26 CONTINUE                                                          OHTP0854
      A(11,J6)=K3-K1+1                                                  OHTP0855
   25 K1=A(14,J6)                                                       OHTP0856
      K2=A(15,J6)+K1-1                                                  OHTP0857
      IF(K1.GT.K2) GOTO 22                                              OHTP0858
      DO 29 K4=K1,K2                                                    OHTP0859
      K5=0                                                              OHTP0860
      K6=P(1,K4)                                                        OHTP0861
      DO 30 K7=1,J3                                                     OHTP0862
      IF(K6-W1(K7)) 29,29,30                                            OHTP0863
   30 K5=K5+1                                                           OHTP0864
   29 P(1,K4)=K6-K5                                                     OHTP0865
   22 CONTINUE                                                          OHTP0866
   20 CONTINUE                                                          OHTP0867
      N=N-J3                                                            OHTP0868
      NT=NT-J2                                                          OHTP0869
      GOTO 31                                                           OHTP0870
C     *****PARAMETERS OF TAILS:                                         OHTP0871
   12 DO 32 J=1,N                                                       OHTP0872
   32 A(16,J)=A(11,J)                                                   OHTP0873
      J=0                                                               OHTP0874
      J0=1                                                              OHTP0875
   33 J=J+1                                                             OHTP0876
      TQC(4,J0)=-2                                                      OHTP0877
      NN=TQ(3,J0)                                                       OHTP0878
      NK=NN/1000                                                        OHTP0879
      W2(J)=NK+1                                                        OHTP0880
      NN=NN-NK*1000                                                     OHTP0881
      W1(J)=NN                                                          OHTP0882
   34 W2(J)=W2(J)-1                                                     OHTP0883
      IF(W2(J).LT.0) GOTO 35                                            OHTP0884
   36 J0=NN+W2(J)                                                       OHTP0885
      J1=A(1,J0)                                                        OHTP0886
   37 A(16,J1)=A(16,J1)-1                                               OHTP0887
      IF(A(16,J1).LT.0) GOTO 34                                         OHTP0888
      J2=A(10,J1)+A(16,J1)                                              OHTP0889
      J2=S(1,J2)                                                        OHTP0890
      J0=A(12,J2)                                                       OHTP0891
      IF(TQC(4,J0)+1) 37,33,33                                          OHTP0892
   35 J1=A(1,NN)                                                        OHTP0893
      J0=A(12,J1)                                                       OHTP0894
      JT=A(13,J1)+J0-1                                                  OHTP0895
      NK=NN+TQ(3,J0)/1000                                               OHTP0896
      JN=0                                                              OHTP0897
      JM=0                                                              OHTP0898
      DO 43 J7=1,2                                                      OHTP0899
      DO 38 J1=NN,NK                                                    OHTP0900
      J3=A(1,J1)                                                        OHTP0901
      J2=A(10,J3)                                                       OHTP0902
      J3=A(11,J3)+J2-1                                                  OHTP0903
      IF(J2.GT.J3) GOTO 38                                              OHTP0904
      DO 39 J4=J2,J3                                                    OHTP0905
      J5=S(1,J4)                                                        OHTP0906
      J6=A(12,J5)                                                       OHTP0907
      IF(J6.EQ.J0) GOTO 39                                              OHTP0908
      IF(J7.GT.1) GOTO 44                                               OHTP0909
      TQC(1,J6)=-2000                                                   OHTP0910
      GOTO 39                                                           OHTP0911
   44 IF(TQC(1,J6).GT.-1500) GOTO 39                                    OHTP0912
      TQC(1,J6)=-1000                                                   OHTP0913
      J6=J6+A(13,J5)-1                                                  OHTP0914
      IF(TQC(3,J6).GT.JM) JM=TQC(3,J6)                                  OHTP0915
      JN=JN+QZ(4,J6)                                                    OHTP0916
   39 CONTINUE                                                          OHTP0917
   38 CONTINUE                                                          OHTP0918
   43 CONTINUE                                                          OHTP0919
      DO 40 J1=J0,JT                                                    OHTP0920
      JM=JM+TQ(4,J1)-TQ(6,J1)+1                                         OHTP0921
      JN=JN+1                                                           OHTP0922
      TQC(3,J1)=JM                                                      OHTP0923
   40 QZ(4,J1)=JN                                                       OHTP0924
      J=J-1                                                             OHTP0925
      IF(J.LE.0) GOTO 41                                                OHTP0926
      NN=W1(J)                                                          OHTP0927
      GOTO 36                                                           OHTP0928
   41 RETURN                                                            OHTP0929
  100 FORMAT(/5X,31HB OóEPEÑHOCTà MAPòPìTOB- ñàKãõ;/5X,34Hàá'üTõ CãEÑìûôOHTP0930
     1àE MAPòPìTHõE KAPTõ:)                                             OHTP0931
  101 FORMAT(/5X,7HM/KAPTA,I3,18H, COÑEPÜàT ÑETAãà:,10I5/(33X,10I5))    OHTP0932
      END                                                               OHTP0933
