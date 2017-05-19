      INTEGER*2 A(26,200),S(2,200),P(2,200),TQ(6,500),TQC(4,500),NXZ(10,BNOM0001
     14),F(10)/'0 ','1 ','2 ','3 ','4 ','5 ','6 ','7 ','8 ','9 '/,Z/' г'BNOM0002
     2/,CO/'FF'/,U(20,100)                                              BNOM0003
      INTEGER*2 CP/'яо'/                                                BNOM0004
      INTEGER*2 ZD/'*'/,GZZI/'  '/                                      BNOM0005
      INTEGER*2 M11/'11'/,M12/'12'/                                     BNOM0006
      INTEGER*2 CTM                                                     BNOM0007
      INTEGER*2 PROBEL/'  '/,KDPG(500),MES,DAY                          BNOM0008
      INTEGER DATAV                                                     BNOM0009
      INTEGER*2 PR1/'бо'/,PR2/'гд'/,PR3/'го'/,PR4/'от'/,PR5/'ро'/       BNOM0010
      INTEGER*2 PR6/'к '/,PR7/'т '/,PR8/'хо'/                           BNOM0011
      INTEGER*2 KDP(180),KLD(1)                                         BNOM0012
      INTEGER*2 ZAK(4),NAIM(5),KLAS(10),MAS,PRIZ,HERT(10),ZEZ           BNOM0013
      INTEGER*2 PR,PRR                                                  BNOM0014
      INTEGER QZ(4,500),IW(100),KOT(13),BOT(6),UW(2,200)                BNOM0015
      INTEGER*2 P40/'40'/,GZZ,GZ/'яо'/                                  BNOM0016
      integer*1 LN(4)                                                   BNOM0017
      integer*1 D/' '/                                                  BNOM0018
      EQUIVALENCE (NXZ(4,1),LN(1))                                      BNOM0019
      integer*2 dummy2
      integer*4 dummy4,dummy
      data nf/1/
      open(3,file='F:\ASUIPW\tek_INF\obnom.dat',access='direct',recl=182
     1,form='formatted')
      DATA NF/1/                                                        BNOM0021
      open(666,file='tmp.txt')
      read(666,7778) nwid
 7778 format(i1)
      if(nwid.eq.0) goto 7701 
      read(666,7779) mot
 7779 format(i2)
 7701 NK=1                                                              BNOM0022
      open(4,file='F:\ASUIPW\tek_INF\u.dat',form='unformatted')
      READ(4) NW,NU                                                     BNOM0023
      READ(4) ((UW(I,J),I=1,2),J=1,NW),((U(I,J),I=1,20),J=1,NU)         BNOM0024
      open(2,file='F:\ASUIPW\tek_INF\plan.dat',form='unformatted')
      open(1,file='F:\ASUIPW\tek_INF\portfel.dat',form='unformatted') 
      READ(2) LP,NN,KK                                                  BNOM0025
      if(nwid.eq.0) goto 7700
      nm=(nn-nn/1000000*1000000)/10000
      if(nm.le.mot) goto 7700
      close(2)
      close(1)
      open(2,file='F:\ASUIPW\tek_INF\plan.dat',form='unformatted')
      open(1,file='F:\ASUIPW\tek_INF\portfel.dat',form='unformatted') 
      READ(2) LP,NN,KK  
 7700 LP1=2*LP                                                          BNOM0026
      READ(2) (KDP(I),I=1,LP1)                                          BNOM0027
      NKP=KK                                                            BNOM0028
      CALL KALDNI(NKP,0,KLD)                                            BNOM0029
      NKP=KLD(1)/10                                                     BNOM0030
      DO 200 I1=LP,LP1                                                  BNOM0031
      IF(NKP.EQ.KDP(I1)/10) GOTO  201                                   BNOM0032
  200 CONTINUE                                                          BNOM0033
  201 NKP=I1-LP                                                         BNOM0034
      CALL KALDNI(NN,250,KDPG)                                          BNOM0035
      LP=250                                                            BNOM0036
      MES=NN/10000-NN/1000000*100                                       BNOM0037
      DAY=NN-NN/10000*10000                                             BNOM0038
   60 IF(NF.EQ.1) GOTO 1                                                BNOM0039
      READ(NF) NXZ,(IW(I),I=1,NU),IQ,IP,ID,LD,LZ,LF,MP,MS,NA,NS,NT      BNOM0040
      IF(NA.LE.-1000) GOTO 61                                           BNOM0041
      GOTO 62                                                           BNOM0042
    1 READ(NF) NXZ,(IW(I),I=1,NU),IQ,IP,ID,LZ,MP,NA,NS,NT               BNOM0043
      IF(NA.LT.0) GOTO 20                                               BNOM0044
   62 READ(NF) ((A(I,J),I=1,26),J=1,NA)                                 BNOM0045
      READ(NF) ((S(I,J),I=1,2),J=1,NS),((P(I,J),I=1,2),J=1,NS)          BNOM0046
      READ(NF) ((TQ(I,J),I=1,6),J=1,NT),((QZ(I,J),I=1,4),J=1,NT)        BNOM0047
      READ(NF) ((TQC(I,J),I=1,4),J=1,NT)                                BNOM0048
      IF(IP.NE.0.AND.IP.NE.1.AND.IP.NE.2) GOTO 60                       BNOM0049
      IF(NXZ(5,1).EQ.CP) GOTO 60                                        BNOM0050
      LFF=LF                                                            BNOM0051
      CTM=0                                                             BNOM0052
      NKT=0                                                             BNOM0053
      DO 35 I=2,NT                                                      BNOM0054
      IF(TQC(2,I).EQ.0) GOTO 35                                         BNOM0055
      NKT=NKT+QZ(1,I)                                                   BNOM0056
   35 CONTINUE                                                          BNOM0057
      N=A(10,1)                                                         BNOM0058
      N=S(1,N)                                                          BNOM0059
      N=A(12,N)+A(13,N)-1                                               BNOM0060
      NM=TQ(2,N)/100                                                    BNOM0061
      N=TQ(1,N)                                                         BNOM0062
      NZ=U(1,N)                                                         BNOM0063
      IF(NZ.NE.17) GOTO 60                                              BNOM0064
   30 CONTINUE                                                          BNOM0078
      NTF=0
      IQM=TQC(2,1)
      IF(IQ.EQ.0) GOTO 100                                              BNOM0079
      IF(NXZ(5,1).EQ.CP) GOTO 100                                       BNOM0080
      KT=KT-(KT*QZ(2,1)*100/IQ+50)/100                                  BNOM0081
      ID=ID-(ID*QZ(2,1)*10/IQ+5)/10                                     BNOM0082
      IQ=IQ-QZ(2,1)
      NTF=ID*IQM/IQ 
      MP=MP/10000*100+((MP-MP/10000*10000)-(MP-MP/10000*10000)/100*100)
      IF(KT.LE.0) KT=0                                                  BNOM0084
      IF(ID.LE.0) ID=0                                                  BNOM0085
      IF(IQ.LE.0) IQ=0                                                  BNOM0086
  100 CONTINUE                                                          BNOM0087
      IF(NF.EQ.1) LD=0                                                  BNOM0089
      IF(NF.EQ.1) DATAV=0                                               BNOM0090
      IF(NF.EQ.1) MDK=0                                                 BNOM0091
      IF(NF.EQ.1) GOTO 203                                              BNOM0092
      MMP=32000                                                         BNOM0093
      DO 216 I=2,NT                                                     BNOM0094
      I1=TQ(2,I)-TQ(2,I)/100*100                                        BNOM0095
      IF(I1.LE.82) GOTO 216                                             BNOM0096
      TQ4=TQ(4,I)                                                       BNOM0097
      IF(TQC(2,I).EQ.0.AND.TQC(1,I).LT.0) TQ4=TQC(1,I)                  BNOM0098
      IF(TQ4.LE.MMP) MMP=TQ4                                            BNOM0099
  216 CONTINUE                                                          BNOM0100
      IF(MMP.EQ.32000) MDK=777777                                       BNOM0101
      IF(MMP.EQ.32000) GOTO 217                                         BNOM0102
      IF(MMP.LT.-LP) MDK=999999                                         BNOM0103
      IF(MMP.LT.-LP) GOTO 217                                           BNOM0104
      IF(MMP.GT.LP) MDK=0                                               BNOM0105
      IF(MMP.GT.LP) GOTO 217                                            BNOM0106
      MMP=MMP+LP                                                        BNOM0107
      MDK=KDPG(MMP)/10                                                  BNOM0108
      IDATA=DAY                                                         BNOM0109
      GOD=IDATA                                                         BNOM0110
      IF(MDK-MDK/100*100.LE.MES.AND.MDK-LP.GT.LP1/2) IDATA=IDATA+1      BNOM0111
      IF(MDK-MDK/100*100.GE.MES.AND.MDK-LP.LE.0) IDATA=IDATA-1          BNOM0112
      MDK=MDK*100+(IDATA-IDATA/100*100)                                 BNOM0113
  217 CONTINUE                                                          BNOM0114
      LF=LF+LP                                                          BNOM0115
      IF(LF.GT.LP+LP) DATAV=0                                           BNOM0116
      IF(LF.GT.LP+LP) GOTO 203                                          BNOM0117
      IDATA=DAY                                                         BNOM0118
      LF=KDPG(LF)/10                                                    BNOM0119
      IF(LF-LF/100*100.LT.MES) IDATA=IDATA+1                            BNOM0120
      IF(LF-LF/100*100.EQ.MES.AND.LFF.GT.25) IDATA=DATA+1               BNOM0121
      DATAV=LF*100+(IDATA-IDATA/100*100)                                NOMT0135
  203 NXZ(5,1)=PROBEL                                                   BNOM0123
      NXZ(6,1)=PROBEL                                                   BNOM0124
      IF(NF.EQ.1) NXZ(5,1)=PR4                                          BNOM0125
      IF(NF.EQ.1) GOTO 204                                              BNOM0126
      IF(LFF.LE.NKP) NXZ(5,1)=PR1                                       BNOM0127
      IF(LFF.LE.NKP) GOTO 204                                           BNOM0128
      IF(TQ(2,NT).EQ.501) GOTO 205                                      BNOM0129
      IF(TQ(2,2).NE.512.AND.TQ(2,2).NE.513) NXZ(5,1)=PR2                BNOM0130
      IF(TQ(2,2).NE.512.AND.TQ(2,2).NE.513) GOTO 204                    BNOM0131
      IF(TQC(2,2).LE.0.AND.TQC(1,2).LT.0) NXZ(5,1)=PR2                  BNOM0132
      IF(TQC(2,2).LE.0.AND.TQC(1,2).LT.0) GOTO 204                      BNOM0133
      IF(TQC(2,2).GT.0) NXZ(5,1)=PR3                                    BNOM0134
      IF(TQC(2,2).GT.0) GOTO 204                                        BNOM0135
      KSCP=0                                                            BNOM0136
      DO 206 J=3,NT                                                     BNOM0137
      IF(TQC(2,J).EQ.0.AND.TQC(1,J).LE.0) GOTO 206                      BNOM0138
      IF(TQ(4,J).LE.LP1/2) KSCP=1                                       BNOM0139
  206 CONTINUE                                                          BNOM0140
      IF(KSCP.EQ.0) NXZ(5,1)=PR3                                        BNOM0141
      IF(KSCP.EQ.1) NXZ(5,1)=PR2                                        BNOM0142
      GOTO 204                                                          BNOM0143
  205 IF(TQC(2,NT).LE.0.AND.TQC(1,NT).LT.0) NXZ(5,1)=PR2                BNOM0146
      IF(TQC(2,NT).LE.0.AND.TQC(1,NT).LT.0) GOTO 204                    BNOM0147
      IF(TQC(2,NT).GT.0) NXZ(5,1)=PR3                                   BNOM0148
      IF(TQC(2,NT).GT.0) GOTO 204                                       BNOM0149
      KSCP=0                                                            BNOM0150
      KTQP=NT-1                                                         BNOM0151
      N11=A(10,NA)                                                      BNOM0152
      IF(N11.EQ.0) IZIK=2                                               BNOM0153
      IF(N11.NE.0) IZIK=A(12,S(1,N11))+A(13,S(1,N11))                   BNOM0154
      DO 207 J=IZIK,KTQP                                                BNOM0155
      IF(TQC(2,J).EQ.0.AND.TQC(1,J).LE.0) GOTO 207                      BNOM0156
      IF(TQ(4,J).LE.LP1/2) KSCP=1                                       BNOM0157
  207 CONTINUE                                                          BNOM0158
      IF(KSCP.EQ.0) NXZ(5,1)=PR3                                        BNOM0159
      IF(KSCP.EQ.1) NXZ(5,1)=PR2                                        BNOM0160
  204 DO 2 J=1,10                                                       BNOM0161
      IF(NXZ(4,1).EQ.F(J)) GOTO 8                                       BNOM0162
    2 CONTINUE                                                          BNOM0163
      WRITE(3,33,rec=NK) NZ,(NXZ(I,1),I=1,6),(NXZ(I,4),I=1,5),(NXZ(I,3),BNOM0164
     1i=1,10),ID,IQ,IP,MP,LZ,KT,(BOT(I),I=1,6),NKT,NM,LD,MDK,DATAV,(NXZ(BNOM0165
     2I,2),I=1,10),NXZ(8,4),NXZ(9,4),IQM,NTF
   33 format(i2,21a2,17i6,10A2,2A2,2I6)
      NK=NK+1                                                           BNOM0166
      GOTO 60                                                           BNOM0167
    8 LN(2)=LN(3)                                                       BNOM0168
      LN(3)=LN(4)                                                       BNOM0169
      LN(4)=D                                                           BNOM0170
      WRITE(3,34,rec=NK) NZ,D,(NXZ(I,1),I=1,5),D,(NXZ(I,4),I=1,5),(NXZ(IBNOM0171
     1,3),i=1,10),ID,IQ,IP,MP,LZ,KT,(BOT(I),I=1,6),NKT,NM,LD,MDK,DATAV, BNOM0172
     2(NXZ(I,2),I=1,10),NXZ(8,4),NXZ(9,4),IQM,NTF
   34 format(i2,a1,5a2,a1,15a2,17i6,10A2,2A2,2I6)
      NK=NK+1                                                           BNOM0173
      GOTO 60                                                           BNOM0174
   20 NF=2                                                              BNOM0175
      REWIND 1                                                          BNOM0176
      GOTO 60                                                           BNOM0177
   61 REWIND 2                                                          BNOM0178
      LZ=0                                                              BNOM0179
      DO 42 I=1,6                                                       BNOM0180
   42 BOT(I)=0                                                          BNOM0181
      CTM=0                                                             BNOM0182
   40 FORMAT(1X,4A2,A2,10X,10A2,I1,I5,10A2,5A2,A2,4X,I4,I2,I5,I8,22X,I1)BNOM0183
      open(8,file='F:\asyip\nakzai.dat')
   43 READ (8,40,END=41) ZAK,GZZ,KLAS,IP,IQ,HERT,NAIM,MAS,ZEZ,NZ,ID,MP,
     1PRIZ
      IF(IP.NE.0.AND.IP.NE.1.AND.IP.NE.2) GOTO 43                       BNOM0185
      IF(GZZ.EQ.GZ)  GOTO 43                                            BNOM0186
      MP=MP/10000*100+((MP-MP/10000*10000)-(MP-MP/10000*10000)/100*100)
      PR=PR5                                                            BNOM0187
      PRR=PROBEL                                                        BNOM0188
      IF(PRIZ.EQ.1) PRR=PR6                                             BNOM0189
      IF(PRIZ.EQ.0.AND.IP.EQ.9) PR=PR8                                  BNOM0190
      IF(PRIZ.EQ.0.AND.IP.EQ.9) PRR=PR7                                 BNOM0191
      IF(PRIZ.EQ.0.AND.IP.NE.9) PRR=PR7                                 BNOM0192
  333 FORMAT(I2,21A2,13I6,2A2,A2,3I6,10A2,I4)   
      WRITE(3,333,rec=NK) NZ,ZAK,PR,PRR,NAIM,KLAS,ID,IQ,IP,MP,LZ,LZ,    BNOM0194
     1(BOT(I),I=1,6),LZ,PROBEL,PROBEL,MAS,LZ,LZ,LZ,HERT,ZEZ             BNOM0195
      NK=NK+1                                                           BNOM0196
      GOTO 43                                                           BNOM0197
   41 close(3)
      END                                                               BNOM0199
 
