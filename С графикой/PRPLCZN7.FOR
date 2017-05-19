      INCLUDE  'FGRAPH.FI'
      INCLUDE  'FGRAPH.FD'
C     DEFINE FILE 2(17000,65,L,IJK)                                     LCZN0001
      INTEGER*2 W(2,20)/40*0/,MET/'СП'/,NZ15/'15'/,ZMV(18)/18*0/        LCZN0002
      INTEGER*2 A(26,200),S(2,200),P(2,200),TQ(6,500),TQC(4,500),U(20,10LCZN0003
     *0),NXZ(10,4),KDP(180),ZAK(4),NZ,NM,IVO,NMTK,NOP,I1,I2,NZMPO,NZMPR,LCZN0004
     *PRIZN,pri,nz99/'99'/                                                         LCZN0005
      INTEGER QZ(4,500),UW(2,200),IPI,TR,RAS                            LCZN0006
      character*1 zakc(8),prob/' '/,zakp(8)/8*' '/
      equivalence(zakc,zak)
      record / rccoord / curpos
      integer*2 dummy2
      integer*4 dummy4,dummy
      dummy=setvideomode( $ERESCOLOR )
      dummy=remappalette( 0,$BLUE )
      call clearscreen( $GCLEARSCREEN )
   14 FORMAT(4A2,5I2,2I5,4I4,I2,I1,I4,7A2)                              LCZN0007
  100 FORMAT(18I4)                                                      LCZN0008
      dummy2=settextcolor(2)
      call settextposition( 10,25, curpos ) 
      call outtext( 'задай цеха-участки(ццмм),если для всех -ввод' )
      read(*,100) (zmv(i),i=1,18)
      NZAP=0                                                            LCZN0010
      open(unit=4,file='f:\asyip\u.dat',form='unformatted')
      open(unit=1,file='f:\asyip\plan.dat',form='unformatted')
      open(2,file='f:\asyip\opercz.dat')
      READ(4) NW,NU                                                     LCZN0011
      READ(4) ((UW(I,J),I=1,2),J=1,NW),((U(I,J ),I=1,20),J=1,NU)        LCZN0012
      REWIND 4                                                          LCZN0013
      READ(1) LP,NN,NK                                                  LCZN0014
      LP1=LP*2                                                          LCZN0015
      READ(1) (KDP(J),J=1,LP1)                                          LCZN0016
      dummy2=settextcolor(2)
      call settextposition( 11,25, curpos ) 
      call outtext( 'задай начало планового периода(ччммгггг)' )
      read(*,777) nn
  777 format(i6)
      call settextposition( 12,25, curpos ) 
      call outtext( 'задай конец планового периода(ччммгггг)' )
      read(*,777) nk
      LP0=LP+1                                                          LCZN0019
      CALL KALDNI(NN,0,NXZ)                                             LCZN0020
      DO 1 NN=LP0,LP1                                                   LCZN0021
      IF(NXZ(1,1).EQ.KDP(NN)) GOTO 2                                    LCZN0022
    1 CONTINUE                                                          LCZN0023
      NN=LP0                                                            LCZN0024
    2 CALL KALDNI(NK,0,NXZ)                                             LCZN0025
      DO 3 NK=LP0,LP1                                                   LCZN0026
      IF(NXZ(1,1).EQ.KDP(NK)) GOTO 4                                    LCZN0027
    3 CONTINUE                                                          LCZN0028
      NK=LP1                                                            LCZN0029
    4 READ(1) NXZ,(I,J=1,NU),IQ,IP,ID,LQ,LZ,LF,MP,MS,NA,NS,NT           LCZN0030
      IF(NA.LT.0) GOTO 6                                                LCZN0031
      READ(1) ((A(I,J),I=1,26),J =1,NA)                                 LCZN0032
      READ(1) ((S(I,J),I=1,2),J=1,NS),((P(I,J ),I=1,2),J =1,NS)         LCZN0033
      READ(1) ((TQ(I,J),I=1,6),J=1,NT),((QZ(I,J),I=1,4),J=1,NT)         LCZN0034
      READ(1) ((TQC(I,J),I=1,4),J=1,NT)                                 LCZN0035
      IF(NXZ(5,1).EQ.MET) GOTO 4                                        LCZN0037
      if(nxz(10,4).eq.nz99) goto4
      IP1=IP                                                            LCZN0038
      IP=3-(IP+1)/2                                                     LCZN0039
      IF(IP.LE.0) IP=1                                                  LCZN0040
      DO 13 I=1,4                                                       LCZN0041
      ZAK(I)=NXZ(I,1)                                                   LCZN0042
   13 CONTINUE                                                          LCZN0043
      if(zakc(8).ne.prob) goto 333
      do 334 i=1,7
      zakp(i+1)=zakc(i)
  334 continue
      do 335 i=1,8
  335 zakc(i)=zakp(i) 
  333 DO 40 I=1,NU                                                      LCZN0044
      U(5,I)=9999                                                       LCZN0045
   40 CONTINUE                                                          LCZN0046
      DO 41 J=2,NT                                                      LCZN0047
      if(tq(2,j).eq.250.or.tq(2,j).eq.160.or.tq(2,j).eq.0) goto 41
      if(zmv(1).eq.0) goto 411
      DO 410 JJ=1,18                                                    LCZN0048
      IF(U(1,TQ(1,J))*100+TQ(2,J)/100.EQ.ZMV(JJ)) GOTO 411              LCZN0049
  410 CONTINUE                                                          LCZN0050
      GOTO 41                                                           LCZN0051
  411 I1=TQ(4,J)+LP                                                     LCZN0052
      I2=TQ(5,J)+LP                                                     LCZN0053
      IF((I2-NN)*(NK-I1).LT.0) GOTO 41                                  LCZN0054
      I=TQ(1,J)                                                         LCZN0055
      I1=KDP(I1)/10.                                                    LCZN0056
      IF(U(5,I).GT.I1) U(5,I)=I1                                        LCZN0057
   41 CONTINUE                                                          LCZN0058
      I3=0                                                              LCZN0059
      DO 42 I=1,NU                                                      LCZN0060
      IF(I.EQ.1) GOTO 43                                                LCZN0061
      IF(U(20,I-1).NE.U(20,I)) GOTO 43                                  LCZN0062
      IF(U(5,I).LT.W(2,I3)) W(2,I3)=U(5,I)                              LCZN0063
      GOTO 42                                                           LCZN0064
   43 I3=I3+1                                                           LCZN0065
      W(1,I3)=U(20,I)                                                   LCZN0066
      W(2,I3)=U(5,I)                                                    LCZN0067
   42 CONTINUE                                                          LCZN0068
      J1=1                                                              LCZN0069
      DO 44 I=1,I3                                                      LCZN0070
      DO 45 J=J1,NU                                                     LCZN0071
      IF(U(20,J).NE.W(1,I)) J1=J                                        LCZN0072
      IF(U(20,J).NE.W(1,I)) GOTO 44                                     LCZN0073
      U(5,J)=W(2,I)                                                     LCZN0074
   45 CONTINUE                                                          LCZN0075
   44 CONTINUE                                                          LCZN0076
      DO 7 J=2,NT                                                       LCZN0077
      if(tq(2,j).eq.250.or.tq(2,j).eq.160.or.tq(2,j).eq.0) goto 7
      J1=TQ(1,J)                                                        LCZN0078
      if(zmv(1).eq.0) goto 711
      DO 710 JJ=1,18                                                    LCZN0079
      IF(U(1,TQ(1,J))*100+TQ(2,J)/100.EQ.ZMV(JJ)) GOTO 711              LCZN0080
  710 CONTINUE                                                          LCZN0081
      GOTO 7                                                            LCZN0082
  711 IF(TQC(2,J).LE.0) GOTO 7                                          LCZN0083
      I1=TQ(4,J)+LP                                                     LCZN0084
      I2=TQ(5,J)+LP                                                     LCZN0085
      IF((I2-NN)*(NK-I1).LT.0) GOTO 7                                   LCZN0086
      I1=KDP(I1)/10.                                                    LCZN0087
      IF(I2.GT.LP1) I2=32000                                            LCZN0088
      IF(I2.LE.LP1) I2=KDP(I2)/10                                       LCZN0089
      I=TQ(1,J)                                                         LCZN0090
      NZ=U(1,I)                                                         LCZN0091
      NM=TQ(2,J )/100                                                   LCZN0092
      I=TQ(3,J)                                                         LCZN0093
      I=I-I/1000*1000                                                   LCZN0094
      I=A(1,I)                                                          LCZN0095
      NMTK=A(9,I)                                                       LCZN0096
      NOP=J-A(12,I)+1                                                   LCZN0097
      TR=QZ(1,J)                                                        LCZN0098
      RAS=QZ(2,J)                                                       LCZN0099
      I=TQ(3,J)                                                         LCZN0100
      I=I-I/1000*1000                                                   LCZN0101
      I=A(1,I)                                                          LCZN0102
      IF(J.EQ.A(12,I)) GOTO 8                                           LCZN0103
      IF((A(12,I)+A(13,I)-1).EQ.J) GOTO 9                               LCZN0104
      I3=TQ(1,J-1)                                                      LCZN0105
      I4=TQ(1,J+1)                                                      LCZN0106
      NZMPR=U(1,I3)*100+TQ(2,J-1)/100                                   LCZN0107
      NZMPO=U(1,I4)*100+TQ(2,J+1)/100                                   LCZN0108
      GOTO 10                                                           LCZN0109
    8 NZMPR=0                                                           LCZN0110
      IF((A(12,I)+A(13,I)-1).EQ.J) NZMPO=0                              LCZN0111
      IF((A(12,I)+A(13,I)-1).EQ.J) GOTO 10                              LCZN0112
      I4=TQ(1,J+1)                                                      LCZN0113
      NZMPO=U(1,I4)*100+TQ(2,J+1)/100                                   LCZN0114
      GOTO 10                                                           LCZN0115
    9 NZMPO=0                                                           LCZN0116
      I3=TQ(1,J-1)                                                      LCZN0117
      NZMPR=U(1,I3)*100+TQ(2,J-1)/100                                   LCZN0118
   10 PRI=QZ(3,J)*IP                                                    LCZN0119
      IF(LF.GT.LP) GOTO 11                                              LCZN0120
      PRIZN=2                                                           LCZN0121
      GOTO 12                                                           LCZN0122
   11 PRIZN=1                                                           LCZN0123
      IF(MS.EQ.-1) PRIZN=2                                              LCZN0124
   12 IF(TQ(2,J).EQ.501.AND.NMTK.EQ.1.AND.NOP.EQ.1) GOTO 7              LCZN0125
      IF(TQ(2,J).EQ.501.AND.NMTK.EQ.99) NMTK=1                          LCZN0126
      IF(TQ(4,J).EQ.TQ(5,J)) GOTO 70                                    LCZN0127
      IF(TQ(5,J).GT.NK-LP) TQ(5,J)=NK-LP+1                              LCZN0128
      TQ(5,J)=TQ(5,J)-1                                                 LCZN0129
      IF(IP1.EQ.0.AND.U(2,TQ(1,J)).LE.820) ITR=U(2,TQ(1,J))*2           LCZN0130
      IF(IP1.EQ.0.AND.U(2,TQ(1,J)).GT.820) ITR=U(2,TQ(1,J))             LCZN0131
      IF(IP1.NE.0) ITR=U(2,TQ(1,J))                                     LCZN0132
      IF(QZ(1,J).EQ.0) IRAS=0                                           LCZN0133
      IF(QZ(1,J).EQ.0) ITR=0                                            LCZN0134
      IF(QZ(1,J).EQ.0) GOTO 75                                          LCZN0135
      IRAS=ITR*QZ(2,J)/QZ(1,J)                                          LCZN0136
   75 M1=TQ(4,J)+LP                                                     LCZN0137
      M2=TQ(5,J)+LP                                                     LCZN0138
      IF(M2.LT.NN) GOTO 72                                              LCZN0139
      IF(M1.LT.NN) M1=NN                                                LCZN0140
      DO 71 M=M1,M2                                                     LCZN0141
      NZAP=NZAP+1                                                       LCZN0142
      M3=KDP(M)/10                                                      LCZN0143
      IVO=1                                                             LCZN0144
      WRITE(2,14) (ZAK(I),I=1,4),NZ,NM,IVO,NMTK,NOP,ITR,IRAS,NZMPR,     LCZN0145
     *NZMPO,M3,M3,PRI,PRIZN,U(5,J1),(U(I,J1),I=13,19)                   LCZN0146
  71  CONTINUE                                                          LCZN0147
   72 CONTINUE                                                          LCZN0148
       IF(TQ(5,J).EQ.NK-LP) GOTO 7                                      LCZN0149
      TQ(5,J)=TQ(5,J)+1                                                 LCZN0150
      TR=QZ(1,J)-ITR*(TQ(5,J)-TQ(4,J))                                  LCZN0151
      RAS=QZ(2,J)-IRAS*(TQ(5,J)-TQ(4,J))                                LCZN0152
      I1=KDP(TQ(5,J)+LP)/10                                             LCZN0153
      IF(TR.LT.0) GOTO 7                                                LCZN0154
   70 CONTINUE                                                          LCZN0155
      NZAP=NZAP+1                                                       LCZN0156
      IVO=0                                                             LCZN0157
      WRITE(2,14) (ZAK(I),I=1,4),NZ,NM,IVO,NMTK,NOP,TR,RAS,NZMPR,NZMPO  LCZN0158
     *,I1,I2,PRI,PRIZN,U(5,J1),(U(I,J1),I=13,19)                        LCZN0159
    7 CONTINUE                                                          LCZN0160
      GOTO 4                                                            LCZN0161
   6  dummy=setvideomode($defaultmode)
      STOP                                                              LCZN0162
      END                                                               LCZN0163
