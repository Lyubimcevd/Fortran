      SUBROUTINE PRIOR7(A,TQ,QZ,NT,NA,S,U,LZ,IP,NU1,TQC)                ZAK40006
      INTEGER*2 TQC(4,500)                                              ZAK40007
      INTEGER*2 A(26,200),TQ(6,500),PRIZN(500)                          ZAK40008
      INTEGER*2 U(20,100)                                               ZAK40009
      INTEGER*2 UR(200),NU(10),KU(10)                                   ZAK40010
      INTEGER*2 S(2,200),SB                                             ZAK40011
      INTEGER QZ(4,500),TR                                              ZAK40012
      INTEGER*2 SLES/'83'/                                              ZAK40013
      DO 20 I=1,NA                                                      ZAK40014
      IF(A(9,I).EQ.1) NMK1=A(12,I)                                      ZAK40015
      IF(A(9,I).EQ.1) KMK1=A(13,I)                                      ZAK40016
      NMK=A(12,I)                                                       ZAK40017
      KMK=NMK+A(13,I)-1                                                 ZAK40018
      DO 21 J=NMK,KMK                                                   ZAK40019
      IF(TQC(2,J).EQ.0) GOTO 24                                         ZAK40020
      IF(J.EQ.NMK) GOTO 20                                              ZAK40021
      KMK=J-1                                                           ZAK40022
      DO 23 J1=NMK,KMK                                                  ZAK40023
      TQ(4,J1)=TQ(4,J)                                                  ZAK40024
      TQ(5,J1)=TQ(4,J)                                                  ZAK40025
      TQ(6,J1)=TQ(4,J)                                                  ZAK40026
   23 CONTINUE                                                          ZAK40027
      GOTO 20                                                           ZAK40028
   24 TQ(4,J)=TQ(6,KMK)                                                 ZAK40029
      TQ(5,J)=TQ(6,KMK)                                                 ZAK40030
      TQ(6,J)=TQ(6,KMK)                                                 ZAK40031
   21 CONTINUE                                                          ZAK40032
   20 CONTINUE                                                          ZAK40033
      INOP=820                                                          ZAK40034
      IF(IP.EQ.1) INOP=100                                              ZAK40035
      IF(IP.EQ.2) INOP=200                                              ZAK40036
      IF(IP.EQ.0) INOP=0                                                ZAK40037
      DO 1 I=2,NT                                                       ZAK40038
      IF(TQ(2,I).EQ.512.OR.TQ(2,I).EQ.513) GOTO 1                       ZAK40039
      IF(TQC(2,I).EQ.0) GOTO 1                                          ZAK40040
   91 R=(QZ(1,I)+0.0)/U(2,TQ(1,I))                                      ZAK40041
      J4=INT(R)                                                         ZAK40042
      IF(R-J4.LE.0.0) J4=J4-1                                           ZAK40043
      TQ(6,I)=TQ(5,I)-U(6,TQ(1,I))                                      ZAK40044
      TQ(4,I)=TQ(5,I)+J4                                                ZAK40045
    1 PRIZN(I)=0                                                        ZAK40046
      I=NT                                                              ZAK40047
   81 MMK=TQ(3,I)                                                       ZAK40048
   82 I=I-1                                                             ZAK40049
      IF(I.LE.1) GOTO 88                                                ZAK40050
      IF(TQC(2,I).EQ.0) GOTO 82                                         ZAK40051
      IF(MMK.NE.TQ(3,I)) GOTO 81                                        ZAK40052
      ID1=TQ(5,I)-TQ(6,I)                                               ZAK40053
      ID2=TQ(4,I)-TQ(5,I)                                               ZAK40054
      TQ(6,I)=TQ(4,I+1)+1                                               ZAK40055
      TQ(5,I)=TQ(6,I)+ID1                                               ZAK40056
      TQ(4,I)=TQ(5,I)+ID2                                               ZAK40057
      GOTO 82                                                           ZAK40058
   88 DO 2 I=2,NA                                                       ZAK40059
      NACH=A(12,I)                                                      ZAK40060
      IPR=PRIZN(NACH)                                                   ZAK40061
      IF(IPR.GT.0) GOTO 2                                               ZAK40062
      KON=NACH+A(13,I)-1                                                ZAK40063
      J=KON                                                             ZAK40064
      TR=0                                                              ZAK40065
      KRAN=0                                                            ZAK40066
    5 IF(TQ(5,J).EQ.TQ(6,J)) GOTO 31                                    ZAK40067
      IF(KRAN.EQ.0) GOTO 6                                              ZAK40068
      K=J+1                                                             ZAK40069
      GOTO 7                                                            ZAK40070
   31 IF(TQ(4,J).EQ.TQ(5,J)) GOTO 32                                    ZAK40071
      IF(KRAN.NE.0) K=J+1                                               ZAK40072
      IF(KRAN.NE.0) GOTO 7                                              ZAK40073
      N=J                                                               ZAK40074
      KRAN=1                                                            ZAK40075
      ID2=TQ(4,J)-TQ(5,J)                                               ZAK40076
      TR=QZ(1,J)-U(2,TQ(1,J))*ID2                                       ZAK40077
      TR=TR+INOP                                                        ZAK40078
      IF(TR.LT.U(2,TQ(1,J))) GOTO 6                                     ZAK40079
      KRAN=0                                                            ZAK40080
      TR=0                                                              ZAK40081
      GOTO 6                                                            ZAK40082
   32 IF(KRAN.EQ.0) N=J                                                 ZAK40083
      KRAN=1                                                            ZAK40084
      IF(TQC(2,J).EQ.0) GOTO 6                                          ZAK40085
      TR=TR+QZ(1,J)+INOP                                                ZAK40086
      I1=TQ(2,J)-TQ(2,J)/100*100                                        ZAK40087
      IF(I1.EQ.SLES.AND.QZ(1,J).EQ.1) TR=TR-INOP                        ZAK40088
      IF(TR.GE.U(2,TQ(1,J))) GOTO 4                                     ZAK40089
    6 J=J-1                                                             ZAK40090
      IF(J.GE.NACH) GOTO 5                                              ZAK40091
      IF(KRAN.EQ.0) PRIZN(NACH)=1                                       ZAK40092
      IF(KRAN.EQ.0) GOTO 2                                              ZAK40093
      K=NACH                                                            ZAK40094
      GOTO 7                                                            ZAK40095
    4 IF(N.EQ.J) K=J                                                    ZAK40096
      IF(N.EQ.J) J=J-1                                                  ZAK40097
      IF(N.EQ.J) GOTO 7                                                 ZAK40098
      K=J+1                                                             ZAK40099
    7 IF(K.EQ.N) GOTO 33                                                ZAK40100
      M=K                                                               ZAK40101
    8 TQ(4,M)=TQ(4,N)                                                   ZAK40102
      TQ(5,M)=TQ(4,N)                                                   ZAK40103
      TQ(6,M)=TQ(4,N)                                                   ZAK40104
      M=M+1                                                             ZAK40105
      IF(M.LT.N) GOTO 8                                                 ZAK40106
      IF(K.EQ.NACH) PRIZN(NACH)=1                                       ZAK40107
      IF(K.EQ.NACH) GOTO 2                                              ZAK40108
      M=K-1                                                             ZAK40109
    9 ID1=TQ(5,M)-TQ(6,M)                                               ZAK40110
      ID2=TQ(4,M)-TQ(5,M)                                               ZAK40111
      TQ(6,M)=TQ(4,M+1)+1                                               ZAK40112
      TQ(5,M)=TQ(6,M)+ID1                                               ZAK40113
      TQ(4,M)=TQ(5,M)+ID2                                               ZAK40114
      M=M-1                                                             ZAK40115
      IF(M.GE.NACH) GOTO 9                                              ZAK40116
   33 KRAN=0                                                            ZAK40117
      TR=0                                                              ZAK40118
      J=J+1                                                             ZAK40119
      GOTO 6                                                            ZAK40120
  333 ID1=TQ(5,J)-TQ(6,J)                                               ZAK40121
      ID2=TQ(4,J)-TQ(5,J)                                               ZAK40122
      TQ(6,J)=TQ(4,J+1)+1                                               ZAK40123
      TQ(5,J)=TQ(6,J)+ID1                                               ZAK40124
      TQ(4,J)=TQ(5,J)+ID2                                               ZAK40125
      GOTO 33                                                           ZAK40126
    2 CONTINUE                                                          ZAK40127
      DO 50 K=1,NA                                                      ZAK40128
   50 UR(K)=0                                                           ZAK40129
      N=1                                                               ZAK40130
      I=1                                                               ZAK40131
   51 NU(I)=A(10,N)                                                     ZAK40132
      KU(I)=NU(I)+A(11,N)-1                                             ZAK40133
      M1=NU(I)                                                          ZAK40134
      M2=KU(I)                                                          ZAK40135
      DO 52 J=M1,M2                                                     ZAK40136
      IF(I.GT.UR(S(1,J))) UR(S(1,J))=I                                  ZAK40137
   52 CONTINUE                                                          ZAK40138
   53 N=S(1,NU(I))                                                      ZAK40139
      I=I+1                                                             ZAK40140
      IF(A(11,N).GT.0) GOTO 51                                          ZAK40141
   54 I=I-1                                                             ZAK40142
      IF(I.LE.0) GOTO 60                                                ZAK40143
      NU(I)=NU(I)+1                                                     ZAK40144
      IF(NU(I).GT.KU(I)) GOTO 54                                        ZAK40145
      GOTO 53                                                           ZAK40146
   60 DO 61 I=1,NT                                                      ZAK40147
   61 PRIZN(I)=0                                                        ZAK40148
      MUR=0                                                             ZAK40149
      DO 62 K=1,NA                                                      ZAK40150
      IF(UR(K).GT.MUR) MUR=UR(K)                                        ZAK40151
   62 CONTINUE                                                          ZAK40152
      MUR=MUR-1                                                         ZAK40153
      DO 68 MU=1,MUR                                                    ZAK40154
      DO 63 I=1,NA                                                      ZAK40155
      IF(UR(I).NE.MU) GOTO 63                                           ZAK40156
      IF(A(11,I).LE.0) GOTO 63                                          ZAK40157
      MX=TQ(4,A(12,I))                                                  ZAK40158
      ND=A(10,I)                                                        ZAK40159
      KD=ND+A(11,I)-1                                                   ZAK40160
      MK=A(9,I)                                                         ZAK40161
   64 IF(MK.EQ.A(9,S(1,ND))) GOTO 65                                    ZAK40162
      NMK=A(12,S(1,ND))                                                 ZAK40163
      KMK=NMK+A(13,S(1,ND))-1                                           ZAK40164
      IF(PRIZN(A(12,S(1,ND))).EQ.0) GOTO 66                             ZAK40165
      IF(TQ(6,KMK).GE.MX) GOTO 65                                       ZAK40166
   66 ID1=TQ(5,KMK)-TQ(6,KMK)                                           ZAK40167
      ID2=TQ(4,KMK)-TQ(5,KMK)                                           ZAK40168
      TQ(6,KMK)=MX                                                      ZAK40169
   67 TQ(5,KMK)=TQ(6,KMK)+ID1                                           ZAK40170
      ID3=TQ(4,KMK)                                                     ZAK40171
      TQ(4,KMK)=TQ(5,KMK)+ID2                                           ZAK40172
      IF(KMK.LE.NMK) PRIZN(NMK)=1                                       ZAK40173
      IF(KMK.LE.NMK) GOTO 65                                            ZAK40174
      KMK=KMK-1                                                         ZAK40175
      ID1=TQ(5,KMK)-TQ(6,KMK)                                           ZAK40176
      ID2=TQ(4,KMK)-TQ(5,KMK)                                           ZAK40177
      ID3=TQ(6,KMK)-ID3                                                 ZAK40178
      TQ(6,KMK)=TQ(4,KMK+1)+ID3                                         ZAK40179
      GOTO 67                                                           ZAK40180
   65 ND=ND+1                                                           ZAK40181
      IF(ND.LE.KD) GOTO 64                                              ZAK40182
   63 CONTINUE                                                          ZAK40183
   68 CONTINUE                                                          ZAK40184
   70 LZ=0                                                              ZAK40185
      NZIK=NMK1+KMK1                                                    ZAK40186
      DO 71 I=1,NT                                                      ZAK40187
      IF(TQC(2,I).EQ.0) GOTO 71                                         ZAK40188
      IF(TQ(4,I).GT.LZ) LZ=TQ(4,I)                                      ZAK40189
   71 CONTINUE                                                          ZAK40190
      N=A(10,1)                                                         ZAK40191
      N=S(1,N)                                                          ZAK40192
      N=A(12,N)+A(13,N)-1                                               ZAK40193
      N=TQ(1,N)                                                         ZAK40194
      NZ=U(1,N)                                                         ZAK40195
      IF(TQ(2,NT).NE.501) GOTO 80                                       ZAK40197
      IF(TQC(2,NT).EQ.0)  GOTO 80                                       ZAK40198
      IF(TQ(4,NT).GE.TQ(4,2)) GOTO 711                                  ZAK40199
      LZ=0                                                              ZAK40200
      DO 710 I=NZIK,NT                                                  ZAK40201
      IF(TQC(2,I).EQ.0) GOTO 710                                        ZAK40202
      IF(TQ(4,I).GT.LZ) LZ=TQ(4,I)                                      ZAK40203
  710 CONTINUE                                                          ZAK40204
  711 TQ(4,NT)=LZ+30+1                                                  ZAK40205
      TQ(5,NT)=LZ+30+1                                                  ZAK40206
      TQ(6,NT)=TQ(5,NT)-1                                               ZAK40207
      R=(QZ(1,NT)+0.0)/U(2,TQ(1,NT))                                    ZAK40208
      J4=INT(R)                                                         ZAK40209
      IF(R-J4.LE. 0.0) J4=J4-1                                          ZAK40210
      TQ(4,NT)=TQ(5,NT)+J4                                              ZAK40211
      LZ=LZ+30                                                          ZAK40212
      IF(TQ(4,NT).LT.TQ(4,2)) GOTO 810                                  ZAK40213
      GOTO 80                                                           ZAK40214
 810  NZIK1=NZIK-1                                                      ZAK40215
      NZIK2=NMK1+1                                                      ZAK40216
      DO 15 I=NZIK2,NZIK1                                               ZAK40217
      TQ(4,I)=TQ(4,I)+LZ+J4+2-TQ(6,NZIK1)                               ZAK40218
      TQ(5,I)=TQ(5,I)+LZ+J4+2-TQ(6,NZIK1)                               ZAK40219
  15  TQ(6,I)=TQ(6,I)+LZ+J4+2-TQ(6,NZIK1)                               ZAK40220
      TQ(4,NMK1)=TQ(4,NMK1+1)                                           ZAK40221
      TQ(5,NMK1)=TQ(4,NMK1+1)                                           ZAK40222
      TQ(6,NMK1)=TQ(4,NMK1+1)                                           ZAK40223
      LZ=TQ(4,2)                                                        ZAK40224
   80 RETURN                                                            ZAK40225
      END                                                               ZAK40226
