      include  'fgraph.fi'
      include  'fgraph.fd'
      INTEGER*2 EDF/-1/,U(8)                                            ODKA0001
      INTEGER*2 NN(27)                                                  ODKA0002
      INTEGER*2 IZ/' 1'/                                                ODKA0003
      INTEGER IPM(25)                                                   ODKA0004
      record / rccoord / curpos
      integer*2 dummy2
      integer*4 dummy4,dummy
   1  FORMAT(////35X,'CBOÄKA O BÛÏOËHEHÈÈ ÏËAHA ',I2,' ÖEXA ÇA ',I6)    ODKA0005
    2 FORMAT(////35X,'CBOÄKA O BÛÏOËHEHÈÈ ÏËAHA ÈHCTPÓMEHTAËÜHÛM ÏPOÈÇBOODKA0006
     1ÄCTBOM HA ',I6)                                                   ODKA0007
   45 FORMAT(67X,'HA×AËÜHÈK OTÄEËA ACÓÏ',13X,'/  ÈÃHATOB B.B./')        ODKA0008
    3 FORMAT(1X,118(1H-)/1X,'HAÈMEHOBAHÈE    !MECß×-! ÏËAH !MECß×-!ÔAKTÈODKA0009
     1×ECKOE !     BAËOBAß ÏPOÄÓKÖÈß C HA×AËA MECßÖA      !HOMEHKËATÓPA!ODKA0010
     2CÄAHO ÇA!'/17X,'!HÛÉ   !      !HÛÉ   !BÛÏOËHEHÈE  !',57(1H-),'!   ODKA0011
     3    !'/1X,'ÏOÄPAÇÄEËEHÈß   !PECÓPC! ÏÝO  !ÏËAH  !ÏËAHA       !    ODKA0012
     4ÏËAH    !ÏËAHOB ÔAKT !BHEÏËAH.ÔAKT!ÏPOÖ !ÔAKTÈ×.BÛÏOË! CÓTKÈ  !'/1ODKA0013
     5X,89(1H-),'!     !',22(1H-)/1X,'                !H/×ACÛ!H/×ACÛ!H/×ODKA0014
     6ACÛ!H/×ACÛ!  %  !H/×ACÛ!  %  !H/×ACÛ!  %  !H/×ACÛ!  %  !BÛÏOË!H/×AODKA0015
     7CÛ!  %  !H/×ACÛ  !'/1X,118(1H-))                                  ODKA0016
    4 FORMAT(1X,8A2,2X,I5,2X,I5,2X,I5,2X,I5,1X,I5,2X,I5,1X,I5,2X,I5,1X,IODKA0017
     15,2X,I5,1X,I5,1X,I5,2X,I5,1X,I5,2X,I5,3X)                         ODKA0018
    5 FORMAT(1H+,17X,I6,2X,I5,2X,I5,2X,I5,1X,I5,2X,I5,1X,I5             ODKA0019
     1,2X,I5,1X,I5,2X,I5,1X,I5,1X,I5,2X,I5,1X,I5,2X,I5,3X)              ODKA0020
    6 FORMAT(' ÈTOÃO ÏO Ó×-ÊÓ')                                         ODKA0021
    7 FORMAT(' ÈTOÃO ÏO ÖEXÓ ')                                         ODKA0022
    8 FORMAT(' ÈHCTPÓMEHT.ÖEX')                                         ODKA0023
    9 FORMAT(' ÈHCTP-ØTAM.ÖEX')                                         ODKA0024
   40 FORMAT(1H )                                                       ODKA0025
   10 FORMAT(' ÈTOÃO')                                                  ODKA0026
      open(4,file='f:\asyip\cbodbal.dat')
   11 FORMAT(8A2,8I5)                                                   ODKA0028
   12 FORMAT(10I8)                                                      ODKA0029
   30 FORMAT(A2)                                                        ODKA0030
   41 FORMAT(1X,118(1H-))                                               ODKA0031
      dummy=setvideomode( $ERESCOLOR )
      dummy=remappalette( 0,$BLUE )
      call clearscreen( $GCLEARSCREEN )
      open(1,file='f:\asyip\planbal.dat')
      open(6,file='prn')
      READ (1,12) KOL                                                   ODKA0032
      READ (1,12) KOL1                                                  ODKA0033
      READ (1,12) (IPM(J),J=1,6)                                        ODKA0034
      READ (1,12) (IPM(J),J=7,15)                                       ODKA0035
      READ (1,12) IP10                                                  ODKA0036
      READ (1,12) IP15                                                  ODKA0037
      KZP=0                                                             ODKA0038
      dummy2=settextcolor(2)
      call settextposition( 10,25, curpos ) 
      call outtext( 'çàäàé äàòó(÷÷ììãã)' )
      read(*,777) id
  777 format(i6)
      DO 22 I=1,KOL                                                     ODKA0042
      MPR1=0                                                            ODKA0043
      IZ8=0                                                             ODKA0044
      IZ7=0                                                             ODKA0045
      IZ6=0                                                             ODKA0046
      IZ5=0                                                             ODKA0047
      IZ4=0                                                             ODKA0048
      IZ3=0                                                             ODKA0049
      IZ2=0                                                             ODKA0050
      IZ1=0                                                             ODKA0051
      IM8=0                                                             ODKA0052
      IM7=0                                                             ODKA0053
      IM6=0                                                             ODKA0054
      IM5=0                                                             ODKA0055
      IM4=0                                                             ODKA0056
      IM3=0                                                             ODKA0057
      IM2=0                                                             ODKA0058
      IM1=0                                                             ODKA0059
      IS1=0                                                             ODKA0060
      IS2=0                                                             ODKA0061
      IS3=0                                                             ODKA0062
      IS4=0                                                             ODKA0063
      IS5=0                                                             ODKA0064
      IS6=0                                                             ODKA0065
      IS7=0                                                             ODKA0066
      IS8=0                                                             ODKA0067
      JK=0                                                              ODKA0068
      NZ=10                                                             ODKA0069
      KPZ=IP10                                                          ODKA0070
      NI=1                                                              ODKA0071
      L=72                                                              ODKA0072
      WRITE (6,1) NZ,ID                                                 ODKA0073
      WRITE (6,3)                                                       ODKA0074
      ASSIGN 14 TO M                                                    ODKA0075
      L=L-12                                                            ODKA0076
   13 READ(4,11,end=18) (U(J),J=1,8),K1,K2,K3,K4,K5,K6,K7,K8            ODKA0079
      NI=NI+1                                                           ODKA0080
      GOTO M,(14,15,23)                                                 ODKA0081
   15 IF(U(8).EQ.IZ.AND.MPR1.EQ.0) GOTO 16                              ODKA0082
   23 ASSIGN 14 TO M1                                                   ODKA0083
      ASSIGN 21 TO M2                                                   ODKA0084
      IF(KU.NE.U(8)) GOTO17                                             ODKA0085
   14 IP1=K3*100/K1                                                     ODKA0086
      IF(K2.EQ.0) K2=1                                                  ODKA0087
      IF(K4.EQ.0) K4=1                                                  ODKA0088
      IP2=K4*100/K2                                                     ODKA0089
      IP3=K5*100/K4                                                     ODKA0090
      IP4=K6*100/K4                                                     ODKA0091
      IP5=IP3+IP4                                                       ODKA0092
      IP6=K7*100/K4                                                     ODKA0093
      IM1=IM1+K1                                                        ODKA0094
      IM2=IM2+K2                                                        ODKA0095
      IM3=IM3+K3                                                        ODKA0096
      IM4=IM4+K4                                                        ODKA0097
      IM5=IM5+K5                                                        ODKA0098
      IM6=IM6+K6                                                        ODKA0099
      IM7=IM7+K7                                                        ODKA0100
      IM8=IM8+K8                                                        ODKA0101
      IZ1=IZ1+K1                                                        ODKA0102
      IZ2=IZ2+K2                                                        ODKA0103
      IZ3=IZ3+K3                                                        ODKA0104
      IZ4=IZ4+K4                                                        ODKA0105
      IZ5=IZ5+K5                                                        ODKA0106
      IZ6=IZ6+K6                                                        ODKA0107
      IZ7=IZ7+K7                                                        ODKA0108
      IZ8=IZ8+K8                                                        ODKA0109
      IS1=IS1+K1                                                        ODKA0110
      IS2=IS2+K2                                                        ODKA0111
      IS3=IS3+K3                                                        ODKA0112
      IS4=IS4+K4                                                        ODKA0113
      IS5=IS5+K5                                                        ODKA0114
      IS6=IS6+K6                                                        ODKA0115
      IS7=IS7+K7                                                        ODKA0116
      IS8=IS8+K8                                                        ODKA0117
      WRITE(6,4) (U(J),J=1,8),K1,KZP,K2,K3,IP1,K4,IP2,K5,IP3,K6,IP4,IP5,ODKA0118
     1K7,IP6,K8                                                         ODKA0119
      L=L-1                                                             ODKA0120
      IF(L.LT.0) L=72                                                   ODKA0121
      ASSIGN 15 TO M                                                    ODKA0122
      KU=U(8)                                                           ODKA0123
      GOTO 13                                                           ODKA0124
   17 JK=JK+1                                                           ODKA0125
      IP1=IM3*100/IPM(JK)                                               ODKA0126
      IP2=IM4*100/IM2                                                   ODKA0127
      IP3=IM5*100/IM4                                                   ODKA0128
      IP4=IM6*100/IM4                                                   ODKA0129
      IP5=IP3+IP4                                                       ODKA0130
      IP6=IM7*100/IM4                                                   ODKA0131
      WRITE(6,6)                                                        ODKA0132
      WRITE(6,5) IM1,IPM(JK),IM2,IM3,IP1,IM4,IP2,IM5,IP3,IM6,IP4,IP5,IM7ODKA0133
     1,IP6,IM8                                                          ODKA0134
      WRITE(6,40)                                                       ODKA0135
      IM1=0                                                             ODKA0136
      IM2=0                                                             ODKA0137
      IM3=0                                                             ODKA0138
      IM4=0                                                             ODKA0139
      IM5=0                                                             ODKA0140
      IM6=0                                                             ODKA0141
      IM7=0                                                             ODKA0142
      IM8=0                                                             ODKA0143
      L=L-2                                                             ODKA0144
      GOTO M1,(14,19)                                                   ODKA0145
   16 ASSIGN 19 TO M1                                                   ODKA0146
      MPR1=1                                                            ODKA0147
      GOTO 17                                                           ODKA0148
   19 IP1=IZ3*100/KPZ                                                   ODKA0149
      IP2=IZ4*100/IZ2                                                   ODKA0150
      IP3=IZ5*100/IZ4                                                   ODKA0151
      IP4=IZ6*100/IZ4                                                   ODKA0152
      IP5=IP3+IP4                                                       ODKA0153
      IP6=IZ7*100/IZ4                                                   ODKA0154
      WRITE(6,7)                                                        ODKA0155
      WRITE(6,5) IZ1,KPZ,IZ2,IZ3,IP1,IZ4,IP2,IZ5,IP3,IZ6,IP4,IP5,IZ7,IP6ODKA0156
     1,IZ8                                                              ODKA0157
      WRITE(6,40)                                                       ODKA0158
      WRITE(6,45)                                                       ODKA0159
      L=L-1                                                             ODKA0160
      L=L-2                                                             ODKA0161
      DO 20 J=1,L                                                       ODKA0162
      WRITE(6,40)                                                       ODKA0163
   20 CONTINUE                                                          ODKA0164
      GOTO M2,(21,22)                                                   ODKA0165
   18 ASSIGN 19 TO M1                                                   ODKA0166
      ASSIGN 22 TO M2                                                   ODKA0167
      GOTO 17                                                           ODKA0168
   21 NZ=15                                                             ODKA0169
      IZ11=IZ1                                                          ODKA0170
      IZ21=IZ2                                                          ODKA0171
      IZ31=IZ3                                                          ODKA0172
      IZ41=IZ4                                                          ODKA0173
      IZ51=IZ5                                                          ODKA0174
      IZ61=IZ6                                                          ODKA0175
      IZ71=IZ7                                                          ODKA0176
      IZ81=IZ8                                                          ODKA0177
      KPZ=IP15                                                          ODKA0178
      L=72                                                              ODKA0179
      WRITE(6,1) NZ,ID                                                  ODKA0180
      WRITE(6,3)                                                        ODKA0181
      L=L-12                                                            ODKA0182
      IZ1=0                                                             ODKA0183
      IZ2=0                                                             ODKA0184
      IZ3=0                                                             ODKA0185
      IZ4=0                                                             ODKA0186
      IZ5=0                                                             ODKA0187
      IZ6=0                                                             ODKA0188
      IZ7=0                                                             ODKA0189
       IZ8=0                                                            ODKA0190
      ASSIGN 23 TO M                                                    ODKA0191
      GOTO 14                                                           ODKA0192
   22 CONTINUE                                                          ODKA0193
      IP11=IZ31*100/IP10                                                ODKA0194
      IP21=IZ41*100/IZ21                                                ODKA0195
      IP31=IZ51*100/IZ41                                                ODKA0196
      IP41=IZ61*100/IZ41                                                ODKA0197
      IP51=IP31+IP41                                                    ODKA0198
      IP1=IZ3*100/IP15                                                  ODKA0199
      IP61=IZ71*100/IZ41                                                ODKA0200
      IPP=IP10+IP15                                                     ODKA0201
      IPS1=IS3*100/IPP                                                  ODKA0202
      IPS2=IS4*100/IS2                                                  ODKA0203
      IPS3=IS5*100/IS4                                                  ODKA0204
      IPS4=IS6*100/IS4                                                  ODKA0205
      IPS5=IPS3+IPS4                                                    ODKA0206
      IPS6=IS7*100/IS4                                                  ODKA0207
      DO 50 I1=1,KOL1                                                   ODKA0208
      WRITE(6,41)                                                       ODKA0209
      L=35                                                              ODKA0210
      WRITE(6,2) ID                                                     ODKA0211
      WRITE(6,3)                                                        ODKA0212
      WRITE(6,8)                                                        ODKA0213
      WRITE(6,5) IZ11,IP10,IZ21,IZ31,IP11,IZ41,IP21,IZ51,IP31,IZ61,IP41,ODKA0214
     1IP51,IZ71,IP61,IZ81                                               ODKA0215
      WRITE(6,9)                                                        ODKA0216
      WRITE(6,5) IZ1,IP15,IZ2,IZ3,IP1,IZ4,IP2,IZ5,IP3,IZ6,IP4,IP5,IZ7,IPODKA0217
     16,IZ8                                                             ODKA0218
      L=L-14                                                            ODKA0219
      WRITE(6,10)                                                       ODKA0220
      WRITE(6,5) IS1,IPP,IS2,IS3,IPS1,IS4,IPS2,IS5,IPS3,IS6,IPS4,IPS5,ISODKA0221
     17,IPS6,IS8                                                        ODKA0222
      L=L-1                                                             ODKA0223
      WRITE(6,40)                                                       ODKA0224
      WRITE(6,45)                                                       ODKA0225
      L=L-2                                                             ODKA0226
      DO 24 J=1,L                                                       ODKA0227
      WRITE(6,40)                                                       ODKA0228
   24 CONTINUE                                                          ODKA0229
   50 CONTINUE                                                          ODKA0230
      WRITE(6,41)                                                       ODKA0231
      dummy=setvideomode($defaultmode)      
      stop
      END                                                               ODKA0232
 
