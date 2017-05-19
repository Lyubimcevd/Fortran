      include  'fgraph.fi'
      include  'fgraph.fd'
      INTEGER*2 EDF/-1/,U(8)                                            ODKA0001
      INTEGER*2 NN(27)                                                  ODKA0002
      INTEGER*2 IZ/' 1'/,NULL/'00'/,NUH,PRUH                                                ODKA0003
      INTEGER IPM(25)                                                   ODKA0004
      record / rccoord / curpos
      integer*2 dummy2
      integer*4 dummy4,dummy
    2 FORMAT(35X,'CBOÑKA O BõèOãHEHàà èãAHA àHCTPìMEHTAãúHõM èPOàáBOÑëíÇODKA0006
     1OM HA ',I8)                                                       ODKA0007
   45 FORMAT(67X,'HAóAãúHàK OTÑEãA ACìè',13X,'/  àÉHATOB B.B./')        ODKA0008
    3 FORMAT(1X,118(1H-)/1X,'HAàMEHOBAHàE    !MECüó-! èãAH !MECüó-!îAKTàODKA0009
     1óECKOE !     BAãOBAü èPOÑìKñàü C HAóAãA MECüñA      !HOMEHKãATìPA!ODKA0010
     2CÑAHO áA!'/17X,'!Hõâ   !      !Hõâ   !BõèOãHEHàE  !',57(1H-),'!   ODKA0011
     3    !'/1X,'èOÑPAáÑEãEHàü   !PECìPC! èùO  !èãAH  !èãAHA       !    ODKA0012
     4èãAH    !èãAHOB îAKT !BHEèãAH.îAKT!èPOñ !îAKTàó.BõèOã! CìTKà  !'/1ODKA0013
     5X,89(1H-),'!     !',22(1H-)/1X,'                !H/óACõ!H/óACõ!H/óODKA0014
     6ACõ!H/óACõ!  %  !H/óACõ!  %  !H/óACõ!  %  !H/óACõ!  %  !BõèOã!H/óAODKA0015
     7Cõ!  %  !H/óACõ  !'/1X,118(1H-))                                  ODKA0016
    4 FORMAT(1X,8A2,2X,I5,2X,I5,2X,I5,2X,I5,1X,I5,2X,I5,1X,I5,2X,I5,1X,IODKA0017
     15,2X,I5,1X,I5,1X,I5,2X,I5,1X,I5,2X,I5,3X)                         ODKA0018
    5 FORMAT(1H+,17X,I6,2X,I5,2X,I5,2X,I5,1X,I5,2X,I5,1X,I5             ODKA0019
     1,2X,I5,1X,I5,2X,I5,1X,I5,1X,I5,2X,I5,1X,I5,2X,I5,3X)              ODKA0020
    6 FORMAT(' àTOÉO èO ìó-äì ',2X,I6,2X,I5,2X,I5,2X,I5,1X,I5,2X,I5            
     1,1X,I5,2X,I5,1X,I5,2X,I5,1X,I5,1X,I5,2X,I5,1X,I5,2X,I5,3X)                                         ODKA0021
    7 FORMAT(' àTOÉO èO ñEXì  ',2X,I6,2X,I5,2X,I5,2X,I5,1X,I5,2X,I5            
     1,1X,I5,2X,I5,1X,I5,2X,I5,1X,I5,1X,I5,2X,I5,1X,I5,2X,I5,3X)                                         ODKA0022
    8 FORMAT(' àHCTPìMEHT.ñEX',2X,I6,2X,I5,2X,I5,2X,I5,1X,I5,2X,I5            
     1,1X,I5,2X,I5,1X,I5,2X,I5,1X,I5,1X,I5,2X,I5,1X,I5,2X,I5,3X)                                         ODKA0023
    9 FORMAT(' àHCTP-òTAM.ñEX',2X,I6,2X,I5,2X,I5,2X,I5,1X,I5,2X,I5            
     1,1X,I5,2X,I5,1X,I5,2X,I5,1X,I5,1X,I5,2X,I5,1X,I5,2X,I5,3X)                                         ODKA0024
   40 FORMAT(1H )                                                       ODKA0025
   10 FORMAT(' àTOÉO',11X,I6,2X,I5,2X,I5,2X,I5,1X,I5,2X,I5,1X            
     1,I5,2X,I5,1X,I5,2X,I5,1X,I5,1X,I5,2X,I5,1X,I5,2X,I5,3X)                                                  ODKA0026
      open(4,file='f:\asyip\cbodbal.dat')
   11 FORMAT(8A2,8I5)                                                   ODKA0028
   12 FORMAT(10I8)                                                      ODKA0029
   30 FORMAT(A2)                                                        ODKA0030
   41 FORMAT(1X,118(1H-))                                               ODKA0031
      dummy=setvideomode( $ERESCOLOR )
      dummy=remappalette( 0,$BLUE )
      call clearscreen( $GCLEARSCREEN )
      open(1,file='f:\asyip\planbal7.dat')
      open(6,file='cbodb.txt')
      kol=1
c      READ (1,12) NKOL                                                   ODKA0032
      READ (1,12) (IPM(J),J=1,6)                                        ODKA0034
      READ (1,12) IP10                                                  ODKA0036
      KZP=0                                                             ODKA0038
      dummy2=settextcolor(2)
      call settextposition( 10,25, curpos ) 
      call outtext( 'ß†§†© §†‚„(ÁÁ¨¨££££)' )
      read(*,777) id
  777 format(i8)
      call settextposition( 11,25, curpos ) 
      call outtext( 'ß†§†© ≠Æ¨•‡ „Á†·‚™†( x) ®´® 00 §´Ô ¢·•Â' )
      read(*,778) nuh
  778 format(a2)
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
      L=24                                                              ODKA0072
      WRITE (6,2) ID                                                    ODKA0073
      WRITE (6,3)                                                       ODKA0074
      L=L-8                                                             ODKA0076
      ASSIGN 14 TO M
   13 READ(4,11,end=18) (U(J),J=1,8),K1,K2,K3,K4,K5,K6,K7,K8            ODKA0079
      NI=NI+1                                                           ODKA0080
      GOTO M,(14,15,23)                                                 ODKA0081
   15 CONTINUE                                                          ODKA0082
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
      if(nuh.eq.u(8)) pruh=1
      if(nuh.ne.u(8)) pruh=0
      if(nuh.eq.null) goto 555
      if(nuh.ne.u(8)) goto 556
  555 WRITE(6,4) (U(J),J=1,8),K1,KZP,K2,K3,IP1,K4,IP2,K5,IP3,K6,IP4,IP5,ODKA0118
     1K7,IP6,K8                                                         ODKA0119
      L=L-1                                                             ODKA0120
      IF(L.LE.0) WRITE (6,2) ID                                                 ODKA0073
      IF(L.LE.0) WRITE (6,3)                                                       ODKA0074
      IF(L.LE.0) L=16                                                   ODKA0121
  556 ASSIGN 15 TO M                                                    ODKA0122
      KU=U(8)                                                           ODKA0123
      GOTO 13                                                           ODKA0124
   17 JK=JK+1                                                           ODKA0125
      IP1=IM3*100/IPM(JK)                                               ODKA0126
      IP2=IM4*100/IM2                                                   ODKA0127
      IP3=IM5*100/IM4                                                   ODKA0128
      IP4=IM6*100/IM4                                                   ODKA0129
      IP5=IP3+IP4                                                       ODKA0130
      IP6=IM7*100/IM4                                                   ODKA0131
C      WRITE(6,6)                                                        ODKA0132
      if(nuh.eq.null) goto 557
      if(pruh.eq.0) goto 558
  557 WRITE(6,6) IM1,IPM(JK),IM2,IM3,IP1,IM4,IP2,IM5,IP3,IM6,IP4,IP5,IM7ODKA0133
     1,IP6,IM8                                                          ODKA0134
c      WRITE(6,40)                                                       ODKA0135
  558 IM1=0                                                             ODKA0136
      IM2=0                                                             ODKA0137
      IM3=0                                                             ODKA0138
      IM4=0                                                             ODKA0139
      IM5=0                                                             ODKA0140
      IM6=0                                                             ODKA0141
      IM7=0                                                             ODKA0142
      IM8=0                                                             ODKA0143
      L=L-1                                                             ODKA0144
      if(nuh.eq.null) goto 561
      if(pruh.eq.0)  goto 560
 561  IF(L.LE.0) WRITE (6,2) ID                                                 ODKA0073
      IF(L.LE.0) WRITE (6,3)                                                       ODKA0074
      IF(L.LE.0) L=16                                                   ODKA0121
 560  GOTO M1,(14,19)                                                   ODKA0145
   16 ASSIGN 19 TO M1                                                   ODKA0146
      MPR1=1                                                            ODKA0147
      GOTO 17                                                           ODKA0148
   19 IP1=IZ3*100/KPZ                                                   ODKA0149
      IP2=IZ4*100/IZ2                                                   ODKA0150
      IP3=IZ5*100/IZ4                                                   ODKA0151
      IP4=IZ6*100/IZ4                                                   ODKA0152
      IP5=IP3+IP4                                                       ODKA0153
      IP6=IZ7*100/IZ4                                                   ODKA0154
C      WRITE(6,7)                                                        ODKA0155
      if(nuh.ne.null) goto 559
      WRITE(6,7) IZ1,KPZ,IZ2,IZ3,IP1,IZ4,IP2,IZ5,IP3,IZ6,IP4,IP5,IZ7,IP6ODKA0156
     1,IZ8                                                              ODKA0157
c      WRITE(6,40)                                                       ODKA0158
C      WRITE(6,45)                                                       ODKA0159
      L=L-1                                                             ODKA0160
      IF(L.LE.0) WRITE (6,2) ID                                                 ODKA0073
      IF(L.LE.0) WRITE (6,3)                                                       ODKA0074
      IF(L.LE.0) L=16                                                   ODKA0121
  559 DO 20 J=1,L                                                       ODKA0162
      WRITE(6,40)                                                       ODKA0163
   20 CONTINUE                                                          ODKA0164
      GOTO M2,(21,222)                                                   ODKA0165
   18 ASSIGN 19 TO M1                                                   ODKA0166
      ASSIGN 222 TO M2                                                   ODKA0167
      GOTO 17                                                           ODKA0168
   21 CONTINUE                                                          ODKA0169
      ASSIGN 23 TO M                                                    ODKA0191
      GOTO 14        
  222 close (4)                                                         ODKA0192
   22 CONTINUE                                                          ODKA0193
      dummy=setvideomode($defaultmode)      
      stop
      END                                                               ODKA0232
