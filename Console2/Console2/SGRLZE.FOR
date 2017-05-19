      SUBROUTINE SGRLZE(NI,NA,NT,NU,A,S,P,TQ,QZ,TQC,U,IP,IW)            RLZE0006
      INTEGER IW(100)                                                   RLZE0007
      INTEGER*2 A(26,1),S(2,1),P(2,1),TQ(6,1),TQC(4,1),T1(100),T2(100)  RLZE0008
      INTEGER*2 U(20,1)                                                 RLZE0009
      INTEGER QZ(4,1)                                                   RLZE0010
      DO 15 J=1,NA                                                      RLZE0011
   15 A(16,J)=A(11,J)                                                   RLZE0012
      NU1=NU-1                                                          RLZE0013
      NI=0                                                              RLZE0014
    1 NI=NI+1                                                           RLZE0015
      IPRK=0                                                            RLZE0016
      DO 2 JU=1,NU1                                                     RLZE0017
      IF(IP.EQ.1.AND.IW(JU).LT.(U(4,JU)*U(3,JU)+50)/100) GOTO 2         RLZE0018
      IF(IP.NE.1.AND.IW(JU).LT.U(2,JU)) GOTO 2                          RLZE0019
      IPRK=1                                                            RLZE0020
      IND=-1                                                            RLZE0021
      M=0                                                               RLZE0022
      DO 3 J=1,NT                                                       RLZE0023
      IF(TQC(2,J).EQ.0) GOTO 3                                          RLZE0024
      J1=TQ(4,J)                                                        RLZE0025
      IF(J1.LT.NI) GOTO 3                                               RLZE0026
      IND=1                                                             RLZE0027
      J2=TQ(5,J)                                                        RLZE0028
      IF(J2.GT.NI.OR.TQ(1,J).NE.JU) GOTO 3                              RLZE0029
      M=M+1                                                             RLZE0030
      IF(M.GT.100) STOP 51                                              RLZE0031
      T1(M)=J                                                           RLZE0032
      T2(M)=0                                                           RLZE0033
    3 CONTINUE                                                          RLZE0034
      IF(IND.LT.0) GOTO 4                                               RLZE0035
      IF(M.LE.0) GOTO 2                                                 RLZE0036
      LT=U(2,JU)                                                        RLZE0037
      LG=LT                                                             RLZE0038
      IF(IP.LE.3.AND.LT.LE.(U(4,JU)*U(3,JU)+50)/100) LG=(U(4,JU)*U(3,JU)RLZE0039
     1+50)/100                                                          RLZE0040
      DO 6 J1=1,M                                                       RLZE0041
      JQ=LT                                                             RLZE0042
      J=T1(J1)                                                          RLZE0043
      J2=TQ(5,J)                                                        RLZE0044
      IF(J2.GE.NI) JQ=QZ(1,J)-(TQ(4,J)-J2)*LT                           RLZE0045
    6 LG=LG-JQ                                                          RLZE0046
      IF(LG.GE.0) GOTO 2                                                RLZE0047
      DO 7 J0=1,M                                                       RLZE0048
      DO 12 J1=1,M                                                      RLZE0049
      IF(T2(J1)) 13,13,12                                               RLZE0050
   12 CONTINUE                                                          RLZE0051
   13 JM=J1                                                             RLZE0052
      I=T1(J1)                                                          RLZE0053
      DO 8 J2=J1,M                                                      RLZE0054
      IF(T2(J2).GT.0) GOTO 8                                            RLZE0055
      J=T1(J2)                                                          RLZE0056
      IF(TQC(3,I)-TQC(3,J)) 8,9,10                                      RLZE0057
    9 IF(QZ(4,I)-QZ(4,J)) 8,11,10                                       RLZE0058
   11 IF(QZ(1,I)-QZ(1,J)) 10,8,8                                        RLZE0059
   10 JM=J2                                                             RLZE0060
      I=J                                                               RLZE0061
    8 CONTINUE                                                          RLZE0062
      J2=TQ(5,I)                                                        RLZE0063
      T2(JM)=NI-J2+1                                                    RLZE0064
      JQ=LT                                                             RLZE0065
      IF(J2.GE.NI) JQ=QZ(1,I)-(TQ(4,I)-J2)*LT                           RLZE0066
      LG=LG+JQ                                                          RLZE0067
      IF(LG.GE.0) GOTO 14                                               RLZE0068
    7 CONTINUE                                                          RLZE0069
   14 DO 5 J=1,M                                                        RLZE0070
      J2=T2(J)                                                          RLZE0071
      IF(J2.LE.0) GOTO 5                                                RLZE0072
      J1=T1(J)                                                          RLZE0073
      CALL SGCORR(J1,J2,A,S,TQ)                                         RLZE0074
    5 CONTINUE                                                          RLZE0075
    2 CONTINUE                                                          RLZE0076
      IF(IPRK.EQ.0) GOTO 4                                              RLZE0077
      GOTO 1                                                            RLZE0078
    4 DO 16 J=1,NA                                                      RLZE0079
   16 A(16,J)=-1                                                        RLZE0080
      RETURN                                                            RLZE0081
      END                                                               RLZE0082
