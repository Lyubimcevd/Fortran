      SUBROUTINE SGCORR(JT,ND,A,S,TQ)                                   RZAK0006
      INTEGER*2 A(26,1),S(2,1),TQ(6,1),W1(100),W2(100)                  RZAK0007
      I=TQ(6,JT)+ND                                                     RZAK0008
      IPR=0                                                             RZAK0009
      J=0                                                               RZAK0010
    1 J=J+1                                                             RZAK0011
      NN=TQ(3,JT)                                                       RZAK0012
      NK=NN/1000                                                        RZAK0013
      W2(J)=NK+1                                                        RZAK0014
      NN=NN-NK*1000                                                     RZAK0015
      W1(J)=NN                                                          RZAK0016
      J1=A(1,NN)                                                        RZAK0017
      J0=A(12,J1)                                                       RZAK0018
      J2=JT+1                                                           RZAK0019
      DO 2 J1=J0,JT                                                     RZAK0020
      J2=J2-1                                                           RZAK0021
      J3=I-TQ(6,J2)                                                     RZAK0022
      IPR=0                                                             RZAK0023
      IF(J3.LE.0) GOTO 3                                                RZAK0024
      IF(TQ(6,J2-1).EQ.TQ(4,J2)) IPR=1                                  RZAK0025
      DO 4 J4=4,6                                                       RZAK0026
    4 TQ(J4,J2)=TQ(J4,J2)+J3                                            RZAK0027
      IF(IPR.EQ.1) GOTO 2                                               RZAK0028
      I=TQ(4,J2)+1                                                      RZAK0029
    2 CONTINUE                                                          RZAK0030
    5 W2(J)=W2(J)-1                                                     RZAK0031
      IF(W2(J).LT.0) GOTO 7                                             RZAK0032
   10 J1=NN+W2(J)                                                       RZAK0033
      J1=A(1,J1)                                                        RZAK0034
    6 A(16,J1)=A(16,J1)-1                                               RZAK0035
      IF(A(16,J1).LT.0) GOTO 5                                          RZAK0036
      J2=A(10,J1)+A(16,J1)                                              RZAK0037
      J2=S(1,J2)                                                        RZAK0038
      J3=A(12,J2)                                                       RZAK0039
      IF(J3.EQ.J0) GOTO 6                                               RZAK0040
      JT=A(13,J2)+J3-1                                                  RZAK0041
      GOTO 1                                                            RZAK0042
    7 NK=NN+NK                                                          RZAK0043
      DO 8 J1=NN,NK                                                     RZAK0044
      J2=A(1,J1)                                                        RZAK0045
    8 A(16,J2)=A(11,J2)                                                 RZAK0046
    3 J=J-1                                                             RZAK0047
      IF(J.LE.0) GOTO 9                                                 RZAK0048
      NN=W1(J)                                                          RZAK0049
      J1=A(1,NN)                                                        RZAK0050
      J0=A(12,J1)                                                       RZAK0051
      I=TQ(4,J0)+1                                                      RZAK0052
      NK=TQ(3,J0)/1000                                                  RZAK0053
      GOTO 10                                                           RZAK0054
    9 RETURN                                                            RZAK0055
      END                                                               RZAK0056
