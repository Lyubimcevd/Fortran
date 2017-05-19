      SUBROUTINE DERPOD(I,A,P,TQC,W1,W2,W3,TQ,K)                        ZAK60006
      INTEGER*2 A(26,1),P(2,1),TQC(4,1),W1(1),W2(1),W3(1),
     1TQ(6,500)       
      JJ=0                                                              ZAK60008
      J=0                                                               ZAK60009
      J0=I                                                              ZAK60010
   12 J=J+1                                                             ZAK60011
      IF(J.GT.100) STOP 100                                             ZAK60012
      W1(J)=J0                                                          ZAK60013
      W2(J)=A(15,J0)                                                    ZAK60014
      IF(JJ.LE.0) GO TO 13                                              ZAK60015
    2 A(16,J0)=1                                                        ZAK60016
      L=A(12,J0)                                                        ZAK60017
      TQC(4,L)=1                                                        ZAK60018
      IF(TQ(3,L).LT.1000) GO TO 15                                      ZAK60019
      I1=TQ(3,L)/1000                                                   ZAK60020
      I2=TQ(3,L)-I1*1000                                                ZAK60021
      I1=I1+1                                                           ZAK60022
      DO 1 L=1,I1                                                       ZAK60023
      J1=I2+L-1                                                         ZAK60024
      J1=A(1,J1)                                                        ZAK60025
      IF(A(16,J1).GT.0) GO TO 1                                         ZAK60026
      K=K+1                                                             ZAK60027
      W3(K)=J1                                                          ZAK60028
    1 A(16,J1)=1                                                        ZAK60029
      GO TO 15                                                          ZAK60030
   13 I1=A(13,J0)                                                       ZAK60031
      IF(I1.LE.0) GO TO 15                                              ZAK60032
      DO 14 L=1,I1                                                      ZAK60033
      I2=A(12,J0)+L-1                                                   ZAK60034
      IF(TQC(4,I2).LE.0) GO TO 14                                       ZAK60035
      A(16,J0)=L                                                        ZAK60036
      JJ=J                                                              ZAK60037
      GO TO 15                                                          ZAK60038
   14 CONTINUE                                                          ZAK60039
   15 W2(J)=W2(J)-1                                                     ZAK60040
      IF(W2(J).LT.0) GO TO 16                                           ZAK60041
      J0=W1(J)                                                          ZAK60042
      J0=A(14,J0)+W2(J)                                                 ZAK60043
      J0=P(1,J0)                                                        ZAK60044
      IF(A(16,J0).LE.0) GO TO 12                                        ZAK60045
      IF(A(16,J0).GT.1.AND.JJ.GT.0) GO TO 2                             ZAK60046
      GO TO 15                                                          ZAK60047
   16 J=J-1                                                             ZAK60048
      IF(J.LE.0) RETURN                                                 ZAK60049
      IF(JJ.GT.J) JJ=0                                                  ZAK60050
      GO TO 15                                                          ZAK60051
      END                                                               ZAK60052
