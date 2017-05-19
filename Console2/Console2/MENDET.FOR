      SUBROUTINE MEHDET(NA,NT,A,S,P,TQ,TQC)                             ZAK20006
      INTEGER*2 A(26,1),S(2,1),P(2,1),TQ(6,1),TQC(4,1),
     1W1(100)/100*0/,  
     2W2(100)/100*0/,W3(50)/50*0/,W4(50)/50*0/                          ZAK20008
      DATA NK/82/                                                       ZAK20009
      DO 11 I=1,NA                                                      ZAK20010
   11 A(16,I)=0                                                         ZAK20011
      IT=0                                                              ZAK20012
      DO 10 I=1,NT                                                      ZAK20013
      TQC(4,I)=0                                                        ZAK20014
      I1=TQ(2,I )-TQ(2,I )/100*100                                      ZAK20015
      IF(I1.LE.NK) GO TO 10                                             ZAK20016
      IF(TQ(4,I).GT.IT) IT=TQ(4,I)                                      ZAK20017
      TQC(4,I)=1                                                        ZAK20018
   10 CONTINUE                                                          ZAK20019
      IF(IT.LE.0) GO TO 24                                              ZAK20020
      IT=IT+1                                                           ZAK20021
      K=0                                                               ZAK20022
      DO 9 I=1,NA                                                       ZAK20023
      IF(A(11,I).GT.0) GO TO 9                                          ZAK20024
      CALL DERPOD(I,A,P,TQC,W1,W2,W3,TQ,K)                              ZAK20025
    9 CONTINUE                                                          ZAK20026
   15 CONTINUE                                                          ZAK20027
      IF(K.LE.0) GO TO 12                                               ZAK20028
      DO 14 I=1,K                                                       ZAK20029
   14 W4(I)=W3(I)                                                       ZAK20030
      K1=K                                                              ZAK20031
      K=0                                                               ZAK20032
      DO 13 I=1,K1                                                      ZAK20033
      J1=W4(I)                                                          ZAK20034
      CALL DERPOD(J1,A,P,TQC,W1,W2,W3,TQ,K)                             ZAK20035
   13 CONTINUE                                                          ZAK20036
      GO TO 15                                                          ZAK20037
   12 J0=1                                                              ZAK20038
      J=0                                                               ZAK20039
    2 J=J+1                                                             ZAK20040
      W1(J)=J0                                                          ZAK20041
      W2(J)=A(11,J0)                                                    ZAK20042
      I1=A(13,J0)                                                       ZAK20043
      IF(I1.LE.0) GO TO 3                                               ZAK20044
      IF(A(16,J0)-1) 5,3,6                                              ZAK20045
    6 I1=A(16,J0)-1                                                     ZAK20046
    5 I2=A(12,J0)+I1-1                                                  ZAK20047
      I3=W1(J-1)                                                        ZAK20048
      I3=A(12,I3)                                                       ZAK20049
      IF(TQ(6,I2).GT.TQ(4,I3)) GO TO 4                                  ZAK20050
      II=TQ(4,I3)-TQ(6,I2)+1                                            ZAK20051
      GO TO 8                                                           ZAK20052
    4 CONTINUE                                                          ZAK20053
      IF(TQ(6,I2).GE.IT) GO TO 3                                        ZAK20054
      II=IT-TQ(6,I2)                                                    ZAK20055
    8 DO 7 I=1,I1                                                       ZAK20056
      I2=A(12,J0)+I-1                                                   ZAK20057
      TQ(4,I2)=TQ(4,I2)+II                                              ZAK20058
      TQ(5,I2)=TQ(5,I2)+II                                              ZAK20059
    7 TQ(6,I2)=TQ(6,I2)+II                                              ZAK20060
      IF(A(16,J0).GT.0) GO TO 3                                         ZAK20061
      A(16,J0)=-1                                                       ZAK20062
    3 W2(J)=W2(J)-1                                                     ZAK20063
      IF(W2(J).LT.0) GO TO 1                                            ZAK20064
      J0=A(10,J0)+W2(J)                                                 ZAK20065
      J0=S(1,J0)                                                        ZAK20066
      GO TO 2                                                           ZAK20067
    1 J=J-1                                                             ZAK20068
      IF(J.LE.0) GO TO 24                                               ZAK20069
      J0=W1(J)                                                          ZAK20070
      GO TO 3                                                           ZAK20071
   24 DO 17 I=1,NA                                                      ZAK20072
   17 A(16,I)=-1                                                        ZAK20073
      RETURN                                                            ZAK20074
      END                                                               ZAK20075
