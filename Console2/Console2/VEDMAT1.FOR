      SUBROUTINE PACMAT(A,NA,NXZ,P,XIFR,NM,ND,RASZ)                     MAT10006
      INTEGER*2 A(26,1),NXZ(10,1),P(2,1),NAM(12),MARCA(10)              MAT10007
      INTEGER*2 RAZ(8)                                                  MAT10008
      REAL*8 V,V1,RASZ                                                  MAT10009
      INTEGER XIFR(5000)                                                MAT10010
  5   FORMAT(I8,12A2,8A2,10A2,I4,I9)                                    MAT10011
      RASZ=0.0                                                          MAT10012
      K=0                                                               MAT10013
      DO 1 I=2,NA                                                       MAT10014
      LL=(A(3,I)*1000+A(4,I))*1000+A(5,I)                               MAT10015
      K=K+1                                                             MAT10016
      D1=A(6,I)/10.                                                     MAT10017
      D2=A(7,I)/10.                                                     MAT10018
      D3=A(8,I)/10.                                                     MAT10019
      IF(LL.EQ.0.AND.D1.EQ.0.AND.D2.EQ.0.AND.D3.EQ.0) GO TO 1           MAT10020
      J1=A(14,I)                                                        MAT10021
      J2=A(15,I)+J1-1                                                   MAT10022
      JM=0                                                              MAT10023
      DO 3 J=J1,J2                                                      MAT10024
  3   JM=JM+P(2,J)                                                      MAT10025
      DO 4 J=1,NM                                                       MAT10026
      IF(LL.NE.XIFR(J)) GO TO 4                                         MAT10027
      READ(ND,5,rec=J) I3,NAM,RAZ,MARCA,I1,I2                           MAT10028
  4   CONTINUE                                                          MAT10029
  1   CONTINUE                                                          MAT10030
      DO 14 I=1,NA                                                      MAT10031
  14  A(16,I)=0                                                         MAT10032
      DO 16 I=2,NA                                                      MAT10033
      IF(A(16,I).GT.0) GO TO 16                                         MAT10034
      A(16,I)=1                                                         MAT10035
      D1=A(6,I)/100.                                                    MAT10036
      D2=A(7,I)/100.                                                    MAT10037
      D3=A(8,I)/100.                                                    MAT10038
      V=A(6,I)/100.                                                     MAT10039
      IF(A(8,I).GT.0) V=D1*D2*D3                                        MAT10040
      IF(A(8,I).GT.0) GOTO 60                                           MAT10041
      IF(A(7,I).GT.0) V=(3.14*D1*D2*D2)/4                               MAT10042
  60  J1=A(14,I)                                                        MAT10043
      J2=A(15,I)+J1-1                                                   MAT10044
      JM=0                                                              MAT10045
      DO 20 K=J1,J2                                                     MAT10046
  20  JM=JM+P(2,K)                                                      MAT10047
      V=V*JM                                                            MAT10048
      I1=I+1                                                            MAT10049
      IF(I1.GT.NA) GOTO 177                                             MAT10050
      DO 17 J=I1,NA                                                     MAT10051
      DO 18 JJ=3,5                                                      MAT10052
      IF(A(JJ,I).NE.A(JJ,J)) GO TO 17                                   MAT10053
  18  CONTINUE                                                          MAT10054
      D1=A(6,J)/100.                                                    MAT10055
      D2=A(7,J)/100.                                                    MAT10056
      D3=A(8,J)/100.                                                    MAT10057
      V1=A(6,J)/100.                                                    MAT10058
      IF(A(8,J).GT.0) V1=D1*D2*D3                                       MAT10059
      IF(A(8,J).GT.0) GOTO 61                                           MAT10060
      IF(A(7,J).GT.0) V1=(3.14*D1*D2*D2)/4                              MAT10061
      IF(A(7,J).EQ.0.AND.A(8,J).EQ.0) V=V*10
  61  J1=A(14,J)                                                        MAT10062
      J2=A(15,J)+J1-1                                                   MAT10063
      JM=0                                                              MAT10064
      DO 28 K=J1,J2                                                     MAT10065
  28  JM=JM+P(2,K)                                                      MAT10066
      V=V1*JM+V                                                         MAT10067
      A(16,J)=1                                                         MAT10068
  17  CONTINUE                                                          MAT10069
  177 LL=(A(3,I)*1000+A(4,I))*1000+A(5,I)                               MAT10070
      IF(LL.EQ.0) GOTO 16                                               MAT10071
      DO 19 J=1,NM                                                      MAT10072
      IF(LL.EQ.XIFR(J)) GO TO 21                                        MAT10073
  19  CONTINUE                                                          MAT10074
      GOTO 16                                                           MAT10075
  21  READ(ND,5,rec=J) I3,NAM,RAZ,MARCA,I1,I2                           MAT10076
      V=V*I1/10000.0                                                    MAT10077
      V1=V*I2                                                           MAT10078
      V1=V1/1000.                                                       MAT10079
      RASZ=RASZ+V1                                                      MAT10080
  16  CONTINUE                                                          MAT10081
      RETURN                                                            MAT10082
      END                                                               MAT10083
