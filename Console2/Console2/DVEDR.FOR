      SUBROUTINE VEDMAT(A,NA,NXZ,P,L,XIFR,NM,ND,MK,SMAT)                VEDR0006
      INTEGER*2 A(26,1),NXZ(10,1),P(2,1),NAM(12),MARCA(10)              VEDR0007
      INTEGER*2 RAZ(8)                                                  VEDR0008
      REAL*8 V,V1,SMAT                                                  VEDR0009
      INTEGER XIFR(5000)                                                VEDR0010
  5   FORMAT(I8,12A2,8A2,10A2,I4,I9)                                    VEDR0011
C     DEFINE FILE 9(4400,104,L,MK)                                      VEDR0012
      SMAT=0.0                                                          VEDR0013
      K=0                                                               VEDR0014
      DO 1 I=1,NA                                                       VEDR0015
      LL=(A(3,I)*1000+A(4,I))*1000+A(5,I)                               VEDR0016
      K=K+1                                                             VEDR0017
      D1=A(6,I)/100.                                                    VEDR0018
      D2=A(7,I)/100.                                                    VEDR0019
      D3=A(8,I)/100.                                                    VEDR0020
      IF(LL.EQ.0.AND.D1.EQ.0.AND.D2.EQ.0.AND.D3.EQ.0) GO TO 1           VEDR0021
      J1=A(14,I)                                                        VEDR0022
      J2=A(15,I)+J1-1                                                   VEDR0023
      JM=0                                                              VEDR0024
      DO 3 J=J1,J2                                                      VEDR0025
  3   JM=JM+P(2,J)                                                      VEDR0026
      DO 4 J=1,NM                                                       VEDR0027
      IF(LL.NE.XIFR(J)) GO TO 4                                         VEDR0028
      READ(ND,5,REC=J) I3,NAM,RAZ,MARCA,I1,I2                           VEDR0029
  4   CONTINUE                                                          VEDR0030
  1   CONTINUE                                                          VEDR0031
      DO 14 I=1,NA                                                      VEDR0032
  14  A(16,I)=0                                                         VEDR0033
      DO 16 I=1,NA                                                      VEDR0034
      IF(A(16,I).GT.0) GO TO 16                                         VEDR0035
      A(16,I)=1                                                         VEDR0036
      D1=A(6,I)/100.                                                    VEDR0037
      D2=A(7,I)/100.                                                    VEDR0038
      D3=A(8,I)/100.                                                    VEDR0039
      V=A(6,I)/100.                                                     VEDR0040
      IF(A(8,I).GT.0) V=D1*D2*D3                                        VEDR0041
      IF(A(8,I).GT.0) GOTO 60                                           VEDR0042
      IF(A(7,I).GT.0) V=(3.14*D1*D2*D2)/4                               VEDR0043
  60  J1=A(14,I)                                                        VEDR0044
      J2=A(15,I)+J1-1                                                   VEDR0045
      JM=0                                                              VEDR0046
      DO 20 K=J1,J2                                                     VEDR0047
  20  JM=JM+P(2,K)                                                      VEDR0048
      V=V*JM                                                            VEDR0049
      I1=I+1                                                            VEDR0050
      IF(I1.GT.NA) GOTO 777                                              VEDR0051
      DO 17 J=I1,NA                                                     VEDR0052
      DO 18 JJ=3,5                                                      VEDR0053
      IF(A(JJ,I).NE.A(JJ,J)) GO TO 17                                   VEDR0054
  18  CONTINUE                                                          VEDR0055
      D1=A(6,J)/100.                                                    VEDR0056
      D2=A(7,J)/100.                                                    VEDR0057
      D3=A(8,J)/100.                                                    VEDR0058
      V1=A(6,J)/100.                                                    VEDR0059
      IF(A(8,J).GT.0) V1=D1*D2*D3                                       VEDR0060
      IF(A(8,J).GT.0) GOTO 61                                           VEDR0061
      IF(A(7,J).GT.0) V1=(3.14*D1*D2*D2)/4                              VEDR0062
  61  J1=A(14,J)                                                        VEDR0063
      J2=A(15,J)+J1-1                                                   VEDR0064
      JM=0                                                              VEDR0065
      DO 28 K=J1,J2                                                     VEDR0066
  28  JM=JM+P(2,K)                                                      VEDR0067
      V=V1*JM+V                                                         VEDR0068
      A(16,J)=1                                                         VEDR0069
  17  CONTINUE                                                          VEDR0070
 777  LL=(A(3,I)*1000+A(4,I))*1000+A(5,I)
      IF(LL.EQ.0) GOTO 16                                               VEDR0072
      DO 19 J=1,NM                                                      VEDR0073
      IF(LL.EQ.XIFR(J)) GO TO 21                                        VEDR0074
  19  CONTINUE                                                          VEDR0075
      GOTO 16                                                           VEDR0076
  21  READ(ND,5,REC=J) I3,NAM,RAZ,MARCA,I1,I2                           VEDR0077
      V=V*I1/10000.0                                                    VEDR0078
      V1=V*I2                                                           VEDR0079
      V1=V1/1000.                                                       VEDR0080
      WRITE (9,50) (NXZ(J,1),J=1,4),(NXZ(J,4),J=1,5),LL,NAM,RAZ,MARCA   VEDR0081
     1,V,V1                                                             VEDR0082
      MK=MK+1                                                           VEDR0083
      SMAT=SMAT+V1                                                      VEDR0084
  50  FORMAT(4A2,5A2,I8,12A2,8A2,10A2,F8.2,F10.2)                       VEDR0085
  16  CONTINUE                                                          VEDR0086
C     IF(MK.GT.1) GOTO 177                                              VEDR0087
C     END FILE 9                                                        VEDR0088
 177  RETURN                                                            VEDR0089
      END                                                               VEDR0090
