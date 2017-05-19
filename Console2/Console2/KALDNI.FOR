      SUBROUTINE KALDNI(ND,LP,KDP)                                      LDNI0006
      INTEGER*2 KM(13)/0 ,31,28,31,30,31,30,31,31,30,31,30,31/,         LDNI0007
     *KDP(1),R/'R '/                                                    LDNI0008
      INTEGER*2 NDN(7)/'R ','R ','R ','R ','R ','N ','N '/              LDNI0009
      INTEGER*2 NDP(8)/ 101,803, 105, 205, 905, 710, 711, 811/          LDNI0010
      ID=ND/1000000                                                     LDNI0011
      M=ND/10000-ID*100                                                 LDNI0012
      IG=ND-ID*1000000-M*10000                                          LDNI0013
    1 FORMAT(20X,20HHEBEPHO ‡A„AHA „ATA:,I9)                            LDNI0014
      IF(ID*(13-M)*IG.GT.0) GO TO 14                                    LDNI0015
      PRINT 1, ND                                                       LDNI0016
      GO TO 15                                                          LDNI0017
   14 CONTINUE                                                          LDNI0018
      IG1=IG/4                                                          LDNI0019
      IF(IG.EQ.IG1*4) KM(3)=29                                          LDNI0020
      M1=0                                                              LDNI0021
      DO 2 I=1,M                                                        LDNI0022
    2 M1=M1+KM(I)                                                       LDNI0023
      M2=(IG-1977)*365+(IG-1976)/4+M1+ID-2                              LDNI0024
      IF(IG.NE.IG1*4) GOTO 3                                            LDNI0025
      M2=M2-1                                                           LDNI0026
    3 IN=M2-(M2/7)*7                                                    LDNI0027
      IF(IN.EQ.0) IN=7                                                  LDNI0028
      LP1=LP*2                                                          LDNI0029
      IF(LP.NE.0) GO TO 12                                              LDNI0030
      LP1=1                                                             LDNI0031
      GOTO 13                                                           LDNI0032
   12 ID1=ID                                                            LDNI0033
      M1=M                                                              LDNI0034
      IN1=IN                                                            LDNI0035
      K=LP+2                                                            LDNI0036
   11 IF(NDN(IN1).NE.R) GO TO 8                                         LDNI0037
      DO 10 I=1,8                                                       LDNI0038
      M2=ID1*100+M1                                                     LDNI0039
      IF(M2.EQ.NDP(I)) GOTO 8                                           LDNI0040
   10 CONTINUE                                                          LDNI0041
      IF(K.EQ.LP+2.AND.ID.NE.ID1) K=K-1                                 LDNI0042
      K=K-1                                                             LDNI0043
      KDP(K)=ID1*1000+M1*10+IN1                                         LDNI0044
    8 ID1=ID1-1                                                         LDNI0045
      IF(ID1.GT.0) GO TO 9                                              LDNI0046
      M1=M1-1                                                           LDNI0047
      IF(M1.EQ.0) M1=12                                                 LDNI0048
      ID1=KM(M1+1)                                                      LDNI0049
    9 IN1=IN1-1                                                         LDNI0050
      IF(IN1.EQ.0) IN1=7                                                LDNI0051
      IF(K.GT.1) GO TO 11                                               LDNI0052
   13 K=LP                                                              LDNI0053
    7 IF(NDN(IN).NE.R) GO TO 4                                          LDNI0054
      DO 6 I=1,8                                                        LDNI0055
      M1=ID*100+M                                                       LDNI0056
      IF(M1.EQ.NDP(I)) GO TO 4                                          LDNI0057
    6 CONTINUE                                                          LDNI0058
      K=K+1                                                             LDNI0059
      KDP(K)=ID*1000+M*10+IN                                            LDNI0060
    4 ID=ID+1                                                           LDNI0061
      IF( ID.LE.KM(M+1)) GOTO 5                                         LDNI0062
      ID=1                                                              LDNI0063
      M=M+1                                                             LDNI0064
      IF(M.EQ.13) M=1                                                   LDNI0065
    5 IN=IN+1                                                           LDNI0066
      IF(IN.EQ.8) IN=1                                                  LDNI0067
      IF(K.LT.LP1) GOTO 7                                               LDNI0068
      KM(3)=28                                                          LDNI0069
   15 RETURN                                                            LDNI0070
      END                                                               LDNI0071
