      INTEGER STR10,STR15                                               PZOT0001
      LOGICAL*1 NAIZ(15)                                                PZOT0002
      INTEGER*2 MS10,MS15                                               PZOT0003
      INTEGER S10,S15,OPT10(6),OPT15(6)                                 PZOT0004
      REAL ST10,ST15,SOPT10(6),SOPT15(6)                                PZOT0005
      INTEGER*2 ZAK(6),NAIM(5),KL(10),IZ(5),IZD(2)                      PZOT0006
      INTEGER KT,KOT(6),DATAV                                           PZOT0007
      REAL BOT(6)                                                       PZOT0008
      INTEGER*2 CTM,CTM1,CTM5                                           PZOT0010
      INTEGER*2 GF1/'  '/,GF2/'  '/,TP/'TÏ'/                            PZOT0011
      integer*2 dummy2
      integer*4 dummy4,dummy                                      
   50 FORMAT(1X,127(1H-))                                               PZOT0013
   30 FORMAT(/,2X,'ÈTOÃO ÏO ÈÏ  ',T50,F8.1,T81,F8.1)                    PZOT0014
    1 FORMAT(/,/,/,/T120,'ËÈCT',I3)                                     PZOT0015
    2 FORMAT(T41,' ÏEPE×EHÜ OÁßÇATEËÜHOÉ HOMEHKËATÓPÛ ÏO ÈHÑÒÐ.ÏÐ-ÂÓ')  PZOT0016
   3  FORMAT(1X,I6)                                                     PZOT0017
   40 FORMAT(8A2)                                                       PZOT0018
    4 FORMAT(1X,127(1H-)/1X,1H!,T15,1H!,'HAÈMEHOBA-!',T31,1H!,T47,1H!,T6PZOT0019
     *8,1H!,T70,                                                        PZOT0020
     *'HOPM',T78,'!KOËÈ-!ÏPÈ-! ÄATA !ÄË.!      !        ÄATA        !'  PZOT0021
     */1X,'!HOMEP ÇAKAÇA!HÈE ÈHCT- !',T27,'ØÈÔP! HAÈMEHOBAHÈE  !HOM.×EPTPZOT0022
     *EÆA ÄETAËÈ  !TPÓÄ-TÜ  !×ECT-!OPÈ-!ÏOTPEÁ!ÖÈK!',                   PZOT0023
     *T107,'!',20(1H-),1H!/1X,1H!,T15,1H!                               PZOT0024
     *,' PÓMEHTA  !',T27,'ÈÇÄ !    ÈÇÄEËÈß    !',T68,1H!,T78,1H!,' BO  !PZOT0025
     *TET ! HOCTÈ!ËA !HEÇAB.!ÇAÏ-KA!KOMÏË.!BÛÏ-KA!'/1X,127(1H-))        PZOT0026
    5 FORMAT(/,2X,'BCEÃO ÇAKAÇOB',1X,I4/,/)                             PZOT0027
   13 FORMAT(1X,T3,6A2,T16,5A2,T27,2A2,T32,15A1,T48,10A2,T71,F6.1,T77,I5PZOT0028
     *,T86,I1,T90,I6,T97,I3,T101,F6.1,T108,I6,T115,I6,T122,I6)          PZOT0029
      open(666,file='tmp.txt')
      read(666,777) idata
  777 format(i6)
      read(666,779) kekz
  779 format(i2)
      do kkekz=1,kekz
      S10=0                                                             PZOT0031
      S15=0                                                             PZOT0032
      N15=0                                                             PZOT0033
      N10=0                                                             PZOT0034
      STR10=0                                                           PZOT0035
      STR15=0                                                           PZOT0036
      DO 70 J=1,6                                                       PZOT0037
      OPT10(J)=0                                                        PZOT0038
      OPT15(J)=0                                                        PZOT0039
70    CONTINUE                                                          PZOT0040
      I=1                                                               PZOT0041
      LL=1                                                              PZOT0042
      K=0                                                               PZOT0043
      M=0                                                               PZOT0044
      NC=1                                                              PZOT0045
      MS10=0                                                            PZOT0046
      MS15=0                                                            PZOT0047
      open(4,file='F:\ASUIPW\tek_INF\obnomt.dat',access='direct',
     1recl=165,form='formatted')                                
      open(1,file='prn')
   20 READ(4,23,end = 8) NZ,ZAK,NAIM,KL,NTR,IQ,IP,ID,LZ,KT,KOT,NKT,     
     *ctm,ld,mdk,DATAV,IZD,NAIZ                                         PZOT0050
   23 format(i2,21a2,17i6,2a2,15a1)
      IF(ZAK(5).EQ.TP) NC=NC+1                                          PZOT0052
      IF(ZAK(5).EQ.TP) GOTO 20                                          PZOT0053
      SKT=NKT/100.                                                      PZOT0054
      BKT=KT/100.                                                       PZOT0055
      TR=NTR/10.                                                        PZOT0056
      DO 25 L=1,6                                                       PZOT0057
   25 BOT(L)=(KOT(L))/100.                                              PZOT0058
    6 IF(K-0)9,9,60                                                     PZOT0060
   60 IF(K1-60) 11,9,9                                                  PZOT0061
    9 WRITE(1,1) I                                                      PZOT0062
      I=I+1                                                             PZOT0063
      WRITE(1,2)                                                        PZOT0064
      WRITE(1,3) IDATA                                                  PZOT0065
      WRITE(1,4)                                                        PZOT0066
      K1=0                                                              PZOT0067
      MS10=KL(10)                                                       PZOT0068
      CTM1=CTM                                                          PZOT0069
      GF1=ZAK(5)                                                        PZOT0070
      GF2=ZAK(6)                                                        PZOT0071
   11 IF(GF1.EQ.ZAK(5).AND.GF2.EQ.ZAK(6)) GOTO 51                       PZOT0072
      WRITE(1,50)                                                       PZOT0073
      GF1=ZAK(5)                                                        PZOT0074
      GF2=ZAK(6)                                                        PZOT0075
      K1=K1+1                                                           PZOT0076
      DO 80 JJ=K1,60                                                    PZOT0077
   80 WRITE(1,81)                                                       PZOT0078
   81 FORMAT (1X,128X)                                                  PZOT0079
      K1=60                                                             PZOT0080
      GOTO 60                                                           PZOT0081
   51 WRITE(1,13) ZAK,NAIM,IZD,NAIZ,KL,TR,IQ,IP,ID,LZ,SKT,LD,MDK,DATAV  PZOT0082
      S10=S10+KT                                                        PZOT0083
      N10=N10+NKT                                                       PZOT0084
      STR10=STR10+NTR                                                   PZOT0085
      DO 71 J=1,6                                                       PZOT0086
      OPT10(J)=OPT10(J)+KOT(J)                                          PZOT0087
71    CONTINUE                                                          PZOT0088
      K1=K1+1                                                           PZOT0089
      K=K+1                                                             PZOT0090
      NC=NC+1                                                           PZOT0091
      GOTO 20                                                           PZOT0092
  8   ST10=S10/100.                                                     PZOT0126
      ST15=S15/100.                                                     PZOT0127
      STN10=N10/100.                                                    PZOT0128
      STN15=N15/100.                                                    PZOT0129
      SNTR10=STR10/10.                                                  PZOT0130
      SNTR15=STR15/10.                                                  PZOT0131
      DO 73 J=1,6                                                       PZOT0132
      SOPT10(J)=OPT10(J)/100.                                           PZOT0133
      SOPT15(J)=OPT15(J)/100.                                           PZOT0134
73    CONTINUE                                                          PZOT0135
      WRITE(1,30) SNTR10,STN10                                          PZOT0136
      WRITE(1,5) K                                                      PZOT0138
      kk=k1
      DO 83 JJ=Kk,60                                                    PZOT0140
   83 WRITE(1,81)                                                       PZOT0141
  888 enddo     
      STOP                                                              PZOT0144
      END                                                               PZOT0145
 
