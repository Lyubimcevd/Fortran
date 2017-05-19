      INTEGER STR10,STR15                                               ZONV0001
      INTEGER*2 MS10,MS15                                               ZONV0002
      INTEGER S10,S15,OPT10(6),OPT15(6)                                 ZONV0003
      REAL ST10,ST15,SOPT10(6),SOPT15(6)                                ZONV0004
      INTEGER*2 ZAK(6),NAIM(5),KL(10),IZ(5)                             ZONV0005
      INTEGER KT,KOT(6),DATAV                                           ZONV0006
      REAL BOT(6)                                                       ZONV0007
      INTEGER*2 CTM,CTM1,CTM5                                           ZONV0009
      INTEGER*2 GF1/'  '/,GF2/'  '/                                     ZONV0010
      integer*2 dummy2
      integer*4 dummy4,dummy
   50 FORMAT(1X,107(1H-))                                               ZONV0012
   30 FORMAT(/,2X,'ÈTOÃO ÏO ÈÏ  ',T50,F8.1,T81,F8.1)                    ZONV0013
    1 FORMAT(/,/,/,/T114,'ËÈCT',I3)                                     ZONV0014
    2 FORMAT(T30,' ÏEPE×EHÜ ÍÅÂÛÏÎËÍÅÍÍÎÉ OÁßÇATEËÜHOÉ HOMEHKËATÓPÛ ÏO È
     1HÑÒÐ.ÏÐ-ÂÓ')                                                      ZONV0015
   3  FORMAT(1X,I6)                                                     ZONV0016
   40 FORMAT(8A2)                                                       ZONV0017
    4 FORMAT(1X,107(1H-)/1X,1H!,T15,1H!,'HAÈMEHOBA-!',T47,1H!,T49,'HOPM'ZONV0018
     *,T58,'!KOËÈ-!ÏPÈ-! ÄATA !ÄË.!      !        ÄATA        !'        ZONV0019
     */1X,'!HOMEP ÇAKAÇA!HÈE ÈHCT- !',T31,'KËACCÈÔÈKATOP   !TPÓÄ-TÜ   !×ZONV0020
     *ECT-!OPÈ-!ÏOTPEÁ!ÖÈK!',T87,'!',20(1H-),1H!/1X,1H!,T15,1H!         ZONV0021
     *,' PÓMEHTA  !',T47,1H!,T58,1H!,' BO  !TET ! HOCTÈ!ËA !HEÇAB.!ÇAÏ-KZONV0022
     *A!KOMÏË.!BÛÏ-KA!'/1X,107(1H-))                                    ZONV0023
    5 FORMAT(/,2X,'BCEÃO ÇAKAÇOB',1X,I4/,/)                             ZONV0024
   13 FORMAT(1X,1x,6A2,1x,5A2,1x,10A2,3x,F6.1,3x,I5,4x,I1,1x,I6,        ZONV0025
     *1x,I3,1x,F6.1,1x,I6,1x,I6,1x,I6)                                  ZONV0026
      open(666,file='tmp.txt')
      read(666,777) idata
  777 format(i6)
      read(666,779) kekz
  779 format(i2)
      open(1,file='prn')
      open (4,file='F:\ASUIPW\tek_INF\obnomnv.dat',access='direct',
     1recl=146,form='formatted')
      do kkekz=1,kekz
      S10=0                                                             ZONV0028
      S15=0                                                             ZONV0029
      N15=0                                                             ZONV0030
      N10=0                                                             ZONV0031
      STR10=0                                                           ZONV0032
      STR15=0                                                           ZONV0033
      DO 70 J=1,6                                                       ZONV0034
      OPT10(J)=0                                                        ZONV0035
      OPT15(J)=0                                                        ZONV0036
70    CONTINUE                                                          ZONV0037
      I=1                                                               ZONV0038
      LL=1                                                              ZONV0039
      K=0                                                               ZONV0040
      M=0                                                               ZONV0041
      NC=1                                                              ZONV0042
      MS10=0                                                            ZONV0043
      MS15=0                                                            ZONV0044
   20 READ(4,23,end=8) NZ,ZAK,NAIM,KL,NTR,IQ,IP,ID,LZ,KT,KOT,NKT,      
     1ctm,ld,mdk,DATAV                                                  ZONV0047
   23 format(i2,21a2,17i6)
      SKT=NKT/100.                                                      ZONV0049
      BKT=KT/100.                                                       ZONV0050
      TR=NTR/10.                                                        ZONV0051
      DO 25 L=1,6                                                       ZONV0052
   25 BOT(L)=(KOT(L))/100.                                              ZONV0053
    6 IF(K-0)9,9,60                                                     ZONV0055
   60 IF(K1-60) 11,9,9                                                  ZONV0056
    9 WRITE(1,1) I                                                      ZONV0057
      I=I+1                                                             ZONV0058
      WRITE(1,2)                                                        ZONV0059
      WRITE(1,3) IDATA                                                  ZONV0060
      WRITE(1,4)                                                        ZONV0061
      K1=0                                                              ZONV0062
      MS10=KL(10)                                                       ZONV0063
      CTM1=CTM                                                          ZONV0064
      GF1=ZAK(5)                                                        ZONV0065
      GF2=ZAK(6)                                                        ZONV0066
   11 IF(GF1.EQ.ZAK(5).AND.GF2.EQ.ZAK(6)) GOTO 51                       ZONV0067
      WRITE(1,50)                                                       ZONV0068
      GF1=ZAK(5)                                                        ZONV0069
      GF2=ZAK(6)                                                        ZONV0070
      K1=K1+1                                                           ZONV0071
      DO 80 JJ=K1,60                                                    ZONV0072
   80 WRITE(1,81)                                                       ZONV0073
   81 FORMAT (1X,128X)                                                  ZONV0074
      K1=60                                                             ZONV0075
      GOTO 60                                                           ZONV0076
   51 WRITE(1,13) (ZAK(i),i=1,6),(NAIM(i),i=1,5),(KL(i),i=1,10),TR,IQ,IP
     1,ID,LZ,SKT,LD,MDK,DATAV           
      S10=S10+KT                                                        ZONV0078
      N10=N10+NKT                                                       ZONV0079
      STR10=STR10+NTR                                                   ZONV0080
      DO 71 J=1,6                                                       ZONV0081
      OPT10(J)=OPT10(J)+KOT(J)                                          ZONV0082
71    CONTINUE                                                          ZONV0083
      K1=K1+1                                                           ZONV0084
      K=K+1                                                             ZONV0085
      NC=NC+1                                                           ZONV0086
      GOTO 20                                                           ZONV0087
   8  ST10=S10/100.                                                     ZONV0121
      ST15=S15/100.                                                     ZONV0122
      STN10=N10/100.                                                    ZONV0123
      STN15=N15/100.                                                    ZONV0124
      SNTR10=STR10/10.                                                  ZONV0125
      SNTR15=STR15/10.                                                  ZONV0126
      DO 73 J=1,6                                                       ZONV0127
      SOPT10(J)=OPT10(J)/100.                                           ZONV0128
      SOPT15(J)=OPT15(J)/100.                                           ZONV0129
73    CONTINUE                                                          zONV0130
      WRITE(1,30) SNTR10,STN10                                          ZONV0131
      WRITE(1,5) K                                                      ZONV0133
      kkk=k1
      DO 83 JJ=Kkk,60                                                   ZONV0135
   83 WRITE(1,81)                                                       ZONV0136
  888 enddo  
      STOP                                                              ZONV0139
      END                                                               ZONV0140
 
