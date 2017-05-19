      INTEGER STR10,STR15,MA1(3),MA5(3)                                 PPZ10001
      INTEGER*2 MS10,MS15                                               PPZ10002
      INTEGER S10,S15,OPT10(6),OPT15(6)                                 PPZ10003
      REAL ST10,ST15,SOPT10(6),SOPT15(6)                                PPZ10004
      INTEGER*2 ZAK(6),NAIM(5),KL(10),IZ(5)                             PPZ10005
      INTEGER KT,KOT(6)                                                 PPZ10006
      REAL BOT(6)                                                       PPZ10007
      INTEGER*2 CTM,CTM1,CTM5                                           PPZ10009
      integer*2 dummy2
      integer*4 dummy4,dummy
   30 FORMAT(/,2X,'ÈTOÃO ÏO ÖEXÓ',T40,F8.1,T50,F8.1,T60,F7.1,T70,F7.1,  PPZ10011
     1T80,F7.1,T90,F7.1,T100,F7.1,T110,F7.1,T120,F8.1)                  PPZ10012
    1 FORMAT(/,/,/,/T114,'ËÈCT',I3)                                     PPZ10013
    2 FORMAT(T41,'ÏOPTÔEËÜ ÇAKAÇOB ÏO ',I2,' ÖEXÓ ')                    PPZ10014
   3  FORMAT(1X,I6)                                                     PPZ10015
   40 FORMAT(8A2)                                                       PPZ10016
    4 FORMAT(1X,128(1H-)/1X,1H!,T15,1H!,'HAÈMEHOBA-!',T47,1H!,T49,'HOPM'PPZ10017
     *,T58,'!KOËÈ-!ÏPÈ-! ÄATA !ÄË.!CÓMM.!',T92,'TPÓÄOEMKOCTÜ ÏO OÏEPAÖÈßPPZ10018
     *M',T122,1H!/1X,'!HOMEP ÇAKAÇA!HÈE ÈHCT- !',T31,'KËACCÈÔÈKATOP   ! PPZ10019
     *TPÓÄ-TÜ  !×ECT-!OPÈ-!ÏOTPEÁ!ÖÈK!TPÓ- !',35(1H-),1H!/1X,1H!,T15,1H!PPZ10020
     *,' PÓMEHTA  !',T47,1H!,T58,1H!,' BO  !TET ! HOCTÈ!ËA !ÄOEM.! CË. !PPZ10021
     * TOK.!ÔPEÇ.!ØËÈÔ.!K-P. !ÏPO×.!HEÇAB'/1X,128(1H-))                 PPZ10022
    5 FORMAT(/,2X,'BCEÃO ÇAKAÇOB',1X,I4/,/)                             PPZ10023
   50 FORMAT(1X,100(1H-))                                               PPZ10024
   13 FORMAT(1X,T3,6A2,T16,5A2,T27,10A2,T50,F6.1,T60,I5,T66,I1,T70,I6,T PPZ10025
     *77,I3,T81,F6.1,T87,F6.1,T93,F6.1,T99,F6.1,T105,F6.1,T111,F6.1,    PPZ10026
     *T117,F6.1,T123,F6.1)                                              PPZ10027
   82 FORMAT(1X,' ÈTOÃO',T50,F6.1,T81,F6.1,'   BCEÃO ÇAKAÇOB: ',I5)     PPZ10028
   81 FORMAT(1X,128X)                                                   PPZ10029
      open(666,file='tmp.txt')
      read(666,777) idata
  777 format(i6)
      read(666,778) nomzex
  778 format(i2)
      read(666,779) kekz
  779 format(i2)
      open(1,file='prn')
      open(4,file='F:\ASUIPW\tek_INF\prports.dat',access='direct',
     1recl=142,form='formatted')
      do kkekz=1,kekz
      MA1(1)=0                                                          PPZ10031
      MA1(2)=0                                                          PPZ10032
      MA1(3)=0                                                          PPZ10033
      MA5(1)=0                                                          PPZ10034
      MA5(2)=0                                                          PPZ10035
      MA5(3)=0                                                          PPZ10036
      S10=0                                                             PPZ10037
      S15=0                                                             PPZ10038
      N15=0                                                             PPZ10039
      N10=0                                                             PPZ10040
      STR10=0                                                           PPZ10041
      STR15=0                                                           PPZ10042
      DO 70 J=1,6                                                       PPZ10043
      OPT10(J)=0                                                        PPZ10044
      OPT15(J)=0                                                        PPZ10045
70    CONTINUE                                                          PPZ10046
      I=1                                                               PPZ10047
      LL=1                                                              PPZ10048
      K=0                                                               PPZ10049
      M=0                                                               PPZ10050
      NC=1                                                              PPZ10051
      MS10=0                                                            PPZ10052
      MS15=0                                                            PPZ10053
   20 READ(4,33,end = 8) NZ,ZAK,NAIM,KL,NTR,IQ,IP,ID,LZ,KT,KOT,NKT,     
     1CTM   
   33 format(i2,21a2,13i6)
      if(nomzex.ne.nz) goto 22
      SKT=NKT/100.                                                      PPZ10057
      BKT=KT/100.                                                       PPZ10058
      TR=NTR/10.                                                        PPZ10059
      DO 25 L=1,6                                                       PPZ10060
   25 BOT(L)=(KOT(L))/100.                                              PPZ10061
      IF(NZ-15) 6,7,21                                                  PPZ10062
    6 IF(K-0)9,9,10                                                     PPZ10063
   10 GOTO 62                                                           PPZ10064
      IF(K1-60) 80,85,85                                                PPZ10065
   85 WRITE(1,1) I                                                      PPZ10066
      I=I+1                                                             PPZ10067
      WRITE(1,2) NZ                                                     PPZ10068
      WRITE(1,3) IDATA                                                  PPZ10069
      WRITE(1,4)                                                        PPZ10070
      K1=0                                                              PPZ10071
   80 SNTR10=MA1(2)/10                                                  PPZ10072
      ST10=MA1(3)/100                                                   PPZ10073
      WRITE(1,82) SNTR10,ST10,MA1(1)                                    PPZ10074
      MA1(1)=0                                                          PPZ10075
      MA1(2)=0                                                          PPZ10076
      MA1(3)=0                                                          PPZ10077
      K1=K1+1                                                           PPZ10078
      DO 64 KI=K1,60                                                    PPZ10079
   64 WRITE(1,68)                                                       PPZ10080
   68 FORMAT(1X,' ')                                                    PPZ10081
      GOTO 9                                                            PPZ10082
   62 IF(CTM1.EQ.CTM) GOTO 60                                           PPZ10083
      DO 65 KI=K1,60                                                    PPZ10084
   65 WRITE(1,68)                                                       PPZ10085
      GOTO 9                                                            PPZ10086
   60 IF(K1-60) 11,9,9                                                  PPZ10087
    9 WRITE(1,1) I                                                      PPZ10088
      I=I+1                                                             PPZ10089
      WRITE(1,2)NZ                                                      PPZ10090
      WRITE(1,3) IDATA                                                  PPZ10091
      WRITE(1,4)                                                        PPZ10092
      K1=0                                                              PPZ10093
      MS10=KL(10)                                                       PPZ10094
      CTM1=CTM                                                          PPZ10095
   11 WRITE(1,13) ZAK,NAIM,KL,TR,IQ,IP,ID,LZ,BKT,(BOT(N),N=1,6),SKT     PPZ10096
      MA1(1)=MA1(1)+1                                                   PPZ10097
      MA1(2)=MA1(2)+NTR                                                 PPZ10098
      MA1(3)=MA1(3)+KT                                                  PPZ10099
      S10=S10+KT                                                        PPZ10100
      N10=N10+NKT                                                       PPZ10101
      STR10=STR10+NTR                                                   PPZ10102
      DO 71 J=1,6                                                       PPZ10103
      OPT10(J)=OPT10(J)+KOT(J)                                          PPZ10104
71    CONTINUE                                                          PPZ10105
      K1=K1+1                                                           PPZ10106
      K=K+1                                                             PPZ10107
      NC=NC+1                                                           PPZ10108
      GOTO 20                                                           PPZ10109
    7 IF(M-0)14,14,61                                                   PPZ10110
   61 IF(K5-60) 16,14,14                                                PPZ10111
   14 WRITE(1,1) LL                                                     PPZ10112
      LL=LL+1                                                           PPZ10113
      WRITE(1,2) NZ                                                     PPZ10114
      WRITE(1,3) IDATA                                                  PPZ10115
      WRITE(1,4)                                                        PPZ10116
      K5=0                                                              PPZ10117
      MS15=KL(10)                                                       PPZ10118
      CTM5=CTM                                                          PPZ10119
      GF1=ZAK(5)                                                        PPZ10120
      GF2=ZAK(6)                                                        PPZ10121
   16 GOTO 52                                                           PPZ10122
      WRITE(1,50)                                                       PPZ10123
      GF1=ZAK(5)                                                        PPZ10124
      GF2=ZAK(6)                                                        PPZ10125
      K5=K5+1                                                           PPZ10126
      DO 822 JJ=K5,60                                                   PPZ10127
  822 WRITE(1,81)                                                       PPZ10128
      K5=60                                                             PPZ10129
      GOTO 61                                                           PPZ10130
   52 WRITE(1,13) ZAK,NAIM,KL,TR,IQ,IP,ID,LZ,BKT,(BOT(N),N=1,6),SKT     PPZ10131
      S15=S15+KT                                                        PPZ10132
      N15=N15+NKT                                                       PPZ10133
      STR15=STR15+NTR                                                   PPZ10134
      DO 72 J=1,6                                                       PPZ10135
      OPT15(J)=OPT15(J)+KOT(J)                                          PPZ10136
72    CONTINUE                                                          PPZ10137
      K5=K5+1                                                           PPZ10138
      M=M+1                                                             PPZ10139
   21 CONTINUE                                                          PPZ10140
   22 NC=NC+1                                                           PPZ10141
      GOTO 20                                                           PPZ10142
   8  if(nomzex.eq.10.and.k.eq.0) goto 888
      if(nomzex.eq.15.and.m.eq.0) goto 888
      ST10=S10/100.                                                     PPZ10143
      ST15=S15/100.                                                     PPZ10144
      STN10=N10/100.                                                    PPZ10145
      STN15=N15/100.                                                    PPZ10146
      SNTR10=STR10/10.                                                  PPZ10147
      SNTR15=STR15/10.                                                  PPZ10148
      DO 73 J=1,6                                                       PPZ10149
      SOPT10(J)=OPT10(J)/100.                                           PPZ10150
      SOPT15(J)=OPT15(J)/100.                                           PPZ10151
73    CONTINUE                                                          PPZ10152
      if(nomzex.eq.10) WRITE(1,30) SNTR10,STN10                         ZONV0131
      if(nomzex.eq.15) WRITE(2,30) SNTR15,STN15                         ZONV0132
      if(nomzex.eq.10) WRITE(1,5) K                                     ZONV0133
      if(nomzex.eq.15) WRITE(2,5)M                                      ZONV0134
      if(nomzex.eq.10) kkk=k1
      if(nomzex.eq.15) kkk=k5
      DO 83 JJ=Kkk,60                                                   ZONV0135
   83 WRITE(1,81)                                                       ZONV0136
 888  enddo
      STOP                                                              PPZ10161
      END                                                               PPZ10162
 
