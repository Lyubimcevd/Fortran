      include  'fgraph.fi'
      include  'fgraph.fd'
      INTEGER STR10,STR15,MA1(3),MA5(3)                                 PPZ10001
      INTEGER*2 MS10,MS15                                               PPZ10002
      INTEGER S10,S15,OPT10(6),OPT15(6)                                 PPZ10003
      REAL ST10,ST15,SOPT10(6),SOPT15(6)                                PPZ10004
      INTEGER*2 ZAK(6),NAIM(5),KL(10),IZ(5)                             PPZ10005
      INTEGER KT,KOT(6)                                                 PPZ10006
      REAL BOT(6)                                                       PPZ10007
      INTEGER*2 CTM,CTM1,CTM5,null/'00'/,nuhb                                           PPZ10009
      record / rccoord / curpos
      integer*2 dummy2
      integer*4 dummy4,dummy
      dummy=setvideomode( $ERESCOLOR )
      dummy=remappalette( 0,$BLUE )
      call clearscreen( $GCLEARSCREEN )
C      PAUSE 'ìCTAHOBà ÅìMAÉì'                                           PPZ10010
   30 FORMAT(/,2X,'àTOÉO èO àè  ',T40,F8.1,T50,F8.1,T60,F7.1,T70,F7.1,  PPZ10011
     1T80,F7.1,T90,F7.1,T100,F7.1,T110,F7.1,T120,F8.1)                  PPZ10012
    1 FORMAT(1x,T114,'ãàCT',I3)                                           PPZ10013
    2 FORMAT(T41,'èOPTîEãú áAKAáOB èO àHëíê.èê-Çì')                     PPZ10014
   3  FORMAT(1X,I6)                                                     PPZ10015
   40 FORMAT(8A2)                                                       PPZ10016
    4 FORMAT(1X,128(1H-)/1X,1H!,T15,1H!,'HAàMEHOBA-!',T47,1H!,T49,'HOPM'PPZ10017
     *,T58,'!KOãà-!èPà-! ÑATA !Ñã.!CìMM.!',T92,'TPìÑOEMKOCTú èO OèEPAñàüPPZ10018
     *M',T122,1H!/1X,'!HOMEP áAKAáA!HàE àHCT- !',T31,'KãACCàîàKATOP   ! PPZ10019
     *TPìÑ-Tú  !óECT-!OPà-!èOTPEÅ!ñàK!TPì- !',35(1H-),1H!/1X,1H!,T15,1H!PPZ10020
     *,' PìMEHTA  !',T47,1H!,T58,1H!,' BO  !TET ! HOCTà!ãA !ÑOEM.! Cã. !PPZ10021
     * TOK.!îPEá.!òãàî.!K-P. !èPOó.!HEáAB'/1X,128(1H-))                 PPZ10022
    5 FORMAT(/,2X,'BCEÉO áAKAáOB',1X,I4/,/)                             PPZ10023
   50 FORMAT(1X,100(1H-))                                               PPZ10024
   13 FORMAT(1X,T3,6A2,T16,5A2,T27,10A2,T50,F6.1,T60,I5,T66,I1,T70,I6,T PPZ10025
     *77,I3,T81,F6.1,T87,F6.1,T93,F6.1,T99,F6.1,T105,F6.1,T111,F6.1,    PPZ10026
     *T117,F6.1,T123,F6.1)                                              PPZ10027
   82 FORMAT(1X,' àTOÉO',T50,F6.1,T81,F6.1,'   BCEÉO áAKAáOB: ',I5)     PPZ10028
   81 FORMAT(1X,128X)                                                   PPZ10029
      dummy2=settextcolor(2)
      call settextposition( 10,25, curpos ) 
      call outtext( 'ß†§†© §†‚„(ÁÁ¨¨££)' )
      read(*,777) idata
  777 format(i6)
      dummy2=settextcolor(2)
      call settextposition( 11,25, curpos ) 
      call outtext( 'ß†§†© ≠Æ¨•‡ „Á†·‚™†( Â),¢·•-00' )
      read(*,779) nuhb
  779 format(a2)
      open(1,file='port.txt')
      open (4,file='f:\asyip\prports.dat',access='direct',recl=122,
     1form='formatted')
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
      LL=0                                                              PPZ10048
      K=0                                                               PPZ10049
      M=0                                                               PPZ10050
      NC=1                                                              PPZ10051
      MS10=0                                                            PPZ10052
      MS15=0                                                            PPZ10053
      lst=1
   20 READ(4,33,end=8,rec=NC) NZ,ZAK,NAIM,KL,NTR,IQ,IP,ID,LZ,KT,KOT,NKT,
     1CTM   
      if(nuhb.eq.null) goto 4444
      if(kl(10).ne.nuhb) nc=nc+1
      if(kl(10).ne.nuhb) goto 20
 4444 continue
   33 format(i2,21a2,13i6)
      SKT=NKT/100.                                                      PPZ10057
      BKT=KT/100.                                                       PPZ10058
      TR=NTR/10.                                                        PPZ10059
      DO 25 L=1,6                                                       PPZ10060
   25 BOT(L)=(KOT(L))/100.                                              PPZ10061
    6 IF(K-0)9,9,10                                                     PPZ10063
   10 GOTO 62                                                           PPZ10064
      IF(K1-16) 80,85,85                                                PPZ10065
   85 ll=ll+1
      if(ll.ge.lst) write(1,1) LL                                                      PPZ10066
      if(ll.ge.lst) write(1,2)                                                        PPZ10068
      if(ll.ge.lst) write(1,3) IDATA                                                  PPZ10069
      if(ll.ge.lst) write(1,4)                                                        PPZ10070
      K1=0                                                              PPZ10071
   80 SNTR10=MA1(2)/10                                                  PPZ10072
      ST10=MA1(3)/100                                                   PPZ10073
      if(ll.ge.lst) write(1,82) SNTR10,ST10,MA1(1)                                    PPZ10074
      MA1(1)=0                                                          PPZ10075
      MA1(2)=0                                                          PPZ10076
      MA1(3)=0                                                          PPZ10077
      K1=K1+1                                                           PPZ10078
      DO 64 KI=K1,16                                                    PPZ10079
   64 if(ll.ge.lst) write(1,68)                                                       PPZ10080
   68 FORMAT(1X,' ')                                                    PPZ10081
      GOTO 9                                                            PPZ10082
   62 IF(CTM1.EQ.CTM) GOTO 60                                           PPZ10083
      DO 65 KI=K1,16                                                    PPZ10084
   65 if(ll.ge.lst) write(1,68)                                                       PPZ10085
      GOTO 9                                                            PPZ10086
   60 IF(K1-16) 11,9,9                                                  PPZ10087
c    9 pause'ìëíÄHéÇà ÅìåÄÉÄ à HÄÜåà äã."ENTER"'
   9  continue
      ll=ll+1
      if(ll.ge.lst) write(1,1) LL                                                      PPZ10088
      if(ll.ge.lst) write(1,2)                                                        PPZ10090
      if(ll.ge.lst) write(1,3) IDATA                                                  PPZ10091
      if(ll.ge.lst) write(1,4)                                                        PPZ10092
      K1=0                                                              PPZ10093
      MS10=KL(10)                                                       PPZ10094
      CTM1=CTM                                                          PPZ10095
   11 if(ll.ge.lst) write(1,13) ZAK,NAIM,KL,TR,IQ,IP,ID,LZ,BKT,
     1(BOT(N),N=1,6),SKT                                                PPZ10096
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
  8   ST10=S10/100.                                                     PPZ10143
      ST15=S15/100.                                                     PPZ10144
      STN10=N10/100.                                                    PPZ10145
      STN15=N15/100.                                                    PPZ10146
      SNTR10=STR10/10.                                                  PPZ10147
      SNTR15=STR15/10.                                                  PPZ10148
      DO 73 J=1,6                                                       PPZ10149
      SOPT10(J)=OPT10(J)/100.                                           PPZ10150
      SOPT15(J)=OPT15(J)/100.                                           PPZ10151
73    CONTINUE                                                          PPZ10152
      if(ll.ge.lst) write(1,30) SNTR10,STN10                                          ZONV0131
      if(ll.ge.lst) write(1,5) K                                                      ZONV0133
      kkk=k1
      DO 83 JJ=Kkk,16                                                   ZONV0135
   83 if(ll.ge.lst) write(1,81)                                                       ZONV0136
 888  continue
      dummy=setvideomode($defaultmode)      
      STOP                                                              PPZ10161
      END                                                               PPZ10162
