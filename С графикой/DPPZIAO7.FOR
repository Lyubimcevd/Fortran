      INCLUDE  'FGRAPH.FI'
      INCLUDE  'FGRAPH.FD'
      INTEGER STR10,STR15,MA1(3),MA5(3)                                 15090001
      INTEGER*2 MS10,MS15                                               15090002
      INTEGER S10,S15,OPT10(6),OPT15(6)                                 15090003
      REAL ST10,ST15,SOPT10(6),SOPT15(6)                                15090004
      INTEGER*2 ZAK(6),NAIM(5),KL(10),IZ(5)                             15090005
      INTEGER KT,KOT(6)                                                 15090006
      real bot(6)
      integer*2 ctm,ctm1,ctm5
      character*4 zakzao/'2046'/
      character*4 zakaz
      equivalence(zak,zakaz)
      record / rccoord / curpos
      integer*2 dummy2
      integer*4 dummy4,dummy
      dummy=setvideomode( $ERESCOLOR )
      dummy=remappalette( 0,$BLUE )
      call clearscreen( $GCLEARSCREEN )
   30 FORMAT(/,2X,'àTOÉO èO àè  ',T40,F8.1,T50,F8.1,T60,F7.1,T70,F7.1,  15090010
     1T80,F7.1,T90,F7.1,T100,F7.1,T110,F7.1,T120,F8.1)                  15090011
    1 FORMAT(/,/,/,/T114,'ãàCT',I3)                                     15090012
    2 FORMAT(T41,'àHBEHTAPàáAñàOHHAü BEÑOMOCTú èO HàíÖã"àHëíêìåÖHí"')   15090013
   3  FORMAT(1X,I8)                                                     15090014
   40 FORMAT(8A2)                                                       15090015
    4 FORMAT(1X,128(1H-)/1X,1H!,T15,1H!,'HAàMEHOBA-!',T47,1H!,T49,'HOPM'15090016
     *,T58,'!KOãà-!èPà-! ÑATA !Ñã.!CìMM.!',T92,'TPìÑOEMKOCTú èO OèEPAñàü15090017
     *M',T122,1H!/1X,'!HOMEP áAKAáA!HàE àHCT- !',T31,'KãACCàîàKATOP   ! 15090018
     *TPìÑ-Tú  !óECT-!OPà-!èOTPEÅ!ñàK!TPì- !',35(1H-),1H!/1X,1H!,T15,1H!15090019
     *,' PìMEHTA  !',T47,1H!,T58,1H!,' BO  !TET ! HOCTà!ãA !ÑOEM.! Cã. !15090020
     * TOK.!îPEá.!òãàî.!K-P. !èPOó.!HEáAB'/1X,128(1H-))                 15090021
    5 FORMAT(/,2X,'BCEÉO áAKAáOB',1X,I4/,/)                             15090022
   13 FORMAT(1X,T3,6A2,T16,5A2,T27,10A2,T50,F6.1,T60,I5,T66,I1,T70,I6,T 15090023
     *77,I3,T81,F7.2,T87,F7.2,T93,F7.2,T99,F7.2,T105,F7.2,T111,F7.2,    15090024
     *T117,F7.2,T123,F7.2)                                              15090025
   82 FORMAT(1X,' àTOÉO',T50,F8.1,T81,F8.1,'   BCEÉO áAKAáOB: ',I5)     15090026
      open(4,file='f:\asyip\invzaks.dat')
      open(1,file='prn')
      dummy2=settextcolor(2)
      call settextposition( 10,25, curpos ) 
      call outtext( 'ß†§†© §†‚„(ÁÁ¨¨££££)' )
      read(*,777) idata
  777 format(i8)
      DO 85 J=1,3                                                       15090028
      MA1(J)=0                                                          15090029
   85 MA5(J)=0                                                          15090030
      S10=0                                                             15090031
      S15=0                                                             15090032
      N15=0                                                             15090033
      N10=0                                                             15090034
      STR10=0                                                           15090035
      STR15=0                                                           15090036
      DO 70 J=1,6                                                       15090037
      OPT10(J)=0                                                        15090038
      OPT15(J)=0                                                        15090039
70    CONTINUE                                                          15090040
      I=1                                                               15090041
      LL=1                                                              15090042
      K=0                                                               15090043
      M=0                                                               15090044
      NC=1                                                              15090045
      MS10=0                                                            15090046
      MS15=0                                                            15090047
c      DEFINE FILE 4(7000,108,L,NMKZ)                                   15090048
   20 READ(4,50,end=8) NZ,ZAK,NAIM,KL,NTR,IQ,IP,ID,LZ,KT,KOT,NKT,CTM    15090049
   50 format(i2,6a2,5a2,10a2,2i5,i1,i6,i3,8i6,i2)
      IF(ZAKAZ.EQ.ZAKZAO) GOTO 20
      SKT=NKT/100.                                                      15090051
      BKT=KT/100.                                                       15090052
      TR=NTR/10.                                                        15090053
      DO 25 L=1,6                                                       15090054
   25 BOT(L)=(KOT(L))/100.                                              15090055
    6 IF(K-0)9,9,10                                                     15090057
   10 IF(MS10.EQ.KL(10)) GOTO 62                                        15090058
      IF(K1-60) 80,81,81                                                15090059
   81 WRITE(1,1) I                                                      15090060
      I=I+1                                                             15090061
      WRITE(1,2)                                                        15090062
      WRITE(1,3) IDATA                                                  15090063
      WRITE(1,4)                                                        15090064
      K1=0                                                              15090065
   80 SNTR10=MA1(2)/10.                                                 15090066
      ST10=MA1(3)/100.                                                  15090067
      WRITE(1,82) SNTR10,ST10,MA1(1)                                    15090068
      DO 86 J=1,3                                                       15090069
   86 MA1(J)=0                                                          15090070
      K1=K1+1                                                           15090071
      DO 64 KI=K1,60                                                    15090072
   64 WRITE(1,68)                                                       15090073
   68 FORMAT(1X,' ')                                                    15090074
      GOTO 9                                                            15090075
   62 IF(CTM1.EQ.CTM) GOTO 60                                           15090076
      DO 65 KI=K1,60                                                    15090077
   65 WRITE(1,68)                                                       15090078
      GOTO 9                                                            15090079
   60 IF(K1-60) 11,9,9                                                  15090080
    9 WRITE(1,1) I                                                      15090081
      I=I+1                                                             15090082
      WRITE(1,2)                                                        15090083
      WRITE(1,3) IDATA                                                  15090084
      WRITE(1,4)                                                        15090085
      K1=0                                                              15090086
      MS10=KL(10)                                                       15090087
      CTM1=CTM                                                          15090088
   11 WRITE(1,13) ZAK,NAIM,KL,TR,IQ,IP,ID,LZ,BKT,(BOT(N),N=1,6),SKT     15090089
      MA1(1)=MA1(1)+1                                                   15090090
      MA1(2)=MA1(2)+NTR                                                 15090091
      MA1(3)=MA1(3)+KT                                                  15090092
      S10=S10+KT                                                        15090093
      N10=N10+NKT                                                       15090094
      STR10=STR10+NTR                                                   15090095
      DO 71 J=1,6                                                       15090096
      OPT10(J)=OPT10(J)+KOT(J)                                          15090097
71    CONTINUE                                                          15090098
      K1=K1+1                                                           15090099
      K=K+1                                                             15090100
      NC=NC+1                                                           15090101
      GOTO 20                                                           15090102
    8 SNTR10=MA1(2)/10.                                                 15090149
      SNTR15=MA5(2)/10.                                                 15090150
      ST10=MA1(3)/100.                                                  15090151
      ST15=MA5(3)/100.                                                  15090152
      WRITE(1,82) SNTR10,ST10,MA1(1)                                    15090153
      ST10=S10/100.                                                     15090155
      ST15=S15/100.                                                     15090156
      STN10=N10/100.                                                    15090157
      STN15=N15/100.                                                    15090158
      SNTR10=STR10/10.                                                  15090159
      SNTR15=STR15/10.                                                  15090160
      DO 73 J=1,6                                                       15090161
      SOPT10(J)=OPT10(J)/100.                                           15090162
      SOPT15(J)=OPT15(J)/100.                                           15090163
73    CONTINUE                                                          15090164
      WRITE(1,30) SNTR10,ST10,(SOPT10(J),J=1,6),STN10                   15090165
      WRITE(1,5) K                                                      15090167
      dummy=setvideomode($defaultmode)      
      STOP                                                              15090169
      END                                                               15090170
