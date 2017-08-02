      INTEGER*2 A(26,200),S(2,200),P(2,200),TQ(6,500),TQC(4,500),U(20,100),NXZ(10,4),AZ(8,4000),ZOP(2,8000),ZP(2,300),KDP(180)     
      INTEGER ZG(100,60),UC(100)
      INTEGER QZ(4,500),IW(100),UW(2,200)                               
      INTEGER*2 KDPP(180),ZAKP(4,100)/400*'99'/               
      INTEGER*2 IC(2,65),PIC(65),KPRP(1)                               
      character*1 prd/' '/,py/' '/
      DATA LIMZ/4000/,LMZP/300/,LIMO/8000/                              
      DATA NF1/4/,NF2/3/,ND/10/                                         
      DATA NZ,JO,JD,NPZ/4*0/                                            
      open(10,file='D:\work.dat',access='direct',recl=120)
    2 FORMAT (10I8)                                                     
   30 FORMAT(4A2,72X)                                                   
    1 FORMAT(A2,A2,I2,7X)                                               
      open(unit=nf1,file='D:\u.dat',form='unformatted')
      READ (NF1) NW,NU                                                  
      READ (NF1) ((UW(I,J),I=1,2),J=1,NW),((U(I,J),I=1,20),J=1,NU)      
      REWIND NF1                                                        
      I=0                                                 
      DO 4 J=1,NU                                                       
      IF(U(7,J).LE.0) GO TO 4                                           
      I=I+1                                                             
      U(7,J)=I  
      UC(I)=(int4(U(3,J))*int4(U(4,J))+50)/100                          
    4 CONTINUE                                                  
      NUC=I                                                             
      jw=1                                                              
      ld=0                                                              
      open(666, file = 'tmp.txt')
      open(5,file='D:\prizp.dat')
      do 7777 i=1,65
      READ (5,1) IC(1,I),IC(2,I),PIC(I)                                 
 7777 continue
      read(666,777) nn
  777 format(i8)
      read(666,777) nk
  781 CALL KALDNI(NK,0,KDP)                                             
      NK1=KDP(1)                                                        
      CALL KALDNI(NN,90,KDP)                                            
      DO 5 J=91,180                                                     
      IF(NK1.EQ.KDP(J)) GOTO 6                                          
    5 CONTINUE                                                          
    6 LP=J+LD-90                                                        
      CALL KALDNI(NN,LP,KDP)                                            
      open(unit=2,file='D:\plan.dat',form='unformatted')
      open(unit=nf2,file='D:\plan1.dat',form='unformatted')
      open(unit=1,file='D:\portfel.dat',form='unformatted')
      open(unit=8,file='D:\portfel1.dat',form='unformatted')
      IF(JW.LE.0) GOTO 7                                                
      READ(02) I,J,K                                                    
      I1=I+I                                                            
      READ(02) (KDPP(M),M=1,I1)                                         
      CALL KALDNI(K,0,KPRP)                                             
      DO 10 M=1,I1                                                      
      IF(KDPP(M).EQ.KPRP(1)) GOTO 11                                    
   10 CONTINUE                                                          
   11 KPRP=M-I                             
      open(9,file='D:\progz.dat')
      READ(9,30,END=31) ((ZAKP(I,J),I=1,4),J=1,100)                  
   31 CALL INPACK(ND,2,NU,NUC,LP,NZ,NPZ,JO,JD,LIMZ,LIMO,LMZP,AZ,ZOP,ZG,ZP,U,NXZ,IW,A,S,P,TQ,QZ,TQC,KDP,K5,K55,K,KPRP(1),IC,PIC,ZAKP) 
    7 CONTINUE                                         
      CALL INPACK(ND,1,NU,NUC,LP,NZ,NPZ,JO,JD,LIMZ,LIMO,LMZP,AZ,ZOP,ZG,ZP,U,NXZ,IW,A,S,P,TQ,QZ,TQC,KDP,K5,K55,K,KPRP(1),IC,PIC,ZAKP) 
      CALL PERPLN(NZ,NPZ,LP,LD,NUC,JO,ZG,UC,AZ,ZOP)                     
      CALL OBPLAN(ND,LP,LD,NZ,NPZ,NUC,AZ,ZP,ZG,UC)
      CALL PLTPGN(2,1,8,NF2,9,NU,NUC,LP,NN,NK,NZ,NPZ,A,S,P,TQ,QZ,TQC,NXZ,IW,KDP(1),AZ,ZG,U,K5,K55)              
      STOP                                                              
      END                                                               
      SUBROUTINE INPACK(ND,NF1,NU,NUC,LP,NZ,NPZ,JO,JD,LIMZ,LIMO,LMZP,AZ,ZOP,ZG,ZP,U,NXZ,IW,A,S,P,TQ,QZ,TQC,KDP,K5,K55,KPP,KPRP,IC,PIC,ZAKP)                                                             
      INTEGER*2 A(26,1),S(2,1),P(2,1),TQ(6,1),TQC(4,1),NXZ(10,4),U(20,1),AZ(8,1),ZP(2,1),ZOP(2,1),KDP(1),ZAKP(4,1)                                                 
      INTEGER*2 CD/'CO'/                                                
      INTEGER*2 MET/'—œ'/                                               
      INTEGER*2 MET1/'  '/                                              
      INTEGER QZ(4,1),IW(NU),W(20),ZG(100,1)                            
      INTEGER*2 CON,C                                                   
      INTEGER*2 IC(2,65),PIC(65),ZAK(2)                                 
      LOGICAL*1 XVOST(2),IZD(2),D/' '/                                  
      INTEGER*2 NOMZ                                                    
      INTEGER*2 XVOST2,D2/'  '/                                         
      INTEGER*2 PTQ(500),RAZ                                            
      EQUIVALENCE (NOMZ,XVOST(1)),(XVOST(2),XVOST2),(ZAK(2),IZD(1))     
      DATA NSLES/83/,C/'99'/                                            
   76 FORMAT (6(I4,4X))                                                 
      IF(NF1.EQ.2) NF=6                                                 
      IF(NF1.EQ.1) NF=5                                                 
      K5=0                                                              
      K55=0                                                           
      LP1=LP+LP                                                         
      DO 1 J=1,LP                                                       
      DO 1 I=1,NUC                                                      
    1 ZG(I,J)=0                                                         
      DO 2 K=1,2500                                         
      IF(NF1.NE.2) GOTO 35                                              
      READ(NF1) NXZ,IW,IQ,IP,ID,LD,LZ,LF,MP,MS,NA,NS,NT             
      IF(LF.LE.KPRP) IPRZ=0                                             
      IF(LF.GT.KPRP) IPRZ=1                                       
      GOTO 34                                                           
   35 IF(NF1.EQ.1) READ(NF1) NXZ,IW,IQ,IP,ID,LZ,MP,NA,NS,NT            
   34 IF(NA.LE.-1000) GOTO 4                                             
      READ (NF1) ((A(I,J),I=1,26),J=1,NA)                               
      READ (NF1) ((S(I,J),I=1,2),J=1,NS),((P(I,J),I=1,2),J=1,NS)        
      READ (NF1) ((TQ(I,J),I=1,6),J=1,NT),((QZ(I,J),I=1,4),J=1,NT)      
      READ (NF1) ((TQC(I,J),I=1,4),J=1,NT)                              
      PRG=0                                                             
      IF(ZAKP(1,1).EQ.C) GOTO 34444                                     
      DO 32222 J=1,100                                                  
      IF(ZAKP(1,J).EQ.C) GOTO 34444                                     
      DO 33333 I=1,4                                                    
      IF(NXZ(I,1).NE.ZAKP(I,J)) GOTO 32222                              
33333 CONTINUE                                                          
      PRG=1                                                             
      GOTO 34444                                                        
32222 CONTINUE                                                          
34444 CON=99                                                            
      N=A(10,1)                                                         
      N=S(1,N)                                                          
      N=A(12,N)+A(13,N)-1                                               
      N=TQ(1,N)                                                         
      N=U(1,N)                                                          
      IF(NF.EQ.5.AND.N.EQ.17) K5=K5+1                                   
      IF(NF.EQ.5.AND.N.EQ.15) K55=K55+1                                 
      IF(NF.EQ.5.AND.IP.EQ.9) GOTO 2                                    
      XVOST2=D2                                                         
      ZAK(1)=NXZ(1,1)                                                   
      ZAK(2)=NXZ(2,1)                                                   
      IZD(2)=D                                                          
      NOMZ=NXZ(4,1)                                                     
      IF(XVOST2.EQ.D2) IZD(1)=D                                         
      DO 601 I=1,65                                                     
      IF(IC(1,I).EQ.ZAK(1).AND.IC(2,I).EQ.ZAK(2)) CON=PIC(I)            
  601 CONTINUE                                                          
      IF(NXZ(5,1).EQ.CD.AND.NF.EQ.6) GOTO 2                             
      IF(N.EQ.99) GOTO 2                                                
  600 CONTINUE                                                          
      J1=A(10,1)                                                        
      J2=A(11,1)+J1-1                                                   
      DO 19 J3=J1,J2                                                    
      J=S(1,J3)                                                         
      DO 20 I=1,10                                                      
      IF(A(I+16,J).NE.NXZ(I,2)) GOTO 19                                 
   20 CONTINUE                                                          
      GOTO 21                                                           
   19 CONTINUE                                                          
   21 CONTINUE                                                          
      DO 720 KOL=1,2                                                    
      DO 49 J3=2,NA                                                     
      J2=A(12,J3)+A(13,J3)-1                                            
      J1=A(12,J3)                                                       
      NPOS=J2-1                                                         
      DO 50 J6=J1,J2                                                    
      KJ=J2-J6                                                          
      J4=J1+KJ                                                          
      IF(J4.EQ.J2.AND.TQC(2,J4).NE.0) GOTO 22                                                           
      IF(TQC(2,J4).NE.0) GOTO 50                                         
      IF(KJ.EQ.0) GOTO 50                                                
      GOTO 700                                                          
   22 NOMP=A(14,J3)                                                     
      KOLP=A(15,J3)                                                     
      KON=NOMP+KOLP-1                                                   
      IF(NA.EQ.3.AND.A(9,2).EQ.A(9,3)) GOTO 50                          
      DO 710 J10=NOMP,KON                                               
      NSB=P(1,J10)                                                      
      IF(NSB.EQ.1) GOTO 710                                             
      NSB=A(12,NSB)                                                     
      IF(TQC(2,NSB).NE.0) GOTO 710                                      
      KJ=KJ+1                                                           
      GOTO 700                                                          
  710 CONTINUE                                                          
      GOTO 50                                                           
  700 KJ=KJ+1                                                           
      DO 51 JK=1,KJ                                                     
      J5=J1+JK-1                                                        
  51  TQC(2,J5)=0                                                       
      GOTO 49                                                           
   50 CONTINUE                                                          
   49 CONTINUE                                                          
  720 CONTINUE                                                          
      J3=A(10,1)                                                        
      J3=A(11,1)+J3-1                                                   
      J3=S(1,J3)                                                        
      J8=A(12,J3)+A(13,J3)-1                                            
      J1=A(12,J3)                                                       
      DO 150 J6=J1,J8                                                   
      KJ=J8-J6                                                          
      J4=J1+KJ                                                          
      IF(TQ(2,J4)-TQ(2,J4)/100*100.NE.83) GOTO 150                      
      R=(QZ(1,J4)+0.0)/U(2,TQ(1,J4))                                    
      J5=INT(R)                                                         
      IF(R-J5.LE.0.0) J5=J5-1                                           
      IF(NF.EQ.5) J66=TQ(4,J4)-TQ(5,J4)                                 
      IF(NF.EQ.6) J66=TQ(5,J4)-TQ(4,J4)                                 
      J7=J66-J5                                                         
      GOTO 151                                                          
 150  CONTINUE                                                          
      J7=0                                                              
  151 CONTINUE                                                          
      J=A(12,J)+A(13,J)-1                                               
      J=TQ(1,J)                                                         
      NC=U(1,J)                                                         
      IF(NXZ(5,1).EQ.MET) GOTO 14                                       
      IF(TQC(1,1).LT.0.AND.IQ.EQ.0) GOTO 14                             
      IF(TQC(1,1).LT.0) TQC(2,1)=0                                      
      IF((IQ-QZ(2,1)-TQC(2,1)).GT.0) GOTO 14                            
      GO TO 2                                                           
  140 J1=A(10,1)                                                        
      J2=A(11,1)+J1-1                                                   
      DO 13 J3=J1,J2                                                    
      J=S(1,J3)                                                         
      J=A(12,J)+A(13,J)-1                                               
      IF(TQC(2,J).GT.0)GOTO 14                                          
   13 CONTINUE                                                          
      GOTO 2                                                             
   14 LZ=0                                                              
      NT1=NT                                                            
      NT=0                                                              
      DO 3 J=2,NT1                                                      
      IF(TQC(2,J).LE.0) GO TO 3                                         
      NT=NT+1                                                           
      IF(NF.EQ.5) GOTO 555                                              
      IF(J.EQ.1) GOTO 152                                               
      IF(J.GT.J4.AND.J.LE.J8) GOTO 152                                  
      TQ(4,J)=LF-TQ(4,J)-J7                                             
      IF(J.EQ.J4) GOTO 553                                              
      TQ(5,J)=LF-TQ(5,J)-J7                                             
      TQ(6,J)=LF-TQ(6,J)-J7                                             
      GOTO 53                                                           
  152 CONTINUE                                                          
      TQ(4,J)=LF-TQ(4,J)                                                
  553 TQ(5,J)=LF-TQ(5,J)                                                
      TQ(6,J)=LF-TQ(6,J)                                                
      IF(NF.EQ.6) GOTO 53                                               
  555 IF(J.EQ.1) GOTO 53                                                
      IF(J.GT.J4.AND.J.LE.J8) GOTO 53                                   
      TQ(4,J)=TQ(4,J)-J7                                                
      IF(J.EQ.J4) GOTO 53                                               
      TQ(5,J)=TQ(5,J)-J7                                                
      TQ(6,J)=TQ(6,J)-J7                                                
   53 IF(TQ(4,J).GT.LZ) LZ=TQ(4,J)                                      
    3 CONTINUE                                                          
      LZ=LZ+1                                                           
      IF(LZ-1.EQ.0) GOTO 602                                            
      DO 68 I=1,NT1                                                     
   68 PTQ(I)=0                                                          
      NSBOR=A(10,1)                                                     
      KSBOR=NSBOR+A(11,1)-1                                             
      DO 60 I=NSBOR,KSBOR                                               
      PTQ(A(12,S(1,I)))=1                                               
   60 CONTINUE                                                          
      MIN=32000                                                         
      MAX=0                                                             
      DO 61 I=2,NA                                                      
      NMK=A(12,I)                                                       
      KMK=NMK+A(13,I)-1                                                 
      IF(PTQ(NMK).EQ.1) GOTO 61                                         
      KR=0                                                              
      DO 62 J=NMK,KMK                                                   
      IF(KR.EQ.1) GOTO 63                                               
      KOD=TQ(2,J)-TQ(2,J)/100*100                                       
      IF(KOD.EQ.NSLES) KR=1                                             
      IF(KOD.EQ.NSLES) GOTO 63                                          
      IF(TQC(2,J).EQ.0) GOTO 62                                         
      IF(TQ(6,J).LT.MIN) MIN=TQ(6,J)                                    
      GOTO 62                                                           
   63 IF(TQC(2,J).EQ.0) GOTO 62                                         
      IF(TQ(4,J).GT.MAX) MAX=TQ(4,J)                                    
      GOTO 61                                                           
   62 CONTINUE                                                          
   61 CONTINUE                                                          
      IF(MIN.EQ.32000.OR.MIN.LT.0) GOTO 602                             
      IF(MAX.NE.0) GOTO 64                                              
      DO 91 I=NSBOR,KSBOR                                               
      NMK=A(12,S(1,I))                                                  
      KMK=NMK+A(13,S(1,I))-1                                            
      DO 92 J=NMK,KMK                                                   
      IF(TQC(2,J).EQ.0) GOTO 92                                         
      IF(TQ(4,J).GT.MAX) MAX=TQ(4,J)                                    
      GOTO 91                                                           
   92 CONTINUE                                                          
   91 CONTINUE                                                          
   64 RAZ=MIN-MAX                                                       
      IF(RAZ.LE.1) GOTO 602                                             
      RAZ=RAZ-1                                                         
      DO 65 I=2,NA                                                      
      NMK=A(12,I)                                                       
      KMK=NMK+A(13,I)-1                                                 
      IF(PTQ(NMK).EQ.1) GOTO 65                                         
      PTQ(NMK)=1                                                        
      DO 66 J=NMK,KMK                                                   
      KOD=TQ(2,J)-TQ(2,J)/100*100                                       
      IF(KOD.EQ.NSLES) GOTO 65                                          
      IF(TQC(2,J).EQ.0) GOTO 66                                         
      DO 67 I2=4,6                                                      
   67 TQ(I2,J)=TQ(I2,J)-RAZ                                             
   66 CONTINUE                                                          
   65 CONTINUE                                                          
  602 LZ=0                                                              
      DO 78 J=2,NT1                                                     
      IF(TQC(2,J).EQ.0) GOTO 78                                         
      IF(TQ(4,J).GT.LZ) LZ=TQ(4,J)                                      
   78 CONTINUE                                                          
      LZ=LZ+1                                                           
      IF(NF.EQ.5) GOTO 12                                               
      NPZ=NPZ+1                                                         
   12 DO 5 J1=1,NT1                                                     
      IF(TQC(2,J1).LE.0) GOTO 5                                         
      I2=LZ-TQ(5,J1)                                                    
      I1=LZ-TQ(4,J1)                                                    
      IF(I1.GT.LP) GOTO 5                                               
      IF(I2.GT.LP) I2=LP+1                                              
      I3=TQ(1,J1)                                                       
      I4=U(7,I3)                                                        
      IF(I4.LE.0) GO TO 5                                               
      I1=LZ-TQ(4,J1)                                                    
      IF(I1.GE.I2) GO TO 15                                             
      I=I2-1                                                            
      DO 6 J=I1,I                                                       
      IF(QZ(4,J1).LT.1000000.OR.IP.NE.0.OR.U(2,I3).NE.820) ZG(I4,J)=ZG(I4,J)+U(2,I3)                                                    
      IF(QZ(4,J1).GE.1000000.AND.IP.EQ.0.AND.U(2,I3).EQ.820) ZG(I4,J)=ZG(I4,J)+U(2,I3)*2                                                  
    6 CONTINUE                                                          
      IF(I2.EQ.LP+1) GOTO 5                                             
   15 IF(QZ(4,J1).LT.1000000.OR.IP.NE.0.OR.U(2,I3).NE.820) ZG(I4,I2)=ZG(I4,I2)+QZ(1,J1)-U(2,I3)*(I2-I1)                                  
      IF(QZ(4,J1).GE.1000000.AND.IP.EQ.0.AND.U(2,I3).EQ.820) ZG(I4,I2)=ZG(I4,I2)+QZ(1,J1)-U(2,I3)*2*(I2-I1)                              
    5 CONTINUE                                                          
      IU=0                                                              
      I1=0                                                              
      DO 7 I=1,NUC                                                      
      I1=I1+1                                                           
      I2=I1                                                             
      DO 8 J=1,LP                                                       
      IF(ZG(I,J).LE.0) GO TO 8                                          
      I1=I1+1                                                           
      IF(I1.GT.LMZP) STOP 111                                           
      ZP(1,I1)=J                                                        
      ZP(2,I1)=ZG(I,J)                                                  
    8 ZG(I,J)=0                                                         
      IF(I2.GE.I1) GO TO 9                                              
      ZP(1,I2)=I                                                        
      ZP(2,I2)=I1-I2                                                    
      IU=IU+1                                                           
      GO TO 7                                                           
    9 I1=I1-1                                                           
    7 CONTINUE                                                          
      NZ=NZ+1                                                           
      IF(NZ.GT.LIMZ) STOP 112                                           
      AZ(1,NZ)=K                                                        
      AZ(2,NZ)=LZ                                                       
      AZ(8,NZ)=CON                                                      
      MP1=MP-MP/10000*10000                                             
      KPP1=KPP-KPP/10000*10000                                          
      IF(MP1.LT.KPP1) J=0                                               
      IF(MP1.LT.KPP1) GOTO 17                                           
      CALL KALDNI(MP,0,NXZ)                                             
      DO 16 J=1,LP1                                 
      IF(KDP(J).EQ.NXZ(1,1)) GOTO 17                                    
   16 CONTINUE                                                          
   17 AZ(3,NZ)=J-LP                                              
      IF(IP.EQ.0) GOTO 80                                               
      IF(NF.EQ.6.AND.IPRZ.EQ.0) AZ(3,NZ)=1                              
      IF(NF.EQ.6.AND.IPRZ.EQ.1) AZ(3,NZ)=2                              
   80 AZ(4,NZ)=IP                                                       
      IF(PRG.EQ.1) AZ(4,NZ)=8                                           
      AZ(6,NZ)=IU                                                       
      IF(AZ(4,NZ).NE.0.AND.AZ(4,NZ).NE.8) GOTO 10                       
      AZ(5,NZ)=JO+1                                                     
      DO 11 J=1,I1                                                      
      JO=JO+1                                                           
      IF(JO.GT.LIMO) STOP 113                                           
      ZOP(1,JO)=ZP(1,J)                                                 
   11 ZOP(2,JO)=ZP(2,J)                                                 
      GO TO 2                                                           
   10 JD=JD+1                                                           
      AZ(5,NZ)=JD                                                       
      WRITE (ND,rec=JD) I1,((ZP(I,J),I=1,2),J=1,I1)                     
    2 CONTINUE                                                          
    4 REWIND NF1                                                        
      RETURN                                                            
      END                                                               
      SUBROUTINE PERPLN(NZ,NPZ,LP,LD,NUC,JO,ZG,UC,AZ,ZOP)               
      INTEGER*2 WPR(5000)                                               
      INTEGER*2 AZ(8,1),ZOP(2,1)                                        
      INTEGER ZG(100,1),UC(1)                                           
      REAL MINMAX,MAX                                                   
      CONTINUE                                                          
      CONTINUE                                                          
      CONTINUE                                                          
      CONTINUE                                                          
      CONTINUE                                                          
      MINMAX=10000.0                                                    
      DO 20 K=1,NZ                                                      
      WPR(K)=AZ(8,K)                                                    
   20 CONTINUE                                                          
      DO 10 I=1,NUC                                                     
      DO 10 J=1,LP                                                      
   10 ZG(I,J)=0                                                         
      NV=100000/NZ                                                      
      DO 8 KV=1,NV                                                      
      DO 1 K=1,NZ                                                       
      IF(AZ(4,K).NE.0.AND.AZ(4,K).NE.8) GO TO 1                         
      CALL genslpl(W)                                                   
      NW=AZ(2,K)                                                        
      IF(K.GT.NPZ)  NW=NW+LD                                            
      N=AZ(3,K)-NW                                                      
      IF(N.LT.0) N=0                                                    
      ID=INT(N*W)                                                       
      AZ(8,K)=ID+NW                                                     
      J2=AZ(5,K)-1                                                      
      J1=AZ(6,K)                                                        
      DO 2 I=1,J1                                                       
      J2=J2+1                                                           
      J=ZOP(1,J2)                                                       
      J3=ZOP(2,J2)                                                      
      DO 14 I1=1,J3                                                     
      J2=J2+1                                                           
      J4=ZOP(1,J2)+ID                                                   
      IF(J4.LE.0.OR.J4.GT.LP) GOTO 14                                   
      KZG=ZG(J,J4)+ZOP(2,J2)                                            
      ZG(J,J4)=ZG(J,J4)+ZOP(2,J2)                                       
   14 CONTINUE                                                          
    2 CONTINUE                                                          
    1 CONTINUE                                                          
      MAX=0.0                                                           
      DO 3 I=1,NUC                                                      
      IF(UC(I).EQ.0) R=0.0                                              
      IF(UC(I).EQ.0) PRINT 12,I                                         
      IF(UC(I).EQ.0) GOTO 30                                            
      R=1.0/UC(I)                                                       
   30 DO 3 J=1,LP                                                       
      D1=ZG(I,J)*R                                                      
      IF(D1.GT.MAX) MAX=D1                                              
    3 ZG(I,J)=0                                                         
      IF(MAX.GE.MINMAX) GOTO 8                                          
      MINMAX=MAX                                                        
      DO 5 I=1,NZ                                                       
    5 AZ(7,I)=AZ(8,I)                                                   
      IF(MINMAX.LE.1.00) GOTO 6                                         
    8 CONTINUE                                                          
    6 DO 4 K=1,NZ                                                       
      IF(AZ(4,K).GT.0) GO TO 4                                          
      J2=AZ(5,K)-1                                                      
      J1=AZ(6,K)                                                        
      DO 9 I=1,J1                                                       
      J2=J2+1                                                           
      J=ZOP(1,J2)                                                       
      J3=ZOP(2,J2)                                                      
      DO 15 L=1,J3                                                      
      J2=J2+1                                                           
      J4=ZOP(1,J2)+AZ(7,K)-AZ(2,K)                                      
   12 FORMAT (1X,8I8)                                                   
      IF(J4.LE.0.OR.J4.GT.LP) GOTO 15                                   
      KZG=ZG(J,J4)+ZOP(2,J2)                                            
      ZG(J,J4)=ZG(J,J4)+ZOP(2,J2)                                       
   15 CONTINUE                                                          
    9 CONTINUE                                                          
      AZ(8,K)=WPR(K)                                                    
    4 CONTINUE                                                          
         RETURN                                                         
      END                                                               
      SUBROUTINE OBPLAN(ND,LP,LD,NZ,NPZ,NUC,AZ,ZP,ZG,UC)                
      INTEGER*2 AZ(8,1),ZP(2,1)                                         
      INTEGER ZG(100,1),UC(1)                                           
      INTEGER*2 WPR(5000)                                               
   20 FORMAT(3I3)                                                       
      DO 10 J=1,NZ                                                      
      WPR(J)=AZ(8,J)                                                    
      AZ(8,J)=0                                                         
      IF(AZ(4,J).LE.0.OR.AZ(4,J).EQ.8) AZ(8,J)=10000                    
   10 CONTINUE                                                          
      DO 8 N=1,NZ                                                       
      I6=10000                                                          
      DO 9 I=1,NZ                                                       
      IF(AZ(8,I).GT.0) GOTO 9                                           
      K6=AZ(4,I)                                                        
      K2=WPR(I)                                                         
      K4=AZ(3,I)                                                        
      K3=0                                                              
      K1=AZ(2,I)                                                        
      IF(I.GT.NPZ) K3=1                                                 
      IF(I.GT.NPZ) K1=AZ(3,I)                                           
      IF(I.GT.NPZ) K4=AZ(2,I)                                           
      IF(K6-I6) 11,35,9                                                 
   35 IF(K3-I3) 11,12,9                                                 
  12  IF(K2-I2) 11,13,9                                                 
  13  IF(K4-I4) 11,14,9                                                 
  14  IF(K1-I1) 11,9,9                                                  
   11 I1=K1                                                             
      I2=K2                                                             
      I3=K3                                                             
      I4=K4                                                             
      I6=K6                                                             
      I0=I                                                              
    9 CONTINUE                                                          
      IF(I1.GT.9999) GOTO 15                                            
      I=I0                                                              
   24 CONTINUE                                                          
      AZ(8,I)=10000                                                     
      I2=AZ(5,I)                                                        
      READ (ND,rec=I2) J5,((ZP(I1,J),I1=1,2),J=1,J5)                    
      I1=AZ(6,I)                                                        
      J5=0                                                              
      IF(I.GT.NPZ) J5=LD                                                
    5 I4=0                                                              
      DO 2 J1=1,I1                                                      
      I4=I4+1                                                           
      J=ZP(1,I4)                                                        
      J2=ZP(2,I4)                                                       
      DO 3 J3=1,J2                                                      
      I4=I4+1                                                           
      J4=ZP(1,I4)+J5                                                    
      IF(J4.GT.LP) GO TO 3                                              
      IF(ZG(J,J4)+ZP(2,I4).LE.UC(J)) GO TO 3                            
      J5=J5+1                                                           
      GO TO 5                                                           
    3 CONTINUE                                                          
    2 CONTINUE                                                          
      AZ(7,I)=0                                                         
      IF(J5.GE.LP) GOTO 8                                               
      I4=0                                                              
      DO 6 J1=1,I1                                                      
      I4=I4+1                                                           
      J=ZP(1,I4)                                                        
      J2=ZP(2,I4)                                                       
      DO 7 J3=1,J2                                                      
      I4=I4+1                                                           
      J4=ZP(1,I4)+J5                                                    
      IF(J4.GT.LP) GO TO 7                                              
      ZG(J,J4)=ZG(J,J4)+ZP(2,I4)                                        
    7 CONTINUE                                                          
    6 CONTINUE                                                          
      AZ(7,I)=AZ(2,I)+J5                                                
      GOTO 8                                                            
   25 AZ(7,I)=0                                                         
      AZ(8,I)=10000                                                     
    8 CONTINUE                                                          
   15 CONTINUE                                                          
      RETURN                                                            
      END                                                               
      SUBROUTINE KALDNI(ND,LP,KDP)                                      
      INTEGER*2 KM(13)/0 ,31,28,31,30,31,30,31,31,30,31,30,31/,KDP(1  ),R/'R '/                                                  
      INTEGER*2 NDN(7)/'R ','R ','R ','R ','R ','N ','N '/              
      INTEGER*2 NDP(9)/ 101, 201, 803, 105, 205, 905, 710, 711, 811/    
      ID=ND/1000000                                                     
      M=ND/10000-ID*100                                                 
      IG=ND-ID*1000000-M*10000                                          
    1 FORMAT(20X,20HHEBEPHO «AƒAHA ƒATA:,I9)                            
      IF(ID*(13-M)*IG.GT.0) GO TO 14                                    
      GO TO 15                                                          
   14 CONTINUE                                                          
      IG1=IG/4                                                          
      IF(IG.EQ.IG1*4) KM(3)=29                                          
      M1=0                                                              
      DO 2 I=1,M                                                        
    2 M1=M1+KM(I)                                                       
      M2=(IG-1977)*365+(IG-1976)/4+M1+ID-2                              
      IF(IG.NE.IG1*4) GO TO 3                                           
      M2=M2-1                                                           
    3 IN=M2-(M2/7)*7                                                    
      IF(IN.EQ.0) IN=7                                                  
      LP1=LP*2                                                          
      IF(LP.NE.0) GO TO 12                                              
      LP1=1                                                             
      GO TO 13                                                          
   12 ID1=ID                                                            
      M1=M                                                              
      IN1=IN                                                            
      K=LP+2                                                            
   11 IF(NDN(IN1).NE.R) GO TO 8                                         
      DO 10 I=1,8                                                       
      M2=ID1*100+M1                                                     
      IF(M2.EQ.NDP(I)) GO TO 8                                          
   10 CONTINUE                                                          
      IF(K.EQ.LP+2.AND.ID.NE.ID1) K=K-1                                 
      K=K-1                                                             
      KDP(K)=ID1*1000+M1*10+IN1                                         
    8 ID1=ID1-1                                                         
      IF(ID1.GT.0) GO TO 9                                              
      M1=M1-1                                                           
      IF(M1.EQ.0) M1=12                                                 
      ID1=KM(M1+1)                                                      
    9 IN1=IN1-1                                                         
      IF(IN1.EQ.0) IN1=7                                                
      IF(K.GT.1) GO TO 11                                               
   13 K=LP                                                              
    7 IF(NDN(IN).NE.R) GO TO 4                                          
      DO 6 I=1,8                                                        
      M1=ID*100+M                                                       
      IF(M1.EQ.NDP(I)) GO TO 4                                          
    6 CONTINUE                                                          
      K=K+1                                                             
      KDP(K)=ID*1000+M*10+IN                                            
    4 ID=ID+1                                                           
      IF(ID.LE.KM(M+1)) GO TO 5                                         
      ID=1                                                              
      M=M+1                                                             
      IF(M.EQ.13) M=1                                                   
    5 IN=IN+1                                                           
      IF(IN.EQ.8) IN=1                                                  
      IF(K.LT.LP1) GO TO 7                                              
      KM(3)=28                                                          
   15 RETURN                                                            
      END                                                               
      SUBROUTINE PLTPGN(N1,N2,N3,N4,N5,NU,NUC,LP,NN,NK,NZ,NP,A,S,P,TQ,QZ,TQC,NXZ,IW,KDP,AZ,ZG,U,K5,K55)                                   
      INTEGER*2 A(26,200),S(2,200),P(2,200),TQ(6,500),TQC(4,500),NXZ(10,4),AZ(8,4000),ZG1(100,60),KDP(300)                               
      INTEGER*2 CD/'CO'/                                                
      INTEGER*2 KDPS(300),KLD(1)                                        
      INTEGER*2 PORT/' œ'/,U(20,1)                                      
      INTEGER*2 MET/'—œ'/                                               
      INTEGER*2 MET1/'  '/                                              
      INTEGER F(100,3)/300*0/,ZG(100,1)                                 
      INTEGER*4 QZ(4,500),IW(100)                                       
      INTEGER*2 PTQ(500),RAZ                                            
   150 FORMAT(//42X,'CBOÑKA O HAãàóàà áAKAáOB HA ',I8,41X//1X,127('-')/1X,'!ñ !KOãàó áAKAáOB !KOãàó BõèOãHEH!KOãàó áAKAáOB !KOãàó èEPEòEÑòàX  !KOãàóECTBO  áAKAáOB B èOPTîEãE !KOãàó áAKAáOB !OCTA-!KOãàó!'/1X,'!  !B èãAHE èPEÑ  !HõX áAKAáOB   !B èãAHE TEKì- !áAKAáOB àá èãAHA  !',31X,'!BKãûó B èãAH  !ãOCú !BõèOã!'/1X,'!E ! MECüñA       ! B èãAHE      !ôEÉO MECüñA   !èPEÑõÑìôEÉO MECüñA!',10X,'ñEXA',17X,'!àá èOPTîEãü   !B    !HEH- !'/1X,'!  !',110('-'),'!èOPT-!HõX B!'/1X,'!X ! áA-! áA-! Bõ-! áA-! áA-! Bõ-! áA-! áA-! Bõ-!KOã-!áAÑEã !BõèìCK!ÅEá !èPà-!CèEñ!C áAÉOTOBK !àTOÉ! áA-! áA-! Bõ-!îEãE !èOPT-!'/1X,'!  !èìCK!ÑEã !èìCK!èìCK!ÑEã !èìCK!èìCK!ÑEã !èìCK!BO  ! H/ó  ! H/ó  !áA- !OCTA!áA- !',11('-'),'!KOã-!èìCK!ÑEã !èìCK!ñEXA !îEãE !'/1X,'!  !',10('    !'),2('      !'),'ÉOT !HOB !KAáõ!KOã ! H/ó  !BO  !',3('    !'),'     !ñEXA !'/1X,127('-'))                       
  151 FORMAT(1H ,'!17!')                                                
  152 FORMAT(1H ,'!15!')                                                
  153 FORMAT(1H+,4X,10(I4,1X),I6,1X,I6,1X,4(I4,1X),I6,1X,4(I4,1X),2(I5,1X))                                                               
  625 FORMAT(1H+,7X,I5,10X,I5,10X,I5,61X,I5)                            
   53 FORMAT(10X,'B èOPTîEãE CTAãO',I5,' áAKAáOB'//)                    
  530 FORMAT(10X,'B èOPTîEãE ÅõãO',I5,' áAKAáOB')                       
 2000 FORMAT(10X,'B èãAHE ÅõãO',I5,' áAKAáOB')                          
 2001 FORMAT(10X,'B èãAHE CTAãO',I5,' áAKAáOB'/////)                    
  531 FORMAT(//////15X,'èPOTOKOã PACóETA èãAHA',1X,I8)                  
  200 FORMAT (10I8)                                                     
  300 FORMAT(1X,120(1H-))                                               
  301 FORMAT(1H )                                                       
      DATA K100/0/,K101/0/,K102/0/,K103/0/,K104/0/,K105/0/,K106/0/,K107/0/,K108/0/,K109/0/,K110/0/,K111/0/,K112/0/,K113/0/,K114/0/,K115/0/,K116/0/,K117/0/,K118/0/,K119/0/,K120/0/,K121/0/,K122/0/          
      DATA K500/0/,K501/0/,K502/0/,K503/0/,K504/0/,K505/0/,K506/0/,K507/0/,K508/0/,K509/0/,K510/0/,K511/0/,K512/0/,K513/0/,K514/0/,K515/0/,K516/0/,K517/0/,K518/0/,K519/0/,K520/0/,K521/0/,K522/0/          
      DATA KIT1/0/,KIT2/0/,KIT3/0/,KIT4/0/,KIT5/0/,KIT6/0/,KIT7/0/,KIT8/0/,KOLPLS/0/,KOLPLT/0/                                            
      DATA NSLES/83/                                                    
      ij=-1
      NQ=0                                                              
      KZI=0                                                             
      NI=0                                                              
      K=0                                                               
      MS=0                                                              
      LP1=LP+LP                                                         
      WRITE(N4) LP,NN,NK                                                
      WRITE(N4) (KDP(I),I=1,LP1)                                        
      NKPT=NK                                                           
      CALL KALDNI(NKPT,0,KLD)                                           
      DO 604 L=LP,LP1                                                   
      IF(NKPT.EQ.KDP(L)) GOTO 605                                       
  604 CONTINUE                                                          
  605 NKPT=L-LP                                                         
  800 FORMAT (I8)                                                       
      NF=N2                                                             
      IF(NP.LE.0) GO TO 100                                             
      NF=N1                                                             
      K=0                                                               
   60 READ(N1) J,I,I                                                    
      KOH=J                                                             
      LP2=J*2                                                           
      READ (N1)(KDPS(I),I=1,LP2)                                        
      NKPS=I                                                            
      CALL KALDNI(NKPS,0,KLD)                                           
      DO 601 I1=J,LP2                                                   
      IF(NKPS.EQ.KDPS(I1)) GOTO 603                                     
  601 CONTINUE                                                          
  603 NKPS=I1-J                                                         
    3 LU=J                                                              
  100 NJ=NZ+2                                                           
      MW=0                                                              
      DO 1 L=1,NJ                                                       
      M=L+MW                                                            
      IF(NF.EQ.N1) GO TO 15                                             
   14 READ(NF) NXZ,(IW(I),I=1,NU),IQ,IP,ID,LZ,MP,NA,NS,NT               
      KZI=KZI+1                                                         
      GO TO 18                                                          
   15 READ(NF) NXZ,(IW(I),I=1,NU),IQ,IP,ID,LD,LZ,LF,MP,MS,NA,NS,NT      
      KOLPLS=KOLPLS+1                                                   
   18 IF(NA.LE.-1000) GO TO 25                                          
      READ(NF) ((A(I,J),I=1,26),J=1,NA)                                 
      READ(NF) ((S(I,J),I=1,2),J=1,NS),((P(I,J),I=1,2),J=1,NS)          
       READ(NF) ((TQ(I,J),I=1,6),J=1,NT),((QZ(I,J),I=1,4),J=1,NT)       
      READ(NF) ((TQC(I,J),I=1,4),J=1,NT)                                
      K=K+1                                                             
      N=A(10,1)                                                         
      N=S(1,N)                                                          
      N=A(12,N)+A(13,N)-1                                               
      N=TQ(1,N)                                                         
      N=U(1,N)                                                          
      IF(NF.EQ.N1)  GOTO 154                                            
      IF(N.EQ.17) K117=K117+1                                           
      IF(N.EQ.15) K517=K517+1                                           
      IF(NXZ(5,1).EQ.MET.AND.N.EQ.17) K114=K114+1                       
      IF(NXZ(5,1).EQ.MET.AND.N.EQ.15) K514=K514+1                       
 9000 IF(IP.EQ.9.AND.N.EQ.17) K113=K113+1                               
      IF(IP.EQ.9.AND.N.EQ.15) K513=K513+1                               
      IF(IP.EQ.9.AND.IQ-QZ(2,1)-TQC(2,1).LE.0) GOTO 4                   
      IF(N.EQ.99.AND.IQ-QZ(2,1)-TQC(2,1).LE.0) GOTO 4                   
      IF(IP.EQ.9) GOTO 900                                              
      IF(N.EQ.99) GOTO 900                                              
      IF(N.EQ.15) GOTO 619                                              
      IF(A(9,NA).NE.99) GOTO 6222                                       
      IF(TQC(2,NT).LE.0) GOTO 6222                                      
      K112=K112+1                                                       
 6222 IF(TQ(2,2).NE.512.AND.TQ(2,2).NE.513) GOTO 620                    
      IF(TQC(2,2).LE.0) GOTO 620                                        
      K112=K112+1                                                       
      GOTO 155                                                          
  620 K115=K115+1                                                       
      DO 621 I=1,NT                                                     
      IF(TQC(2,I).LE.0) GOTO 621                                        
      K116=K116+QZ(1,I)                                                 
  621 CONTINUE                                                          
      GOTO 155                                                          
  619 IF(A(9,NA).NE.99) GOTO 622                                        
      IF(TQC(2,NT).LE.0) GOTO 622                                       
      K512=K512+1                                                       
      GOTO 155                                                          
  622 K515=K515+1                                                       
      DO 623 I=1,NT                                                     
      IF(TQC(2,I).LE.0) GOTO 623                                        
      K516=K516+QZ(1,I)                                                 
  623 CONTINUE                                                          
      GOTO 155                                                          
  154 IF(N.EQ.15) GOTO 600                                              
      KIT1=KIT1+1                                                       
      IF(LF.LE.NKPS) K102=K102+1                                        
      IF(LF.LE.NKPS) GOTO 155                                           
      IF(TQ(2,2).NE.512.AND.TQ(2,2).NE.513) GOTO 55555                  
      IF(TQC(2,2).LE.0.AND.TQC(1,2).LT.0) K101=K101+1                   
      IF(TQC(2,2).LE.0.AND.TQC(1,2).LT.0) GOTO 155                      
      IF(TQC(2,2).GT.0) K100=K100+1                                     
      IF(TQC(2,2).GT.0) GOTO 155                                        
      KSCP=0                                                            
      DO 650 JJ=3,NT                                                    
      IF(TQC(2,JJ).EQ.0.AND.TQC(1,JJ).LE.0) GOTO 650                    
      IF(TQ(4,JJ).LE.KOH) KSCP=1                                        
  650 CONTINUE                                                          
      IF(KSCP.EQ.0) K100=K100+1                                         
      IF(KSCP.EQ.1) K101=K101+1                                         
      GOTO 155                                                          
  600 KIT5=KIT5+1                                                       
      IF(LF.LE.NKPS) K502=K502+1                                        
      IF(LF.LE.NKPS) GOTO 155                                           
55555 IF(TQ(2,NT).NE.501) K101=K101+1                                   
      IF(TQ(2,NT).NE.501) GOTO 155                                      
      IF(TQC(2,NT).LE.0.AND.TQC(1,NT).LT.0) K101=K101+1                 
      IF(TQC(2,NT).LE.0.AND.TQC(1,NT).LT.0) GOTO 155                    
      IF(TQC(2,NT).GT.0) K100=K100+1                                    
      IF(TQC(2,NT).GT.0) GOTO 155                                       
      KSCP=0                                                            
      KTQP=NT-1                                                         
      N11=A(10,NA)                                                      
      IF(N11.EQ.0) IZIK=2                                               
      IF(N11.NE.0) IZIK=A(12,S(1,N11))+A(13,S(1,N11))                   
      DO 651 JJ=IZIK,KTQP                                               
      IF(TQC(2,JJ).EQ.0.AND.TQC(1,JJ).LE.0) GOTO 651                    
      IF(TQ(4,JJ).LE.KOH) KSCP=1                                        
  651 CONTINUE                                                          
      IF(KSCP.EQ.0) K100=K100+1                                         
      IF(KSCP.EQ.1) K101=K101+1                                         
  155 CONTINUE                                                          
      DO 720 KOL=1,2                                                    
      DO 49 J3=2,NA                                                     
      J2=A(12,J3)+A(13,J3)-1                                            
      J1=A(12,J3)                                                       
      NPOS=J2-1                                                         
      DO 54 J6=J1,J2                                                    
      KJ=J2-J6                                                          
      J4=J1+KJ                                                          
      IF(TQC(2,J4).NE.0.AND.J4.EQ.J2) GOTO 157                          
      IF(TQC(2,J4).NE.0) GOTO 54                                        
      IF(KJ.EQ.0) GOTO 54                                               
      GOTO 700                                                          
  157 NOMP=A(14,J3)                                                     
      KOLP=A(15,J3)                                                     
      KON=NOMP+KOLP-1                                                   
      IF(NA.EQ.3.AND.A(9,2).EQ.A(9,3)) GOTO 54                          
      DO 710 J10=NOMP,KON                                               
      NSB=P(1,J10)                                                      
      IF(NSB.EQ.1) GOTO 710                                             
      NSB=A(12,NSB)                                                     
      IF(TQC(2,NSB).NE.0) GOTO 710                                      
      KJ=KJ+1                                                           
      GOTO 700                                                          
  710 CONTINUE                                                          
      GOTO 54                                                           
  700 KJ=KJ+1                                                           
      DO 55 JK=1,KJ                                                     
      J5=J1+JK-1                                                        
      IF(TQC(2,J5).EQ.0) GOTO 55                                        
      TQC(2,J5)=0                                                       
      TQC(3,J5)=0                                                       
      TQC(1,J5)=KOH                                                     
   55 CONTINUE                                                          
      GOTO 49                                                           
   54 CONTINUE                                                          
   49 CONTINUE                                                          
  720 CONTINUE                                                          
      J3=A(10,1)                                                        
      J3=A(11,1)+J3-1                                                   
      J3=S(1,J3)                                                        
      J8=A(12,J3)+A(13,J3)-1                                            
      J1=A(12,J3)                                                       
      DO 1500 J6=J1,J8                                                  
      KJ=J8-J6                                                          
      J4=J1+KJ                                                          
      IF(TQ(2,J4)-TQ(2,J4)/100*100.NE.83) GOTO 1500                     
      R=(QZ(1,J4)+0.0)/U(2,TQ(1,J4))                                    
      J5=INT(R)                                                         
      IF(R-J5.LE.0.0) J5=J5-1                                           
      IF(NF.EQ.N2) J66=TQ(4,J4)-TQ(5,J4)                                
      IF(NF.EQ.N1) J66=TQ(5,J4)-TQ(4,J4)                                
      J7=J66-J5                                                         
      GOTO 1501                                                         
 1500 CONTINUE                                                          
      J7=0                                                              
 1501 NT1=NT                                                            
      NT2=0                                                             
      DO 1502 J=1,NT1                                                   
      NT2=NT2+1                                                         
      IF(NF.EQ.N2) GOTO 555                                             
      IF(J.EQ.1) GOTO 1520                                              
      IF(J.GT.J4.AND.J.LE.J8) GOTO 1520                                 
      TQ(4,NT2)=LF-TQ(4,J)-J7                                           
      IF(J.EQ.J4) GOTO 553                                              
      TQ(5,NT2)=LF-TQ(5,J)-J7                                           
      TQ(6,NT2)=LF-TQ(6,J)-J7                                           
      GOTO 1502                                                          
 1520 CONTINUE                                                          
      TQ(4,NT2)=LF-TQ(4,J)                                              
  553 TQ(5,NT2)=LF-TQ(5,J)                                              
      TQ(6,NT2)=LF-TQ(6,J)                                              
      IF(NF.EQ.N1) GOTO 1502                                             
  555 IF(J.EQ.1) GOTO 550                                               
      IF(J.GT.J4.AND.J.LE.J8) GOTO 550                                  
      TQ(4,NT2)=TQ(4,J)-J7                                              
      IF(J.EQ.J4) GOTO 533                                              
      TQ(5,NT2)=TQ(5,J)-J7                                              
      TQ(6,NT2)=TQ(6,J)-J7                                              
      GOTO 1502                                                         
  550 TQ(4,NT2)=TQ(4,J)                                                 
  533 TQ(5,NT2)=TQ(5,J)                                                 
      TQ(6,NT2)=TQ(6,J)                                                 
 1502 CONTINUE                                                          
      LZ=0                                                              
      DO 68 I=2,NT                                                      
      IF(TQC(2,I).EQ.0) GOTO 68                                         
      IF(TQ(4,I).GT.LZ) LZ=TQ(4,I)                                      
   68 PTQ(I)=0                                                          
      IF(LZ.EQ.0) LZ=LZ+1                                               
      IF(LZ.EQ.0) GOTO 888                                              
      NSBOR=A(10,1)                                                     
      KSBOR=NSBOR+A(11,1)-1                                             
      DO 70 I=NSBOR,KSBOR                                               
      PTQ(A(12,S(1,I)))=1                                               
   70 CONTINUE                                                          
      MIN=32000                                                         
      MAX=0                                                             
      DO 61 I=2,NA                                                      
      NMK=A(12,I)                                                       
      KMK=NMK+A(13,I)-1                                                 
      IF(PTQ(NMK).EQ.1) GOTO 61                                         
      KR=0                                                              
      DO 62 J=NMK,KMK                                                   
      IF(KR.EQ.1) GOTO 63                                               
      KOD=TQ(2,J)-TQ(2,J)/100*100                                       
      IF(KOD.EQ.NSLES) KR=1                                             
      IF(KOD.EQ.NSLES) GOTO 63                                          
      IF(TQC(2,J).EQ.0) GOTO 62                                         
      IF(TQ(6,J).LT.MIN) MIN=TQ(6,J)                                    
      GOTO 62                                                           
   63 IF(TQC(2,J).EQ.0) GOTO 62                                         
      IF(TQ(4,J).GT.MAX) MAX=TQ(4,J)                                    
      GOTO 61                                                           
   62 CONTINUE                                                          
   61 CONTINUE                                                          
      IF(MIN.EQ.32000.OR.MIN.LT.0) GOTO 7778                            
      IF(MAX.NE.0) GOTO 64                                              
      DO 91 I=NSBOR,KSBOR                                               
      NMK=A(12,S(1,I))                                                  
      KMK=NMK+A(13,S(1,I))-1                                            
      DO 92 J=NMK,KMK                                                   
      IF(TQC(2,J).EQ.0) GOTO 92                                         
      IF(TQ(4,J).GT.MAX) MAX=TQ(4,J)                                    
      GOTO 91                                                           
   92 CONTINUE                                                          
   91 CONTINUE                                                          
   64 RAZ=MIN-MAX                                                       
      IF(RAZ.LE.1) GOTO 7778                                            
      RAZ=RAZ-1                                                         
      DO 65 I=2,NA                                                      
      NMK=A(12,I)                                                       
      KMK=NMK+A(13,I)-1                                                 
      IF(PTQ(NMK).EQ.1) GOTO 65                                         
      PTQ(NMK)=1                                                        
      DO 66 J=NMK,KMK                                                   
      KOD=TQ(2,J)-TQ(2,J)/100*100                                       
      IF(KOD.EQ.NSLES) GOTO 65                                          
      IF(TQC(2,J).EQ.0) GOTO 66                                         
      DO 67 I2=4,6                                                      
   67 TQ(I2,J)=TQ(I2,J)-RAZ                                             
   66 CONTINUE                                                          
   65 CONTINUE                                                          
 7778 LZ=0                                                              
      DO 78 J=2,NT                                                      
      IF(TQC(2,J).EQ.0) GOTO 78                                          
      IF(TQ(4,J).GT.LZ) LZ=TQ(4,J)                                      
   78 CONTINUE                                                          
      LZ=LZ+1                                                           
  888 IF(NF.EQ.N2) GOTO 1000                                            
      IF(K.LT.AZ(1,L).OR.K.GT.AZ(1,NP)) GO TO 4                         
 1000 J1=A(10,1)                                                        
      J2=A(11,1)+J1-1                                                   
      IF(NXZ(5,1).EQ.CD.AND.NF.EQ.N1) GOTO 900                          
      IF(NXZ(5,1).EQ.MET) GOTO 19                                       
      IF(TQC(1,1).LT.0.AND.IQ.EQ.0) GOTO 19                             
      IF(TQC(1,1).LT.0) TQC(2,1)=0                                      
      IF((IQ-QZ(2,1)-TQC(2,1)).GT.0) GOTO 19                            
   90 CONTINUE                                                          
      IF(NF.EQ.N2) GOTO 4                                                
      GO TO 27                                                          
  190 DO 9 J3=J1,J2                                                     
      J=S(1,J3)                                                         
      J=A(12,J)+A(13,J)-1                                               
      IF (TQC(2,J).GT.0) GO TO 19                                       
    9 CONTINUE                                                          
      IF (NF.EQ.N2) GO TO 4                                             
      GO TO 27                                                          
  19  IF(NF.EQ.N2) GOTO 10                                               
      IF(AZ(7,L).LE.0) GOTO 13                                           
      LS=AZ(7,L)-LF                                                     
      DO 5 I=1,NT                                                       
      TQC(1,I)=TQC(1,I)-LU                                              
      LW=-LU                                                            
      IF(TQC(2,I).GT.0) LW=0                                            
      DO 7 J=4,6                                                        
    7 TQ(J,I)=AZ(7,L)-TQ(J,I)-LW                                        
    5 CONTINUE                                                          
      GO TO 6                                                           
   10 LF=AZ(7,M)                                                        
      IF(LF.LE.0) GOTO 13                                               
      DO 12 I=1,NT                                                      
      TQC(1,I)=TQC(1,I)-LU                                              
      LW=-LU                                                            
      IF(TQC(2,I).GT.0) LW=0                                            
      DO 12 J=4,6                                                       
   12 TQ(J,I)=LF-TQ(J,I)-LW                                             
      LD=LF-AZ(2,M)+LP+1                                                
      LD=KDP(LD)/10*100+((NN-NN/10000*10000)-(NN-NN/10000*10000)/100*100)                                                             
    6 LF=AZ(7,M)                                                        
      LZ=AZ(2,M)                                                        
      IF(MS.EQ.-1.AND.AZ(7,M).GT.LP) GOTO 1600                          
      MS=0                                                              
 1600 CONTINUE                                                          
      IF(NXZ(5,1).NE.MET) QZ(2,1)=QZ(2,1)+TQC(2,1)                      
      TQC(2,1)=0                                                        
      LZ=0                                                              
      IPPZ=0                                                            
      DO 133 J=1,NT                                                     
      IF(NXZ(5,1).EQ.MET) QZ(3,J)=0                                     
      IF(QZ(4,J).EQ.99999) QZ(4,J)=0                                    
      IF(TQC(2,J).EQ.0) GOTO 133                                        
      IF(TQ(2,J).EQ.512.OR.TQ(2,J).EQ.513) IPPZ=1                       
      IF(TQ(2,J).EQ.512.OR.TQ(2,J).EQ.513) GOTO 133                     
      IF(LF-TQ(4,J).GT.LZ) LZ=LF-TQ(4,J)                                
  133 CONTINUE                                                          
      IF(IPPZ.EQ.1) LZ=LZ+30                                            
      IF(LZ.EQ.0) LF=1                                                  
      WRITE(N4) NXZ,(IW(I),I=1,NU),IQ,IP,ID,LD,LZ,LF,MP,MS,NA,NS,NT     
      WRITE(N4) ((A(I,J),I=1,26),J=1,NA)                                
      WRITE (N4) ((S(I,J),I=1,2),J=1,NS),((P(I,J),I=1,2),J=1,NS)        
      WRITE (N4) ((TQ(I,J),I=1,6),J=1,NT),((QZ(I,J),I=1,4),J=1,NT)      
      WRITE (N4) ((TQC(I,J),I=1,4),J=1,NT)                              
      KOLPLT=KOLPLT+1                                                   
      IF(NF.EQ.N1) GOTO 609                                             
      IF(N.EQ.15) GOTO 606                                              
      KIT3=KIT3+1                                                       
      KIT4=KIT4+1                                                       
      IF(LF.LE.NKPT) K108=K108+1                                        
      IF(LF.LE.NKPT) K120=K120+1                                        
      IF(LF.LE.NKPT) GOTO 1                                             
      IF(TQ(2,2).NE.512.AND.TQ(2,2).NE.513) GOTO 55556                  
      IF(TQC(2,2).LE.0.AND.TQC(1,2).LT.0) K107=K107+1                   
      IF(TQC(2,2).LE.0.AND.TQC(1,2).LT.0) K119=K119+1                   
      IF(TQC(2,2).LE.0.AND.TQC(1,2).LT.0) GOTO 1                        
      IF(TQC(2,2).GT.0) K106=K106+1                                     
      IF(TQC(2,2).GT.0) K118=K118+1                                     
      IF(TQC(2,2).GT.0) GOTO 1                                          
      KSCP=0                                                            
      DO 652 JJ=3,NT                                                    
      IF(TQC(2,JJ).EQ.0.AND.TQC(1,JJ).LE.0) GOTO 652                    
      IF(TQ(4,JJ).LE.LP) KSCP=1                                         
  652 CONTINUE                                                          
      IF(KSCP.EQ.0) K106=K106+1                                         
      IF(KSCP.EQ.0) K118=K118+1                                         
      IF(KSCP.EQ.1) K107=K107+1                                         
      IF(KSCP.EQ.1) K119=K119+1                                         
      GOTO 1                                                            
  606 KIT7=KIT7+1                                                       
      KIT8=KIT8+1                                                       
      IF(LF.LE.NKPT) K508=K508+1                                        
      IF(LF.LE.NKPT) K520=K520+1                                        
      IF(LF.LE.NKPT) GOTO 1                                             
55556 IF(TQ(2,NT).NE.501) K107=K107+1                                   
      IF(TQ(2,NT).NE.501) K119=K119+1                                   
      IF(TQ(2,NT).NE.501) GOTO 1                                        
      IF(TQC(2,NT).LE.0.AND.TQC(1,NT).LT.0) K107=K107+1                 
      IF(TQC(2,NT).LE.0.AND.TQC(1,NT).LT.0) K119=K119+1                 
      IF(TQC(2,NT).LE.0.AND.TQC(1,NT).LT.0) GOTO 1                      
      IF(TQC(2,NT).GT.0) K106=K106+1                                    
      IF(TQC(2,NT).GT.0) K118=K118+1                                    
      IF(TQC(2,NT).GT.0) GOTO 1                                         
      KSCP=0                                                            
      KTQP=NT-1                                                         
      N11=A(10,NA)                                                      
      IF(N11.EQ.0) IZIK=2                                               
      IF(N11.NE.0) IZIK=A(12,S(1,N11))+A(13,S(1,N11))                   
      DO 653 JJ=IZIK,KTQP                                               
      IF(TQC(2,JJ).EQ.0.AND.TQC(1,JJ).LE.0) GOTO 653                    
      IF(TQ(4,JJ).LE.LP) KSCP=1                                         
  653 CONTINUE                                                          
      IF(KSCP.EQ.0) K106=K106+1                                         
      IF(KSCP.EQ.0) K118=K118+1                                         
      IF(KSCP.EQ.1) K107=K107+1                                         
      IF(KSCP.EQ.1) K119=K119+1                                         
      GOTO 1                                                            
  609 IF(N.EQ.15) GOTO 610                                              
      KIT3=KIT3+1                                                       
      K109=K109+1                                                       
      IF(LF.LE.NKPT) GOTO 611                                           
      IF(TQ(2,2).NE.512.AND.TQ(2,2).NE.513) GOTO 55557                  
      IF(TQC(2,2).LE.0.AND.TQC(1,2).LT.0) GOTO 654                      
      IF(TQC(2,2).GT.0) K106=K106+1                                     
      IF(TQC(2,2).GT.0) GOTO 1                                          
      KSCP=0                                                            
      DO 612 JJ=3,NT                                                    
      IF(TQC(2,JJ).EQ.0.AND.TQC(1,JJ).LE.0) GOTO 612                    
      IF(TQ(4,JJ).LE.LP) KSCP=1                                         
  612 CONTINUE                                                          
      IF(KSCP.EQ.0) K106=K106+1                                         
      IF(KSCP.EQ.0) GOTO 1                                              
      IF(KSCP.EQ.1) GOTO 654                                            
  611 K108=K108+1                                                       
      DO 613 I=1,NT                                                     
      IF(TQC(2,I).LE.0) GOTO 613                                        
      K111=K111+QZ(1,I)                                                 
  613 CONTINUE                                                          
      GOTO 1                                                            
  654 K107=K107+1                                                       
      DO 614 I=1,NT                                                     
      IF(TQC(2,I).LE.0) GOTO 614                                        
      K110=K110+QZ(1,I)                                                 
  614 CONTINUE                                                          
      GOTO 1                                                            
  610 KIT7=KIT7+1                                                       
      K509=K509+1                                                       
      IF(LF.LE.NKPT) GOTO 615                                           
55557 IF(TQ(2,NT).NE.501) GOTO 616                                      
      IF(TQC(2,NT).LE.0.AND.TQC(1,NT).LT.0) GOTO 616                    
      IF(TQC(2,NT).GT.0) K106=K106+1                                    
      IF(TQC(2,NT).GT.0) GOTO 1                                         
      KSCP=0                                                            
      KTQP=NT-1                                                         
      N11=A(10,NA)                                                      
      IF(N11.EQ.0) IZIK=2                                               
      IF(N11.NE.0) IZIK=A(12,S(1,N11))+A(13,S(1,N11))                   
      DO 655 JJ=IZIK,KTQP                                               
      IF(TQC(2,JJ).EQ.0.AND.TQC(1,JJ).LE.0) GOTO 655                    
      IF(TQ(4,JJ).LE.LP) KSCP=1                                         
  655 CONTINUE                                                          
      IF(KSCP.EQ.0) K106=K106+1                                         
      IF(KSCP.EQ.0) GOTO 1                                              
      IF(KSCP.EQ.1) GOTO 616                                            
  615 K508=K508+1                                                       
      DO 617 I=1,NT                                                     
      IF(TQC(2,I).LE.0) GOTO 617                                        
      K111=K111+QZ(1,I)                                                 
  617 CONTINUE                                                          
      GO TO 1                                                           
  616 K107=K107+1                                                       
      DO 618 I=1,NT                                                     
      IF(TQC(2,I).LE.0) GOTO 618                                        
      K110=K110+QZ(1,I)                                                 
  618 CONTINUE                                                          
      GOTO 1                                                            
   27 NI=NI+1                                                           
    4 IF(IJ.LT.0) GOTO 131                                              
  131 IF(NF.EQ.N1) GOTO 156                                             
      IF(N.EQ.17) K122=K122+1                                           
      IF(N.EQ.15) K522=K522+1                                           
      IF(IP.EQ.9.OR.N.EQ.99) GOTO 14                                    
      GOTO 103                                                          
  156 IF(N.EQ.15) GOTO 607                                              
      KIT2=KIT2+1                                                       
      IF(LF.LE.NKPS) K105=K105+1                                        
      IF(LF.LE.NKPS) GOTO 103                                           
      IF(TQ(2,2).NE.512.AND.TQ(2,2).NE.513) GOTO 55558                  
      IF(TQC(2,2).LE.0.AND.TQC(1,2).LT.0) K104=K104+1                   
      IF(TQC(2,2).LE.0.AND.TQC(1,2).LT.0) GOTO 103                      
      IF(TQC(2,2).GT.0) K103=K103+1                                     
      IF(TQC(2,2).GT.0) GOTO 103                                        
      KSCP=0                                                            
      DO 656 JJ=3,NT                                                    
      IF(TQC(2,JJ).EQ.0.AND.TQC(1,JJ).LE.0) GOTO 656                    
      IF(TQ(4,JJ).LE.KOH) KSCP=1                                        
  656 CONTINUE                                                          
      IF(KSCP.EQ.0) K103=K103+1                                         
      IF(KSCP.EQ.1) K104=K104+1                                         
      GOTO 103                                                          
  607 KIT6=KIT6+1                                                       
      IF(LF.LE.NKPS) K505=K505+1                                        
      IF(LF.LE.NKPS) GOTO 103                                           
55558 IF(TQ(2,NT).NE.501) K104=K104+1                                   
      IF(TQ(2,NT).NE.501) GOTO 103                                      
      IF(TQC(2,NT).LE.0.AND.TQC(1,NT).LT.0) K104=K104+1                 
      IF(TQC(2,NT).LE.0.AND.TQC(1,NT).LT.0) GOTO 103                    
      IF(TQC(2,NT).GT.0) K103=K103+1                                    
      IF(TQC(2,NT).GT.0) GOTO 103                                       
      KSCP=0                                                            
      KTQP=NT-1                                                         
      N11=A(10,NA)                                                      
      IF(N11.EQ.0) IZIK=2                                               
      IF(N11.NE.0) IZIK=A(12,S(1,N11))+A(13,S(1,N11))                   
      DO 657 JJ=IZIK,KTQP                                               
      IF(TQC(2,JJ).EQ.0.AND.TQC(1,JJ).LE.0) GOTO 657                    
      IF(TQ(4,JJ).LE.KOH) KSCP=1                                        
  657 CONTINUE                                                          
      IF(KSCP.EQ.0) K103=K103+1                                         
      IF(KSCP.EQ.1) K104=K104+1                                         
  103 IF(NF.EQ.N2) GOTO 102                                             
  101 IF(K-AZ(1,L)) 15,1,15                                             
  102 IF(K-AZ(1,M)) 14,1,14                                             
   13 IF(NXZ(5,1).NE.MET) QZ(2,1)=QZ(2,1)+TQC(2,1)                      
      TQC(2,1)=0                                                        
      DO 120 I=1,NT                                                     
      IF(TQC(2,I).EQ.0) TQC(1,I)=TQC(1,I)-LU                            
  120 CONTINUE                                                          
      WRITE(N3) NXZ,(IW(I),I=1,NU),IQ,IP,ID,LZ,MP,NA,NS,NT              
       WRITE (N3) ((A(I,J),I=1,26),J=1,NA)                              
      WRITE (N3) ((S(I,J),I=1,2),J=1,NS),((P(I,J),I=1,2),J=1,NS)        
      WRITE (N3) ((TQ(I,J),I=1,6),J=1,NT),((QZ(I,J),I=1,4),J=1,NT)      
      WRITE (N3) ((TQC(I,J),I=1,4),J=1,NT)                              
      NQ=NQ+1                                                           
      IF(N.EQ.17) K121=K121+1                                           
      IF(N.EQ.15) K521=K521+1                                           
      GOTO 1                                                            
  900 IF(NF.EQ.N1) GOTO 27                                              
      IF(NXZ(5,1).NE.MET) QZ(2,1)=QZ(2,1)+TQC(2,1)                      
      TQC(2,1)=0                                                        
      WRITE(N3) NXZ,(IW(I),I=1,NU),IQ,IP,ID,LZ,MP,NA,NS,NT              
      WRITE(N3) ((A(I,J),I=1,26),J=1,NA)                                
      WRITE(N3) ((S(I,J),I=1,2),J=1,NS),((P(I,J),I=1,2),J=1,NS)         
      WRITE(N3) ((TQ(I,J),I=1,6),J=1,NT),((QZ(I,J),I=1,4),J=1,NT)       
      WRITE(N3) ((TQC(I,J),I=1,4),J=1,NT)                               
      NQ=NQ+1                                                           
      IF(N.EQ.17) K121=K121+1                                           
      IF(N.EQ.15) K521=K521+1                                           
      GOTO 14                                                           
   25 REWIND NF                                                         
      IF(NF.EQ.N2) GO TO 16                                             
      NI=NP-NI                                                          
      K=0                                                               
      NF=N2                                                             
      MS=0                                                              
      MW=-1                                                             
    1 CONTINUE                                                          
   16 K=NZ-NP                                                           
      NZ1=K-NQ                                                          
      NA=-1000                                                          
      IF(IJ.LT.0) GOTO 132                                              
 132  DO 500 J=1,LP                                                     
      DO 500 I=1,NUC                                                    
  500 ZG1(I,J)=(ZG(I,J)+5)/10                                           
      WRITE(N4) NXZ,(IW(I),I=1,NU),IQ,IP,ID,LD,LZ,LF,MP,MS,NA,NS,NT     
      WRITE(N4) ((ZG1(I,J),I=1,NUC),J=1,LP),((F(I,J),I=1,NUC),J=1,3)    
      REWIND N4                                                         
      WRITE (N3) NXZ,(IW(I),I=1,NU),IQ,IP,ID,LZ,MP,NA,NS,NT             
       REWIND N3                                                        
      WRITE (*,531) NN                                                  
      KZI=KZI-1                                                         
      KOLPLS=KOLPLS-1                                                   
      open(12,file='prn')
      WRITE (*,530) KZI                                                 
      WRITE (*,53) NQ                                                   
      WRITE (*,2000) KOLPLS                                             
      WRITE (*,2001) KOLPLT                                             
      WRITE (*,300)                                                     
      pause'Á‡ÔË¯Ë ÍÓÎË˜ÂÒÚ‚Ó Á‡Í‡ÁÓ‚,‚ÍÎ˛˜Ë ÔÂ˜‡Ú¸ Ë Ì‡ÊÏË ÍÎ."ENTER"'
      DO 302 J=1,10                                                     
  302 WRITE (12,301)                                                    
      K110=K110/100                                                     
      K510=K510/100                                                     
      K111=K111/100                                                     
      K511=K511/100                                                     
      K116=K116/100                                                     
      K516=K516/100                                                     
      DO 305 I=1,1                                                      
      WRITE(12,150) NN                                                  
      WRITE(12,151)                                                     
      WRITE(12,153) K100,K101,K102,K103,K104,K105,K106,K107,K108,K109,K110,K111,K112,K113,K114,K115,K116,K117,K118,K119,K120,K121,K122    
      WRITE(12,151)                                                     
      WRITE(12,625) KIT1,KIT2,KIT3,KIT4                                 
      DO 303 J=1,10                                                     
  303 WRITE (12,301)                                                    
      WRITE (12,300)                                                    
  305 CONTINUE                                                          
      RETURN                                                            
      END                                                               
