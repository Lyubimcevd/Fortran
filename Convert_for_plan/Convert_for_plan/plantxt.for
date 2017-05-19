      INTEGER*2 A(26,200),S(2,200),P(2,200),TQ(6,500),TQC(4,500),NXZ(10,
     14),ZG(100,60),KDP(180),U(20,100)                                  
      INTEGER QZ(4,500),IW(200),F1(100,3),UW(2,200)  
      open(2,file='plan.txt')                                                        
      open(4,file='F:\asuip\asyip\u.dat',form='unformatted')
      READ(4) NW,NU                                                     
      READ(4) ((UW(I,J),I=1,2),J=1,NW),((U(I,J),I=1,20),J=1,NU) 
      NUC = 0
      DO 14 I=1,NU                                                      
      	IF(U(7,I).LE.0) GOTO 14                                           
      	NUC=NUC+1
   14 CONTINUE                                                          
      open(1,file='F:\asuip\asyip\plan.dat',form='unformatted')
      READ(1) LP,NN,NK1
      write(2,*) LP,NN,NK1                                                 
      LP1=LP+LP                                                        
      READ(1) (KDP(I),I=1,LP1)                                                         
      write(2,*) (KDP(I),I=1,LP1)
      kol = 0
  1   READ(1) NXZ,(IW(I),I=1,NU),IQ,IP,ID,LD,LZ,LF,MP,MS,NA,NS,NT       
      write(2,*) NXZ,(IW(I),I=1,NU),IQ,IP,ID,LD,LZ,LF,MP,MS,NA,NS,NT
      IF(NA.LT.0) GOTO 2                                               
      READ(1) ((A(I,J),I=1,26),J=1,NA)
      write(2,*) ((A(I,J),I=1,26),J=1,NA)                               
      READ(1) ((S(I,J),I=1,2),J=1,NS),((P(I,J),I=1,2),J=1,NS)           
      write(2,*) ((S(I,J),I=1,2),J=1,NS),((P(I,J),I=1,2),J=1,NS)
      READ(1) ((TQ(I,J),I=1,6),J=1,NT),((QZ(I,J),I=1,4),J=1,NT)         
      write(2,*) ((TQ(I,J),I=1,6),J=1,NT),((QZ(I,J),I=1,4),J=1,NT)
      READ(1) ((TQC(I,J),I=1,4),J=1,NT)	
      write(2,*) ((TQC(I,J),I=1,4),J=1,NT)
      KOL=KOL+1                                                         
      GOTO 1
  2   READ(1) ((ZG(I,J),I=1,NUC),J=1,LP),((F1(I,J),I=1,NUC),J=1,3)
      write(2,*) ((ZG(I,J),I=1,NUC),J=1,LP),((F1(I,J),I=1,NUC),J=1,3)
      end 