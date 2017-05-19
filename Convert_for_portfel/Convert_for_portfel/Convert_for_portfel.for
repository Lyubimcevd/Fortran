      INTEGER*2 A(26,200),S(2,200),P(2,200),TQ(6,500),TQC(4,500),NXZ(10,
     14)                                                                
      INTEGER QZ(4,500),IW(200)
      open(44,file='F:\ASUIPW\u.dat',form='unformatted')
      open(1,file='F:\ASUIPW\portfel.dat',form='unformatte
     1d')
      open(4,file='portfel.txt')
      KOL=0                                                             
      READ (44) NW,NU                                                   
      REWIND 10                                                         
   1  READ(4,*) NXZ,(IW(I),I=1,NU),IQ,IP,ID,LZ,MP,NA,NS,NT
      write(1) NXZ,(IW(I),I=1,NU),IQ,IP,ID,LZ,MP,NA,NS,NT         
      IF(NA.LE.-1000) GOTO2                                             
      READ(4,*) ((A(I,J),I=1,26),J=1,NA)
      write(1) ((A(I,J),I=1,26),J=1,NA)                            
      READ(4,*) ((S(I,J),I=1,2),J=1,NS),((P(I,J),I=1,2),J=1,NS)
      write(1) ((S(I,J),I=1,2),J=1,NS),((P(I,J),I=1,2),J=1,NS)    
      READ(4,*) ((TQ(I,J),I=1,6),J=1,NT),((QZ(I,J),I=1,4),J=1,NT)
      write(1) ((TQ(I,J),I=1,6),J=1,NT),((QZ(I,J),I=1,4),J=1,NT)   
      READ(4,*) ((TQC(I,J),I=1,4),J=1,NT)
      write(1) ((TQC(I,J),I=1,4),J=1,NT)                          
      KOL=KOL+1                                                         
      GOTO1
   2  a = 0
      end