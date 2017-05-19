      INTEGER*2 U(20,100),NO(100)                                      
      INTEGER UW(2,200)                    
      OPEN(4,FILE='F:\ASUIPW\tek_INF\U.DAT',FORM='UNFORMATTED')
      open(1,file='u.txt')
      READ(4) NW,NU
      write(1,*) NW,NU                                                  
      READ(4) ((UW(I,J),I=1,2),J=1,NW),((U(I,J),I=1,20),J=1,NU)
      write(1,*) ((UW(I,J),I=1,2),J=1,NW),((U(I,J),I=1,20),J=1,NU)      
      END                                                               