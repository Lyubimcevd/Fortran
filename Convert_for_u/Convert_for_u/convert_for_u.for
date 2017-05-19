      INTEGER*2 U(20,100),NO(100)                                      
      INTEGER UW(2,200)                    
      OPEN(1,FILE='F:\ASUIPW\U1.DAT',FORM='UNFORMATTED')
      OPEN(2,FILE='F:\ASUIPW\U2.DAT',FORM='UNFORMATTED')
      OPEN(3,FILE='F:\ASUIPW\U3.DAT',FORM='UNFORMATTED')
      open(4,file='u.txt')
      open(5,file='u.txt')
      open(6,file='u.txt')
      READ(4,*) NW,NU
      write(1) NW,NU                                                  
      READ(4,*) ((UW(I,J),I=1,2),J=1,NW),((U(I,J),I=1,20),J=1,NU)
      write(1) ((UW(I,J),I=1,2),J=1,NW),((U(I,J),I=1,20),J=1,NU)
      READ(5,*) NW,NU
      write(2) NW,NU                                                  
      READ(5,*) ((UW(I,J),I=1,2),J=1,NW),((U(I,J),I=1,20),J=1,NU)
      write(2) ((UW(I,J),I=1,2),J=1,NW),((U(I,J),I=1,20),J=1,NU)
      READ(6,*) NW,NU
      write(3) NW,NU                                                  
      READ(6,*) ((UW(I,J),I=1,2),J=1,NW),((U(I,J),I=1,20),J=1,NU)
      write(3) ((UW(I,J),I=1,2),J=1,NW),((U(I,J),I=1,20),J=1,NU)      
      END                                                               