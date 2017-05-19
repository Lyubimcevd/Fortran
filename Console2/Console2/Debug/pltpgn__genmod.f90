        !COMPILER-GENERATED INTERFACE MODULE: Tue Nov 08 15:41:58 2016
        MODULE PLTPGN__genmod
          INTERFACE 
            SUBROUTINE PLTPGN(N1,N2,N3,N4,N5,NU,NUC,LP,NN,NK,NZ,NP,A,S,P&
     &,TQ,QZ,TQC,NXZ,IW,KDP,AZ,ZG,U,K5,K55)
              INTEGER(KIND=4) :: N1
              INTEGER(KIND=4) :: N2
              INTEGER(KIND=4) :: N3
              INTEGER(KIND=4) :: N4
              INTEGER(KIND=4) :: N5
              INTEGER(KIND=4) :: NU
              INTEGER(KIND=4) :: NUC
              INTEGER(KIND=4) :: LP
              INTEGER(KIND=4) :: NN
              INTEGER(KIND=4) :: NK
              INTEGER(KIND=4) :: NZ
              INTEGER(KIND=4) :: NP
              INTEGER(KIND=2) :: A(26,200)
              INTEGER(KIND=2) :: S(2,200)
              INTEGER(KIND=2) :: P(2,200)
              INTEGER(KIND=2) :: TQ(6,500)
              INTEGER(KIND=4) :: QZ(4,500)
              INTEGER(KIND=2) :: TQC(4,500)
              INTEGER(KIND=2) :: NXZ(10,4)
              INTEGER(KIND=4) :: IW(100)
              INTEGER(KIND=2) :: KDP(300)
              INTEGER(KIND=2) :: AZ(8,4000)
              INTEGER(KIND=4) :: ZG(100,1)
              INTEGER(KIND=2) :: U(20,1)
              INTEGER(KIND=4) :: K5
              INTEGER(KIND=4) :: K55
            END SUBROUTINE PLTPGN
          END INTERFACE 
        END MODULE PLTPGN__genmod
