        !COMPILER-GENERATED INTERFACE MODULE: Tue Nov 08 15:01:22 2016
        MODULE EXTCON__genmod
          INTERFACE 
            SUBROUTINE EXTCON(NI,NA,NS,NT,NE,NU,A,S,P,TQ,QZ,TQC,U,IP,IW)
              INTEGER(KIND=4) :: NI
              INTEGER(KIND=4) :: NA
              INTEGER(KIND=4) :: NS
              INTEGER(KIND=4) :: NT
              INTEGER(KIND=4) :: NE
              INTEGER(KIND=4) :: NU
              INTEGER(KIND=2) :: A(26,1)
              INTEGER(KIND=2) :: S(2,1)
              INTEGER(KIND=2) :: P(2,1)
              INTEGER(KIND=2) :: TQ(6,1)
              INTEGER(KIND=4) :: QZ(4,1)
              INTEGER(KIND=2) :: TQC(4,1)
              INTEGER(KIND=2) :: U(20,1)
              INTEGER(KIND=4) :: IP
              INTEGER(KIND=4) :: IW(100)
            END SUBROUTINE EXTCON
          END INTERFACE 
        END MODULE EXTCON__genmod
