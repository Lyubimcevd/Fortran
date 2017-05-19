        !COMPILER-GENERATED INTERFACE MODULE: Tue Nov 08 15:01:22 2016
        MODULE CHKPRT__genmod
          INTERFACE 
            SUBROUTINE CHKPRT(LIMT,NA,NT,A,S,P,TQ,QZ,TQC,UG,NUG,IP)
              INTEGER(KIND=4) :: LIMT
              INTEGER(KIND=4) :: NA
              INTEGER(KIND=4) :: NT
              INTEGER(KIND=2) :: A(26,1)
              INTEGER(KIND=2) :: S(2,1)
              INTEGER(KIND=2) :: P(2,1)
              INTEGER(KIND=2) :: TQ(6,1)
              INTEGER(KIND=4) :: QZ(4,1)
              INTEGER(KIND=2) :: TQC(4,1)
              INTEGER(KIND=2) :: UG(20,1)
              INTEGER(KIND=4) :: NUG
              INTEGER(KIND=4) :: IP
            END SUBROUTINE CHKPRT
          END INTERFACE 
        END MODULE CHKPRT__genmod
