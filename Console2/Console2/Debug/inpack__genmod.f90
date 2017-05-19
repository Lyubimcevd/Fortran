        !COMPILER-GENERATED INTERFACE MODULE: Tue Nov 08 15:41:57 2016
        MODULE INPACK__genmod
          INTERFACE 
            SUBROUTINE INPACK(ND,NF1,NU,NUC,LP,NZ,NPZ,JO,JD,LIMZ,LIMO,  &
     &LMZP,AZ,ZOP,ZG,ZP,U,NXZ,IW,A,S,P,TQ,QZ,TQC,KDP,K5,K55,KPP,KPRP,IC,&
     &PIC,ZAKP)
              INTEGER(KIND=4) :: NU
              INTEGER(KIND=4) :: ND
              INTEGER(KIND=4) :: NF1
              INTEGER(KIND=4) :: NUC
              INTEGER(KIND=4) :: LP
              INTEGER(KIND=4) :: NZ
              INTEGER(KIND=4) :: NPZ
              INTEGER(KIND=4) :: JO
              INTEGER(KIND=4) :: JD
              INTEGER(KIND=4) :: LIMZ
              INTEGER(KIND=4) :: LIMO
              INTEGER(KIND=4) :: LMZP
              INTEGER(KIND=2) :: AZ(8,1)
              INTEGER(KIND=2) :: ZOP(2,1)
              INTEGER(KIND=4) :: ZG(100,1)
              INTEGER(KIND=2) :: ZP(2,1)
              INTEGER(KIND=2) :: U(20,1)
              INTEGER(KIND=2) :: NXZ(10,4)
              INTEGER(KIND=4) :: IW(NU)
              INTEGER(KIND=2) :: A(26,1)
              INTEGER(KIND=2) :: S(2,1)
              INTEGER(KIND=2) :: P(2,1)
              INTEGER(KIND=2) :: TQ(6,1)
              INTEGER(KIND=4) :: QZ(4,1)
              INTEGER(KIND=2) :: TQC(4,1)
              INTEGER(KIND=2) :: KDP(1)
              INTEGER(KIND=4) :: K5
              INTEGER(KIND=4) :: K55
              INTEGER(KIND=4) :: KPP
              INTEGER(KIND=4) :: KPRP
              INTEGER(KIND=2) :: IC(2,65)
              INTEGER(KIND=2) :: PIC(65)
              INTEGER(KIND=2) :: ZAKP(4,1)
            END SUBROUTINE INPACK
          END INTERFACE 
        END MODULE INPACK__genmod
