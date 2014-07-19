*
* $Id: fript.F,v 1.1.1.1 1996/01/11 14:05:21 mclareni Exp $
*
* $Log: fript.F,v $
* Revision 1.1.1.1  1996/01/11 14:05:21  mclareni
* Fritiof
*
*
C************************************************************************
 
      REAL FUNCTION FRIPT(I,N1,N2,IQ)
 
C....Evaluate the invariant P_T**2 of parton I as if I is put between
C....partons N1 and N2.
C....IQ=1: Definition 1: s12*s13/s123
C....  =2: Definition 2: s12*s13/(s123-s12-s13) (true P_T^2 in CMS of N1 N2)
C....  <0: use the N1 and N2 of previously memorized (N1,N2 dummy here).
 
      IMPLICIT DOUBLE PRECISION (D)
      PARAMETER (KSZJ=4000)
      COMMON/LUJETS/N,K(KSZJ,5),P(KSZJ,5),V(KSZJ,5)
 
      DIMENSION DM(2,5), DI(4), DS(3)
      SAVE DM
      SAVE /LUJETS/
 
      IF(IQ.GT.0) THEN
      DO 20 LO=1,5
      DM(1,LO) = DBLE( P(N1,LO))
20    DM(2,LO) = DBLE( P(N2,LO))
            IF(I.EQ.N1.OR.I.EQ.N2) THEN
      FRIPT = 0.
      RETURN
            ENDIF
      ENDIF
 
      DO 30 L=1,3
      DO 35 LO=1,4
      IF(L.LE.2) DI(LO) = DM(L,LO)+ DBLE( P(I,LO))
      IF(L.EQ.3) DI(LO) = DM(1,LO)+ DI(LO)
35    CONTINUE
30    DS(L) = DI(4)**2- DI(3)**2-DI(2)**2-DI(1)**2
 
      DS(1) = DS(1) - (DM(1,5)+DBLE(P(I,5)))**2
      DS(2) = DS(2) - (DM(2,5)+DBLE(P(I,5)))**2
      IF(IABS(IQ).EQ.2) DS(3) = DS(3) - DS(1) - DS(2)
      IF(DABS(DS(3)).LE.1.D-5) DS(3) = 1.D-5
 
      FRIPT = SNGL (DS(1)*DS(2)/DS(3) )
 
      RETURN
      END
