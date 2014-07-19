*
* $Id: arcopa.F,v 1.1.1.1 1996/01/11 14:05:16 mclareni Exp $
*
* $Log: arcopa.F,v $
* Revision 1.1.1.1  1996/01/11 14:05:16  mclareni
* Fritiof
*
*
C***********************************************************************
C $Id: arcopa.F,v 1.1.1.1 1996/01/11 14:05:16 mclareni Exp $
 
      SUBROUTINE ARCOPA(IJ,IP,ITYP)
 
C...ARiadne subroutine COpy PArton
 
C...Copies a parton from position IJ in /LUJETS/ common block to
C...Position IP in /ARPART/ common block.
 
 
      PARAMETER(MAXDIP=500,MAXPAR=500,MAXSTR=100)
 
      IMPLICIT DOUBLE PRECISION (D)
      IMPLICIT DOUBLE PRECISION (B)
      IMPLICIT LOGICAL (Q)
 
      COMMON /ARPART/ BP(MAXPAR,5),IFL(MAXPAR),IEX(MAXPAR),QQ(MAXPAR),
     $                IDI(MAXPAR),IDO(MAXPAR),INO(MAXPAR),IPART
      SAVE /ARPART/
 
      COMMON /ARJETX/ N,K(300,5),P(300,5),V(300,5)
      SAVE /ARJETX/
 
 
      DO 100 I=1,5
        BP(IP,I)=P(IJ,I)
 100  CONTINUE
 
      IFL(IP)=K(IJ,2)
      IEX(IP)=MOD(K(IJ,4),10)
      QQ(IP)=(ITYP.NE.2)
      INO(IP)=0
      RETURN
 
C**** END OF ARCOPA ****************************************************
      END
