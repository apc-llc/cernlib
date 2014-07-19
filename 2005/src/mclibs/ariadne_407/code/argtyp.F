*
* $Id: argtyp.F,v 1.1.1.1 1996/03/08 16:51:02 mclareni Exp $
*
* $Log: argtyp.F,v $
* Revision 1.1.1.1  1996/03/08 16:51:02  mclareni
* Ariadne
*
*
#include "ariadne/pilot.h"
C***********************************************************************
C $Id: argtyp.F,v 1.1.1.1 1996/03/08 16:51:02 mclareni Exp $
 
      SUBROUTINE ARGTYP(I,ITYP)
 
C...ARiadne subroutine Get TYpe of Particle
 
C...Determines the type of particle I according to ITYP=2: gluon,
C...ITYP=1: quark or anti-di-quark, ITYP=-1: anti-quark or di-quark,
C...ITYP=0: other.
 
      PARAMETER(MAXDIP=500,MAXPAR=500,MAXSTR=100)
 
      IMPLICIT DOUBLE PRECISION (D)
      IMPLICIT DOUBLE PRECISION (B)
      IMPLICIT LOGICAL (Q)
      COMMON /LUJETS/ N,K(4000,5),P(4000,5),V(4000,5)
      SAVE /LUJETS/
      COMMON /LUDAT2/ KCHG(500,3),PMAS(500,4),PARF(2000),VCKM(4,4)
      SAVE /LUDAT2/
 
 
      ITYP=KCHG(LUCOMP(K(I,2)),2)*ISIGN(1,K(I,2))
 
      RETURN
 
C**** END OF ARGTYP ****************************************************
      END
