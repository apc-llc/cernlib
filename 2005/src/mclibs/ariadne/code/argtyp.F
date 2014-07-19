C***********************************************************************
C $Id: argtyp.F,v 1.2 1996/04/10 12:33:18 mclareni Exp $

      SUBROUTINE ARGTYP(I,ITYP)

C...ARiadne subroutine Get TYpe of Particle

C...Determines the type of particle I according to ITYP=2: gluon,
C...ITYP=1: quark or anti-di-quark, ITYP=-1: anti-quark or di-quark,
C...ITYP=0: other.

#include "arimpl.f"
#include "lujets.f"
#include "ludat2.f"


      ITYP=KCHG(LUCOMP(K(I,2)),2)*ISIGN(1,K(I,2))

      RETURN

C**** END OF ARGTYP ****************************************************
      END
