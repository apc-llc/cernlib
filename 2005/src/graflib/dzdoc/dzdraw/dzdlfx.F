*
* $Id: dzdlfx.F,v 1.1.1.1 1996/03/04 16:13:18 mclareni Exp $
*
* $Log: dzdlfx.F,v $
* Revision 1.1.1.1  1996/03/04 16:13:18  mclareni
* Dzdoc/Zebpack
*
*
#include "dzdoc/pilot.h"
      SUBROUTINE DZDLFX(LP,JB,CHID)
      CHARACTER*(*) CHID
#include "dzdoc/bkwrp.inc"
      CALL MZBOOK(0,L,LP,JB,CHID,0,0,3,2,0)
      IQ(KQSP+L+1) = -1
      IQ(KQSP+L+2) = -1
      IQ(KQSP+L+3) = -1
      END
***********************************************************************
