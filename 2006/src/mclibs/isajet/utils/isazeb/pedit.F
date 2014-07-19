*
* $Id: pedit.F,v 1.10 2001/10/08 14:03:17 mclareni Exp $
*
* $Log: pedit.F,v $
* Revision 1.10  2001/10/08 14:03:17  mclareni
* Version 7.58 from author
*
*
#include "sys/CERNLIB_machine.h"
#include "pilot.h"
      FUNCTION PEDIT(ID,P)
C======================================================================
C
C   Purpose and Methods :
C   select particles to be included in Zebra output
C   called for every particle. If PEDIT=.FALSE. particle will
C   be left out
C
C   Inputs  :
C   ID  = particle id
C   P(4)= particle momenta, px,py,pz and E
C
C   Outputs : NONE
C
C   Created  23-MAY-1987   Serban D. Protopopescu
C
C======================================================================
#if defined(CERNLIB_IMPNONE)
      IMPLICIT NONE
#endif
      LOGICAL PEDIT
      INTEGER ID
      REAL P(4)
C======================================================================
      PEDIT=.TRUE.
      IF(P(4).LE.0.0) PEDIT=.FALSE.
  999 RETURN
      END
