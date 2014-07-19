*
* $Id: pfkuip.F,v 1.1.1.1 1996/03/01 11:38:46 mclareni Exp $
*
* $Log: pfkuip.F,v $
* Revision 1.1.1.1  1996/03/01 11:38:46  mclareni
* Paw
*
*
#include "paw/pilot.h"
#if defined(CERNLIB_CZ)
*CMZ :  2.03/07 14/08/93  15.08.36  by  Fons Rademakers
*-- Author :    Alfred Nathaniel   07/04/93
      SUBROUTINE PFKUIP(CHCMD,ISTAT)
      CHARACTER*(*) CHCMD
*
* send a Kuip command to the Piaf server for execution
*
      CALL PFKUPN(CHCMD,0,ISTAT)
*
      END
#endif
