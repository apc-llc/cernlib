*
* $Id: 0pamain.F,v 1.3 1999/05/20 07:37:28 couet Exp $
*
* $Log: 0pamain.F,v $
* Revision 1.3  1999/05/20 07:37:28  couet
* - The flag CERNLIB_TKTCL is now removed. It was not used and just made
*   the code confusing.
*
* Revision 1.2  1996/05/09 14:49:53  cernlib
* Remove czdummy and make it seperate file
*
* Revision 1.1.1.1  1996/03/01 11:38:50  mclareni
* Paw
*
*
#include "paw/pilot.h"
*CMZ :  2.07/15 16/10/95  16.04.07  by  O.Couet
*-- Author :    Rene Brun   03/01/89
      PROGRAM PAMAIN
*
*        MAIN Program for basic PAW
*
#include "paw/pawsiz.inc"
*
      COMMON/PAWC/PAWCOM(NWPAW)
#if defined(CERNLIB_IBM)
      CALL INITC
      CALL ERRSET(151,999,-1)
#endif
#if defined(CERNLIB_HPUX)
      ON REAL UNDERFLOW IGNORE
#endif
*
      CALL PAW(NWPAW,IWTYP)
*
      CALL KUWHAG
*
      CALL PAEXIT
*
      STOP
      END
      SUBROUTINE QNEXT
      END
#if defined(CERNLIB_VAX)||defined(CERNLIB_QMALPH)
      SUBROUTINE CTL$GL_PCB
      ENTRY CTL$T_ACCOUNT
      ENTRY CTL$T_USERNAME
      END
#endif
