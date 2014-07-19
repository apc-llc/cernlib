*
* $Id: cspivec.F,v 1.2 1996/04/02 22:12:46 thakulin Exp $
*
* $Log: cspivec.F,v $
* Revision 1.2  1996/04/02 22:12:46  thakulin
* Remove CERNLIB_PIAF macro and check the run time variable IPIAF in
* paw/pcmode.inc instead.
*
* Revision 1.1.1.1  1996/02/26 17:16:32  mclareni
* Comis
*
*
#include "comis/pilot.h"
#if defined(CERNLIB_PAW)
*CMZ :  1.18/02 14/04/94  11.44.14  by  Vladimir Berezhnoi
*-- Author :    Vladimir Berezhnoi   08/04/94
      SUBROUTINE CSPIVEC(LINE)
      CHARACTER *(*)     LINE
      CHARACTER *32 NAME
      INTEGER CSKUIX
      L=LENOCC(LINE)
      IPOS=1
 1    CALL CSNIDN(LINE(:L),IPOS,NAME,LNAME )
      IF(LNAME.LE.0)RETURN
      CALL KUVECT(NAME,LLOW,LHIGH)
      II=CSKUIX(NAME,LLOW)
      GO TO 1
      END
#endif
