*
* $Id: iplwid.F,v 1.2 2005/10/28 12:38:14 couet Exp $
*
* $Log: iplwid.F,v $
* Revision 1.2  2005/10/28 12:38:14  couet
* - SVG driver, workstation type -779
*
* Revision 1.1.1.1  1996/02/14 13:11:07  mclareni
* Higz
*
*
#include "higz/pilot.h"
*CMZ :  1.21/09 27/09/94  17.04.43  by  O.Couet
*-- Author : O.Couet
      SUBROUTINE IPLWID(IWID)
*.===========>
*.
*.  Set the line width
*.
*. _Input parameters:
*.
*. INTEGER IWID : Line width.
*.
*..==========> (O.Couet)
#include "higz/hipost.inc"
#include "higz/hiflag.inc"
*.______________________________________
*
      IF(VFLAG)THEN
         IF(IWID.LE.1)RETURN
         CALL IPPSTF(15,' stroke-width="')
         CALL IPKOUT(IWID/2)
         CALL IPPSTF(1,'"')
      ELSE
         IF(IWID.EQ.IPSLW)RETURN
         IPSLW=IWID
         CALL IPIOUT(IPSLW)
         CALL IPPSTF(3,' lw')
      ENDIF
*
      END
