*
* $Id: ifput.F,v 1.2 1996/03/07 17:38:15 couet Exp $
*
* $Log: ifput.F,v $
* Revision 1.2  1996/03/07 17:38:15  couet
* IFALCO selection was wrong
*
* Revision 1.1.1.1  1996/02/14 13:10:31  mclareni
* Higz
*
*
#include "higz/pilot.h"
*CMZ :  1.20/08 18/01/94  14.37.09  by  O.Couet
*-- Author :
      SUBROUTINE IFPUT(N)
#include "higz/hifc6.inc"
#if defined(CERNLIB_IBM)||defined(CERNLIB_VAX)
      LOGICAL*1 BOUT(256)
#endif
#if (!defined(CERNLIB_IBM))&&(!defined(CERNLIB_VAX))
      INTEGER BOUT(256)
#endif
      SAVE BOUT
      IPOUT=IPOUT+1
      BOUT(IPOUT)=N
#if defined(CERNLIB_IBM)
      IF (N.EQ.127) THEN
*        Ignore deletes
         BOUT(IPOUT)=27
         IPOUT=IPOUT+1
         BOUT(IPOUT)=63
      END IF
#endif
*  If Char is <= 0 then we output the line.
      IF (N.GT.0) RETURN
      BOUT(IPOUT)=31
*  If Char = 0 then switch to alpha mode, else just output.
      IF (N.EQ.-1) IPOUT=IPOUT-1
      IF (2.LT.IPOUT) CALL IFPUT1(BOUT,IPOUT)
      BOUT(1)=29
      IPOUT=1
      END
