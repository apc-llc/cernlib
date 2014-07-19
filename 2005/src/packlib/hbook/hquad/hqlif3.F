*
* $Id: hqlif3.F,v 1.1.1.1 1996/01/16 17:08:03 mclareni Exp $
*
* $Log: hqlif3.F,v $
* Revision 1.1.1.1  1996/01/16 17:08:03  mclareni
* First import
*
*
#include "hbook/pilot.h"
*CMZ :  4.17/02 12/09/92  16.41.49  by  John Allison
*-- Author :    John Allison   06/09/92
      SUBROUTINE HQLIF3
* Lifts LQ (LHQUAD - 3) chain - optional banks, i.e., only created if plenty
*   of space, namely twice as much as needed.
* See HQINIT for description of bank structure.
 
#include "hbook/hcqcom.inc"
#include "hbook/hcbook.inc"
 
      INTEGER IQUEST
      COMMON /QUEST/ IQUEST (100)
 
      INTEGER NWDJN, NAVAIL
      INTEGER NWW, NIO
#if defined(CERNLIB_DOUBLE)
      PARAMETER (NWW = 2, NIO = 4)
#endif
#if !defined(CERNLIB_DOUBLE)
      PARAMETER (NWW = 1, NIO = 3)
#endif
 
* Lift LHQDJN if pleny of space.
      NWDJN = NWW * NBTOT * NSIG
      NAVAIL = 2 * NWDJN + 10
      CALL MZNEED (IHDIV, NAVAIL, ' ')
      IF (IQUEST(11) .LT. 0) CALL MZNEED (IHDIV, NAVAIL, 'G')
      IF (IQUEST(11) .GE. 0) THEN
         CALL MZBOOK (IHDIV, LHQDJN, LHQUAD, -3, 'HQDJ', 0, 0, NWDJN,
     +   NIO, -1)
      END IF
 
      END
 
