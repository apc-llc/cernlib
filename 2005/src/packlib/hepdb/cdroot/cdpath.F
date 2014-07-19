*
* $Id: cdpath.F,v 1.1.1.1 1996/02/28 16:24:31 mclareni Exp $
*
* $Log: cdpath.F,v $
* Revision 1.1.1.1  1996/02/28 16:24:31  mclareni
* Hepdb, cdlib, etc
*
*
#include "hepdb/pilot.h"
#if (defined(CERNLIB__P3CHILD))&&(defined(CERNLIB_IBM))
* Ignoring t=dummy
#endif
      SUBROUTINE CDPATH (PATHX, IK)
*     =============================
*
************************************************************************
*                                                                      *
*        SUBR. CDPATH (PATHX*, IK)                                     *
*                                                                      *
*   Get the subdirectory address of a partitioned data set             *
*   from the Subdirectory number                                       *
*                                                                      *
*   Arguments :                                                        *
*                                                                      *
*     PATHX(*) Character string specifying the subdirectory path name  *
*     IK       Subdirectory number                                     *
*                                                                      *
*   Called by various routines in the HEPDB package                    *
*                                                                      *
************************************************************************
*
      CHARACTER       PATHX*(*)
      DATA            I2 / 8/
      SAVE            I2
*
*     ------------------------------------------------------------------
*
#if !defined(CERNLIB_IBM)||!defined(CERNLIB__P3CHILD)
      WRITE (PATHX, '(I8)') IK
#endif
#if (defined(CERNLIB_IBM))&&(defined(CERNLIB__P3CHILD))
      CALL UTWRIT (PATHX, '(I8)', IK, 1)
#endif
      I3     = 10
      DO 10 I = 1, I2
        IF (IK.LT.I3) THEN
          I1     = I2 + 1 - I
          PATHX  = PATHX(I1:I2)
          GO TO 999
        ENDIF
        I3     = I3 * 10
   10 CONTINUE
*                                                             END CDPATH
  999 END
