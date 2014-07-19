*
* $Id: cdexpd.F,v 1.1.1.1 1996/02/28 16:24:14 mclareni Exp $
*
* $Log: cdexpd.F,v $
* Revision 1.1.1.1  1996/02/28 16:24:14  mclareni
* Hepdb, cdlib, etc
*
*
#include "hepdb/pilot.h"
      SUBROUTINE CDEXPD (MAXD, NDMX)
*     ==============================
*
************************************************************************
*                                                                      *
*        SUBR. CDEXPD (MAXD, MAXG)                                     *
*                                                                      *
*   Expands the system division by lifting a large bank & dropping it  *
*                                                                      *
*   Arguments :                                                        *
*                                                                      *
*     MAXD     To be expanded by MAXD words (if < 0, Expand by upto    *
*              4/5th of the total store space)                         *
*     NDMX*    Number of words by which system division was expanded   *
*              0 = no expansion                                        *
*                                                                      *
*   Called by user                                                     *
*                                                                      *
************************************************************************
*
#include "zebra/mzcb.inc"
#include "zebra/mzcc.inc"
      COMMON /QUEST/  IQUEST(100)
      PARAMETER       (NTRY=10)
*     ------------------------------------------------------------------
*
      CALL MZSDIV (0, -1)
      IDIVS  = IQTABV (KQT+9)
      LARGE  = 4*NQDMAX(KQT+1)/5
      IF (MAXD.GT.0) LARGE  = MIN (MAXD, LARGE)
      NDMX   = 0
      DO I = 1, NTRY
        NDTRY  = (I * LARGE) / NTRY
        CALL MZNEED (IDIVS, NDTRY, 'G')
        ND     = NDTRY + IQUEST(11) - 20
        IF (ND.GT.NDTRY) ND   = NDTRY
        IF (ND.GT.NDMX)  NDMX = ND
      ENDDO
      IF (NDMX.GT.0) THEN
        CALL MZBOOK (IDIVS, L, 0, 2, 'DUMY', 0, 0, NDMX, 2, -1)
        CALL MZDROP (IDIVS, L, ' ')
      ENDIF
*                                                             END CDEXPD
      END
