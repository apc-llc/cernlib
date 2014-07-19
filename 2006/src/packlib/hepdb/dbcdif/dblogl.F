*
* $Id: dblogl.F,v 1.1.1.1 1996/02/28 16:25:00 mclareni Exp $
*
* $Log: dblogl.F,v $
* Revision 1.1.1.1  1996/02/28 16:25:00  mclareni
* Hepdb, cdlib, etc
*
*
#include "hepdb/pilot.h"
      SUBROUTINE DBLOGL (LUNRZ, LOGLV)
*     ================================
*
************************************************************************
*                                                                      *
*        SUBR. DBLOGL (LUNRZ, LOGLV)                                   *
*                                                                      *
*   Set the level of debug information                                 *
*                                                                      *
*   Arguments :                                                        *
*                                                                      *
*     LUNRZ    RZ file unit                                            *
*     LOGLV    Level of debug output                                   *
*                                                                      *
*   Called by user,   DBACTI                                           *
*                                                                      *
************************************************************************
*
#include "hepdb/cdcblk.inc"
      CHARACTER       TOPN*16
*
*     ------------------------------------------------------------------
*
      IQUEST(1) = 0
      IDEBCD = LOGLV
      LFIXCD = LTOPCD
   10 IF (LFIXCD.EQ.0)  GO TO 999
      IF (LUNRZ.NE.IQ(KOFUCD+LFIXCD+MUPLUN)) THEN
        LFIXCD = LQ(KOFUCD+LFIXCD)
        GO TO 10
      ENDIF
      NCH    = IQ(KOFUCD+LFIXCD+MUPNCH)
      CALL UHTOC (IQ(KOFUCD+LFIXCD+MUPNAM), 4, TOPN, NCH)
      TOPN   = TOPN(1:NCH)
      CALL CDLOGL (TOPN, LOGLV, ' ', IRC)
      IQUEST(1) = IRC
*                                                             END DBLOGL
  999 END
