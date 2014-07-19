*
* $Id: cdclfl.F,v 1.1.1.1 1996/02/28 16:24:46 mclareni Exp $
*
* $Log: cdclfl.F,v $
* Revision 1.1.1.1  1996/02/28 16:24:46  mclareni
* Hepdb, cdlib, etc
*
*
#include "hepdb/pilot.h"
      SUBROUTINE CDCLFL (LUN)
*     =======================
*
************************************************************************
*                                                                      *
*        SUBR. CDCLFL (LUN)                                            *
*                                                                      *
*   Closes unit LUN                                                    *
*                                                                      *
*   Arguments :                                                        *
*                                                                      *
*     LUN      Logical unit number                                     *
*                                                                      *
*   Called by CDACPL, CDACTI, CDAIWR, CDAUXI, CDEDAS, CDEDKY, CDPEEK,  *
*             CDPRES, CDRDDA, CDRDIO                                   *
*                                                                      *
************************************************************************
*
#if defined(CERNLIB_IBM)
      CHARACTER       CTEMP*20, CHLUN*3
*
#endif
*     ------------------------------------------------------------------
*
      CLOSE (LUN)
#if defined(CERNLIB_IBM)
      WRITE (CHLUN, '(I3)') LUN
      CTEMP  = 'FILEDEF '//CHLUN//'  CLEAR'
      CALL VMCMS (CTEMP, ISTAT)
#endif
*                                                             END CDCLFL
  999 END
