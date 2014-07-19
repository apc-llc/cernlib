*
* $Id: cdproc.F,v 1.1.1.1 1996/02/28 16:24:51 mclareni Exp $
*
* $Log: cdproc.F,v $
* Revision 1.1.1.1  1996/02/28 16:24:51  mclareni
* Hepdb, cdlib, etc
*
*
#include "hepdb/pilot.h"
      SUBROUTINE CDPROC (CPRO, CPAR, NCH)
*     ===================================
*
************************************************************************
*                                                                      *
*        SUBR. CDPROC (CPRO, CPAR*, NCH*)                              *
*                                                                      *
*   Reads a character variable from the screen                         *
*                                                                      *
*   Arguments :                                                        *
*                                                                      *
*     CPRO     Prompt to be printed on the screen                      *
*     CPAR     Character variable as supplied by user                  *
*     NCH      Length of the variable CPAR                             *
*                                                                      *
*   Called by CDAUXI                                                   *
*                                                                      *
************************************************************************
*
      PARAMETER       (LUNI=5, LUNO=6)
      CHARACTER       CPRO*(*), CPAR*(*), CHVAL*80
*
*     ------------------------------------------------------------------
*
*  ** Set default and write the prompt
*
      CPAR   = ' '
      NCH    = 0
      WRITE (LUNO, 1001) CPRO
*
*  ** Now read in the line
*
      READ (LUNI, 1002, ERR=999, END=999) CHVAL
      CPAR   = CHVAL
      NCH    = LENOCC (CPAR)
      IF (NCH.GT.0) CALL CLTOU (CPAR(:NCH))
*
#if defined(CERNLIB_IBM)
 1001 FORMAT (' ',A,':')
#endif
#if !defined(CERNLIB_IBM)
 1001 FORMAT (' ',A,': ',$)
#endif
 1002 FORMAT (A80)
*                                                             END CDPROC
  999 END
