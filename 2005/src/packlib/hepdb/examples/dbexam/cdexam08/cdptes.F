*
* $Id: cdptes.F,v 1.1.1.1 1996/02/28 16:23:52 mclareni Exp $
*
* $Log: cdptes.F,v $
* Revision 1.1.1.1  1996/02/28 16:23:52  mclareni
* Hepdb, cdlib, etc
*
*
#include "hepdb/pilot.h"
      SUBROUTINE CDPTES
*
*     =================
*
************************************************************************
*                                                                      *
*        SUBR. CDPTES                                                  *
*                                                                      *
*   Tests the routine CDPLTI and CDPLOB on the fictitious data base    *
*                                                                      *
*   Called by CEXAM08                                                  *
*                                                                      *
************************************************************************
*
      PARAMETER       (NPAWC=20000)
      COMMON /PAWC/   HMEMOR(NPAWC)
      PARAMETER       (NOFF1=10, NOFF2=12, NOFFP=6, NKEYS=NOFF2+1)
      DIMENSION       KEYS(NKEYS), MASK(NKEYS), KOBJ(10)
      CHARACTER       CFNAM*20
*     ------------------------------------------------------------------
*
* *** Initialize HPLOT
*
      LUMET = 2
#if defined(CERNLIB_IBMVM)
      CFNAM = 'CEXAM08.META.A1'
#endif
#if !defined(CERNLIB_IBMVM)
      CFNAM = 'CEXAM08.META'
#endif
#if defined(CERNLIB_APOLLO)||defined(CERNLIB_UNIX)
      CALL CUTOL (CFNAM)
#endif
      CALL CDOPFL (LUMET, CFNAM, 'UNKNOWN', IOERR)
      CALL HLIMIT (-NPAWC)
      CALL HPLINT (0)
      CALL HPLCAP (-LUMET)
*
      CALL VZERO (MASK, NKEYS)
      CALL VZERO (KEYS, NKEYS)
      KEYS (NOFF2+1) = 1
      MASK (NOFF2+1) = 1
      CALL CDPLTI ('//DBL3/MUCH/MCALB/UVDS', MASK, KEYS, 'X', IRC)
      CALL IGSA (1)
      KOBJ (1) = 1
      KOBJ (2) = 3
      KEYBV    = NOFF1 + 1
      CALL CDPLOB ('//DBL3/MUCH/MCALB/UVDS', MASK, KEYS, 2, KOBJ, KEYBV,
     +             1, 'X', IRC)
      CALL IGSA (1)
*
      CALL HPLEND
*                                                             END CDPTES
      END
