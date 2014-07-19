*
* $Id: cdcruv.F,v 1.1.1.1 1996/02/28 16:23:51 mclareni Exp $
*
* $Log: cdcruv.F,v $
* Revision 1.1.1.1  1996/02/28 16:23:51  mclareni
* Hepdb, cdlib, etc
*
*
#include "hepdb/pilot.h"
      SUBROUTINE CDCRUV (ITIM1)
*     =========================
*
************************************************************************
*                                                                      *
*        SUBR. CDCRUV (ITIM1)                                          *
*                                                                      *
*   Creates the UVDS part of the Muon Chamber Data Base                *
*                                                                      *
*   Arguments :                                                        *
*                                                                      *
*     ITIM1    Start time of validity                                  *
*                                                                      *
*   Called by CEXAM07                                                  *
*                                                                      *
************************************************************************
*
#include "cstor7.inc"
#include "cuser7.inc"
      PARAMETER       (NOFF1=10, NOFF2=12, NOFFP=6, NKEYS=NOFF2+1)
      DIMENSION       KEYX(4), KEYS(NKEYS)
      DATA            IPROG /1/, KEYX /4*0/, NENT /0/
      DATA            XRANG / 1000.0/, XCENT / 500.0/
*
*     ------------------------------------------------------------------
*
      NENT  = NENT + 1
      CALL CDPKTS (991231, 235959, INFIN, IRC)
*
* *** --------  UVDS  ----------------------
*
      NKEYU = 16
      NDAT  = 48
      CALL VZERO (KEYS, NKEYS)
      KEYS(NOFF1+1) = ITIM1
      KEYS(NOFF1+2) = INFIN
      KEYS(NOFFP)   = IPROG
      DO 10 K = 1, NKEYU
        CALL MZBOOK (IDIVCU, LUSECU, LUSECU, 2, 'UVDS', 0, 0, NDAT, 3,0)
        KEYS(NOFF2+1) = K
        DO 5 I = 1, NDAT
          Q(LUSECU+I) = XRANG*RNDM(Q) - XCENT
    5   CONTINUE
        CALL CDSTOR ('//DBL3/MUCH/MCALB/UVDS', LUSECU, LKUVCU, IDIVCU,
     +               KEYS, 'DP', IRC)
#if !defined(CERNLIB__DEBUG)
        IF (NENT.LE.50 .AND. LUSECU.NE.0 .AND. K.EQ.NKEYU)
#endif
#if defined(CERNLIB__DEBUG)
        IF (NENT.LE.50 .AND. LUSECU.NE.0)
#endif
     +    CALL DZSHOW ('UVDS    ', IDIVCU, LUSECU, 'B', 0, 0, 0, 0)
        CALL MZDROP (IDIVCU, LUSECU, ' ')
   10 CONTINUE
*                                                             END CDCRUV
      END
