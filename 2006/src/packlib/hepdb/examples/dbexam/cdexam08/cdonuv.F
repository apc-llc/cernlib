*
* $Id: cdonuv.F,v 1.1.1.1 1996/02/28 16:23:52 mclareni Exp $
*
* $Log: cdonuv.F,v $
* Revision 1.1.1.1  1996/02/28 16:23:52  mclareni
* Hepdb, cdlib, etc
*
*
#include "hepdb/pilot.h"
      SUBROUTINE CDONUV (ITIME)
*     =========================
*
************************************************************************
*                                                                      *
*        SUBR. CDONUV (ITIME)                                          *
*                                                                      *
*   Tests CDUSEM on the imitation of the Muon Data Base                *
*                                                                      *
*   Arguments :                                                        *
*                                                                      *
*     ITIME    Validity time                                           *
*                                                                      *
*   Called by CEXAM08                                                  *
*                                                                      *
************************************************************************
*
#include "cstor8.inc"
#include "cuser8.inc"
      PARAMETER       (NOFF1=10, NOFF2=12, NOFFP=6, NKEYS=NOFF2+1)
      DIMENSION       KEYS(100), MASK(100)
      CHARACTER       CHOPT*4
      DATA            NENT /0/
*
*     ------------------------------------------------------------------
*
      CALL VZERO (KEYS, NKEYS)
      CALL VZERO (MASK, NKEYS)
*
* *** -------------- UVDS ---------------------
*
      MASK(NOFF2+1) = 1
      KEYS(NOFF2+1) = 16
      IF (ITIME.EQ.0) THEN
        CALL CDBOOK ('//DBL3/MUCH/MCALB/UVDS', LKUVCU(1), KEYS(NOFF2+1),
     +               ' ', IRC)
        DO 5 I = 1, KEYS(NOFF2+1)
          IQ(LKUVCU(I)+NOFF2+1) = I
          IF (I.NE.KEYS(NOFF2+1)) LKUVCU(I+1) = LQ(LKUVCU(I))
    5   CONTINUE
        GO TO 999
      ENDIF
      NENT  = NENT + 1
      CHOPT = 'M'
      CALL CDUSEM ('//DBL3/MUCH/MCALB/UVDS', LKUVCU(1), ITIME,
     +             MASK, KEYS, CHOPT, IRC)
      DO 10 I = 1, KEYS(NOFF2+1)
        IF (LKUVCU(I).GT.0) THEN
          LDUVCU(I) = LQ(LKUVCU(I)-1)
        ELSE
          LDUVCU(I) = 0
        ENDIF
#if !defined(CERNLIB__DEBUG)
        IF (NENT.LE.50.AND.LDUVCU(I).NE.0.AND.I.EQ.KEYS(NOFF2+1))
#endif
#if defined(CERNLIB__DEBUG)
        IF (NENT.LE.50.AND.LDUVCU(I).NE.0)
#endif
     +    CALL DZSHOW ('UVDS    ', IDIVCU, LDUVCU(I), 'V', 0, 0, 0, 0)
   10 CONTINUE
      CALL CDFREE ('//DBL3/MUCH/MCALB/UVDS', LKUVCU, MASK, KEYS, CHOPT,
     +             IRC)
*                                                             END CDONUV
  999 END
