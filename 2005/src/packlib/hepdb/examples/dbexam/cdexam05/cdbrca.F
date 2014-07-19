*
* $Id: cdbrca.F,v 1.1.1.1 1996/02/28 16:23:47 mclareni Exp $
*
* $Log: cdbrca.F,v $
* Revision 1.1.1.1  1996/02/28 16:23:47  mclareni
* Hepdb, cdlib, etc
*
*
#include "hepdb/pilot.h"
      SUBROUTINE CDBRCA (ITIME)
*     =========================
*
************************************************************************
*                                                                      *
*        SUBR. CDBRCA (ITIME)                                          *
*                                                                      *
*   Creates an example of the Branch-Camac vs. Rack-Crate table        *
*   as suggested by T. Hebecker                                        *
*                                                                      *
*   Arguments :                                                        *
*                                                                      *
*     ITIME    Start of time validity                                  *
*                                                                      *
*   Called by CDL1ET                                                   *
*                                                                      *
************************************************************************
*
#include "cstor5.inc"
#include "cuser5.inc"
      PARAMETER       (NOFF1=10, NOFF2=12, NOFFP=6)
      PARAMETER       (NKEX=NOFF2+4)
      DIMENSION       KEYX(NKEX)
      CHARACTER       CHOPT*8, COMNT*40
      DATA            IPROG/1/, INFIN/773964216/, IFRST/0/
*
*     ------------------------------------------------------------------
*
* *** --------  BRANCH-CAMAC  ----------------------
*
      IFRST = IFRST + 1
      NDAT  = 10
      CHOPT = 'CDY'
      CALL MZBOOK (IDIVCU, LUSECU, LUSECU, 2, 'BRCA', 0, 0, NDAT, 5, 0)
*
*  ** Comment
*
      COMNT = 'BRANCH-CAMAC VS. RACK-CRATE '
      CALL UCTOH (COMNT, IQ(LUSECU+1), 4, 40)
*
      CALL VZERO (KEYX, NKEX)
      KEYX(NOFF1+1) = ITIME
      KEYX(NOFF1+2) = INFIN
      KEYX(NOFFP)   = IPROG
*
*  ** Loop on branch and camac
*
      DO 30 ICR = 1, 20
        DO 25 IBR = 1, 3
          KEYX(NOFF2+1) = IBR
          KEYX(NOFF2+2) = ICR
          KEYX(NOFF2+3) = (ICR-1)/4 + 1
          KEYX(NOFF2+4) = MOD (ICR, 4)
          IF (KEYX(NOFF2+4).EQ.0) KEYX(NOFF2+4) = 4
*
          CALL CDSTOR ('//DBL3/TRIG/HARDCONF/BRACAM', LUSECU, LKTRCU(3),
     +                 IDIVCU, KEYX, CHOPT, IRC)
*
#if defined(CERNLIB__DEBUG)
          IF (LUSECU.NE.0)  THEN
            CALL DZSHOW ('BRACAM  ', IDIVCU, LUSECU, 'B', 0, 0, 0, 0)
          ENDIF
#endif
          CALL UOPTC (CHOPT, 'C', IOPTC)
          IF (IOPTC.NE.0)  THEN
#if defined(CERNLIB__DEBUG)
            IF ((ICR.EQ.1.OR.ICR.EQ.20).AND.(IBR.EQ.1))  THEN
#endif
#if !defined(CERNLIB__DEBUG)
            IF ((ICR.EQ.1).AND.(IBR.EQ.1))  THEN
#endif
              CALL DZSHOW ('LKBRACAM', IDIVCU, LKTRCU(3), 'BLV',0,0,0,0)
            ENDIF
            CALL MZDROP (IDIVCU, LKTRCU(3), 'L')
          ENDIF
   25   CONTINUE
   30 CONTINUE
      CALL MZDROP (IDIVCU, LUSECU, ' ')
*                                                             END CDBRCA
      END
