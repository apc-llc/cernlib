*
* $Id: cdmatt.F,v 1.1.1.1 1996/02/28 16:23:47 mclareni Exp $
*
* $Log: cdmatt.F,v $
* Revision 1.1.1.1  1996/02/28 16:23:47  mclareni
* Hepdb, cdlib, etc
*
*
#include "hepdb/pilot.h"
      SUBROUTINE CDMATT (NTOP, ITIME)
*     ===============================
*
************************************************************************
*                                                                      *
*        SUBR. CDMATT (NTOP, ITIME)                                    *
*                                                                      *
*   Creates table of Module - attributes (prescribed module properties)*
*   as suggested by T. Hebecker                                        *
*                                                                      *
*   Arguments :                                                        *
*                                                                      *
*     NTOP     Topological Module number                               *
*     ITIME    Start of time validity                                  *
*                                                                      *
*   Called by CDL1ET                                                   *
*                                                                      *
************************************************************************
*
#include "cstor5.inc"
#include "cuser5.inc"
      PARAMETER       (NOFF1=10, NOFF2=12, NOFFP=6)
      PARAMETER       (NKEX=NOFF2+10)
      DIMENSION       KEYX(NKEX)
      CHARACTER       CHOPT*8, COMNT*40
      DATA            IPROG/1/, INFIN/773964216/, IFRST/0/
*
*     ------------------------------------------------------------------
*
* *** --------  MODULE ATTRIBUTES  ----------------------
*
      IFRST = IFRST + 1
      NDAT  = 10
      CHOPT = 'CDY'
      CALL MZBOOK (IDIVCU, LUSECU, LUSECU, 2, 'MATT', 0, 0, NDAT, 5, 0)
*
*  ** Comment
*
      COMNT = 'PRESCRIBED PROPERTIES OF THAT MODULE '
      CALL UCTOH (COMNT, IQ(LUSECU+1), 4, 40)
*
      CALL VZERO (KEYX, NKEX)
      KEYX(NOFF1+1)  = ITIME
      KEYX(NOFF1+2)  = INFIN
      KEYX(NOFFP)    = IPROG
*
      KEYX(NOFF2+1)  = NTOP
      ITYP  = NTOP/100
      KEYX(NOFF2+2)  = ITYP
*
*  ** Generate Branch, Camac and Station number
*
      KEYX(NOFF2+3)  = MOD (NTOP, 3)
      IF (KEYX(NOFF2+3).EQ.0) KEYX(NOFF2+3) = 3
      KEYX(NOFF2+4)  = NTOP/100 + 1
      KEYX(NOFF2+5)  = 21.0*RNDM(Q) - 1.0
*
*  ** Generate Relative Timing and Switches
*
      KEYX(NOFF2+6)  = 0
      KEYX(NOFF2+7)  = 0
      IF (ITYP.EQ.5) KEYX(NOFF2+7) = 11
*
*  ** Generate In/Out Cables and Resistors
*
      NCIN  = ITYP
      NCOUT = ITYP
      KEYX(NOFF2+8)  = 1000*NCIN + NCOUT
      KEYX(NOFF2+9)  = KEYX(NOFF2+8)
      KEYX(NOFF2+10) = ITYP*1000
*
      CALL CDSTOR ('//DBL3/TRIG/HARDCONF/MODATT', LUSECU, LKTRCU(5),
     +             IDIVCU, KEYX, CHOPT, IRC)
      IF (LUSECU.NE.0)  THEN
#if defined(CERNLIB__DEBUG)
        CALL DZSHOW ('MODATT  ', IDIVCU, LUSECU, 'B', 0, 0, 0, 0)
#endif
        CALL MZDROP (IDIVCU, LUSECU, ' ')
      ENDIF
*
      CALL UOPTC (CHOPT, 'C', IOPTC)
      IF (IOPTC.NE.0)  THEN
#if defined(CERNLIB__DEBUG)
        IF (IFRST.LE.20)  THEN
#endif
#if !defined(CERNLIB__DEBUG)
        IF (IFRST.LE.5)  THEN
#endif
          CALL DZSHOW ('LKMODATT', IDIVCU, LKTRCU(5), 'BLV', 0, 0, 0, 0)
        ENDIF
        CALL MZDROP (IDIVCU, LKTRCU(5), 'L')
      ENDIF
*                                                             END CDMATT
      END
