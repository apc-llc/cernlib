*
* $Id: cdcbcn.F,v 1.1.1.1 1996/02/28 16:23:47 mclareni Exp $
*
* $Log: cdcbcn.F,v $
* Revision 1.1.1.1  1996/02/28 16:23:47  mclareni
* Hepdb, cdlib, etc
*
*
#include "hepdb/pilot.h"
      SUBROUTINE CDCBCN (ITYP, NICKN, ITIME)
*     =======================================
*
************************************************************************
*                                                                      *
*        SUBR. CDCBCN (ITYP, NICKN, ITIME)                             *
*                                                                      *
*   Creates a table of front pannel cable connections                  *
*   as suggested by T. Hebecker                                        *
*                                                                      *
*   Arguments :                                                        *
*                                                                      *
*     ITYP     Topological module number                               *
*     NICKN    Nickname - unique name for each connection              *
*     ITIME    Start of time validity                                  *
*                                                                      *
*   Called by CDL1ET                                                   *
*                                                                      *
************************************************************************
*
#include "cstor5.inc"
#include "cuser5.inc"
      PARAMETER       (NOFF1=10, NOFF2=12, NOFFP=6)
      PARAMETER       (NKEX=NOFF2+8)
      DIMENSION       KEYX(NKEX), IOWDS(40)
      CHARACTER       CHOPT*8, COMNT*40
      DATA            IPROG/1/, INFIN/773964216/, IFRST/0/
*
*     ------------------------------------------------------------------
*
* *** --------  CABLE CONNECTIONS  ----------------------
*
      IFRST = IFRST + 1
      NDAT  = 11
      CHOPT = 'CDY'
      CALL MZIOCH (IOWDS, 40, '10H -F')
      CALL MZBOOK (IDIVCU, LUSECU, LUSECU, 2, 'MDPR', 0,0, NDAT,IOWDS,0)
*
*  ** Comment
*
      COMNT = 'CABLE CONNECTIONS FOR THAT MODULE '
      CALL UCTOH (COMNT, IQ(LUSECU+1), 4, 40)
*
*  ** Cable length
*
      Q(LUSECU+11) = 10.0*RNDM (Q)
*
      CALL VZERO (KEYX, NKEX)
      KEYX(NOFF1+1) = ITIME
      KEYX(NOFF1+2) = INFIN
      KEYX(NOFFP)   = IPROG
*
*  ** Generate Cable connections
*  ** Only the same types are concerned
*  ** # of output cable = # of input cable = ITYP
*  ** Position of the first cable = ITYP
*
      NTOPO = 100*ITYP + 3*ITYP - 2
      NTOPI = NTOPO + 1
      KEYX(NOFF2+1) = NTOPO
      KEYX(NOFF2+2) = ITYP
      KEYX(NOFF2+3) = ITYP*2 - 1
      KEYX(NOFF2+4) = NTOPI
      KEYX(NOFF2+5) = KEYX(NOFF2+2)
      KEYX(NOFF2+6) = KEYX(NOFF2+3)
*
*  ** Nickname and polarity
*
      KEYX(NOFF2+7) = NICKN
      CALL UCTOH ('P   ', KEYX(NOFF2+8), 4, 4)
*
      CALL CDSTOR ('//DBL3/TRIG/HARDCONF/CABCON', LUSECU, LKTRCU(4),
     +             IDIVCU, KEYX, CHOPT, IRC)
*
      IF (LUSECU.NE.0)  THEN
#if defined(CERNLIB__DEBUG)
        CALL DZSHOW ('CABCON  ', IDIVCU, LUSECU, 'B', 0, 0, 0, 0)
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
          CALL DZSHOW ('LKCABCON', IDIVCU, LKTRCU(4), 'BLV',0,0,0,0)
        ENDIF
        CALL MZDROP (IDIVCU, LKTRCU(4), 'L')
      ENDIF
*                                                             END CDCBCN
      END
