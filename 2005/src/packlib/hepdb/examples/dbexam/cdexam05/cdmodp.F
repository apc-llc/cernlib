*
* $Id: cdmodp.F,v 1.1.1.1 1996/02/28 16:23:47 mclareni Exp $
*
* $Log: cdmodp.F,v $
* Revision 1.1.1.1  1996/02/28 16:23:47  mclareni
* Hepdb, cdlib, etc
*
*
#include "hepdb/pilot.h"
      SUBROUTINE CDMODP (SEROL, SERNO, ITYP, ITIME)
*     =============================================
*
************************************************************************
*                                                                      *
*        SUBR. CDMODP (SEROL, SERNO, ITYP, ITIME)                      *
*                                                                      *
*   Creates an example of the Properties of the Modules                *
*   as suggested by T. Hebecker                                        *
*                                                                      *
*   Arguments :                                                        *
*                                                                      *
*     SEROL    Serial number of the old module                         *
*     SERNO    Serial number of the Module                             *
*     ITYP     Type of the module                                      *
*     ITIME    Start of time validity                                  *
*                                                                      *
*   Called by CDL1ET                                                   *
*                                                                      *
************************************************************************
*
#include "cstor5.inc"
#include "cuser5.inc"
      PARAMETER       (NOFF1=10, NOFF2=12, NOFFP=6)
      PARAMETER       (NKEX=NOFF2+7)
      DIMENSION       IOWDS(40), KEYX(NKEX), KEYO(NKEX)
      CHARACTER       CHOPT*8, COMNT*40
      CHARACTER       SERNO*8, STATU*4, SEROL*8
      LOGICAL         FERA
      DATA            IPROG/1/, INFIN/773964216/, IFRST/0/
*
*     ------------------------------------------------------------------
*
* *** --------  MODPRP  ----------------------
*
      IFRST = IFRST + 1
      CALL VZERO (KEYX, NKEX)
      KEYX(NOFF1+1) = ITIME
      KEYX(NOFF1+2) = INFIN
      KEYX(NOFFP)   = IPROG
*
*  *  Serial Number of Module
*
      CALL UCTOH (SERNO, KEYX(NOFF2+1), 4, 8)
*
*  *  Status
*
      STATU = 'Y   '
      CALL UCTOH (STATU, KEYX(NOFF2+3), 4, 4)
*
*  *  Integer keys
*
      FERA  = ITYP.EQ.5
      NCIN  = ITYP
      NCOUT = ITYP
      KEYX(NOFF2+4) = ITYP
      KEYX(NOFF2+5) = 0
      IF (FERA) KEYX(NOFF2+5) = 11
      KEYX(NOFF2+6) = 1000*NCIN + NCOUT
      KEYX(NOFF2+7) = ITYP*1000
      CALL UCOPY (KEYX, KEYO, NKEX)
      CALL UCTOH (SEROL, KEYO(NOFF2+1), 4, 8)
*
*  *  ADC - Information  (If FERA = 'F   ')
*
      IF (FERA) THEN
        NDAT  = 127
        CALL MZIOCH (IOWDS, 40, '10H 5F 16I 80F -H')
      ELSE
        NDAT  = 15
        CALL MZIOCH (IOWDS, 40, '10H -F')
      ENDIF
*
      CHOPT = 'CDY'
      CALL MZBOOK (IDIVCU, LUSECU, LUSECU, 2, 'MDPR', 0,0, NDAT,IOWDS,0)
      IF (FERA) THEN
*
*  **   ADC - Data
*
        LWU  = LUSECU + 15
        DO 30 I = 1, 16
*                        Channel #
          IQ(LWU+I) = I-1
*                        Pedestals
          Q (LWU+16+I) = 35. + 10.*RNDM(Q)
*                        R.M.S.
          Q (LWU+32+I) = RNDM(Q)
*                        Slopes
          Q (LWU+48+I) = 1. + 0.05*(RNDM(Q)-0.5)
*                        Intercepts
          Q (LWU+64+I) = 1.5*(RNDM(Q)-0.5)
*                        Chi-squares
          Q (LWU+80+I) = 2.*RNDM(Q)
*                        OK ?
          CALL UCTOH ('Y   ', IQ(LWU+96+I), 4, 4)
   30   CONTINUE
      ENDIF
*
*  ** Comment
*
      COMNT = 'MODULE PROPERTIES           '
      CALL UCTOH (COMNT, IQ(LUSECU+1), 4, 40)
*
*  ** Floating data
*
      Q(LUSECU+11) = 99. * RNDM(Q)
      Q(LUSECU+12) = 99. * RNDM(Q)
      Q(LUSECU+13) = 0.
      Q(LUSECU+14) = 0.
      Q(LUSECU+15) = 0.
*
      IF (SEROL.EQ.SERNO)  THEN
        CALL CDSTOR ('//DBL3/TRIG/MODPRP', LUSECU, LKTRCU(2),
     +               IDIVCU, KEYX, CHOPT, IRC)
      ELSE
        CALL CDREPL ('//DBL3/TRIG/MODPRP', LUSECU, LKTRCU(2),
     +               IDIVCU, KEYO, KEYX, CHOPT, IRC)
      ENDIF
      IF (LUSECU.NE.0)  THEN
#if defined(CERNLIB__DEBUG)
        CALL DZSHOW ('MODPRP  ', IDIVCU, LUSECU, 'B', 0, 0, 0, 0)
#endif
#if !defined(CERNLIB__DEBUG)
        IF (MOD(IFRST,50).EQ.1)
     +    CALL DZSHOW ('MODPRP  ', IDIVCU, LUSECU, 'B', 0, 0, 0, 0)
#endif
        CALL MZDROP (IDIVCU, LUSECU, ' ')
      ENDIF
*
      CALL UOPTC (CHOPT, 'C', IOPTC)
      IF (IOPTC.NE.0)  THEN
#if defined(CERNLIB__DEBUG)
        CALL DZSHOW ('LKMODPRP', IDIVCU, LKTRCU(2), 'BLV', 0, 0, 0, 0)
#endif
        CALL MZDROP (IDIVCU, LKTRCU(2), 'L')
      ENDIF
*                                                             END CDMODP
      END
