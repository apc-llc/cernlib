*
* $Id: cdl1et.F,v 1.1.1.1 1996/02/28 16:23:47 mclareni Exp $
*
* $Log: cdl1et.F,v $
* Revision 1.1.1.1  1996/02/28 16:23:47  mclareni
* Hepdb, cdlib, etc
*
*
#include "hepdb/pilot.h"
      SUBROUTINE CDL1ET
*     =================
*
************************************************************************
*                                                                      *
*        SUBR. CDL1ET                                                  *
*                                                                      *
*   Creates an example of the Level 1 Energy Trigger as suggested by   *
*   T. Hebecker                                                        *
*                                                                      *
*   Called by CEXAM05                                                  *
*                                                                      *
************************************************************************
*
#include "cstor5.inc"
#include "cuser5.inc"
      PARAMETER       (NOFF1=10, NOFF2=12, NOFFP=6)
      DIMENSION       KEYX(50), KEYO(50)
      CHARACTER       CHOPT*8, COMNT*40
      CHARACTER       MODU1*4, MODU2*4, MODU5(5)*1, SERNO*8, SEROL*8
      DATA            IPROG /1/, INFIN /773964216/
      DATA            MODU1 /'XMOD'/, MODU5 /'A','B','C','D','E'/
*
*     ------------------------------------------------------------------
*
* *** --------  TOPSER  ----------------------
*
      NKEX  = NOFF2 + 3
      CALL CDCDL1
*
*  ** 10 data words reserved for comments (40 characters)
*
      NDAT  = 10
*
*  ** Set-up TOPSER at time IT1  and date ID1
*
      ID1   = 870731
      IT1   = 103000
      CALL CDPKTS (ID1, IT1, IDT1, IRC)
*
*  ** Set up Branch-Camac vs. Rack-crate table
*
      CALL CDBRCA (IDT1)
*
      DO 30 I = 1, 50
        CALL VZERO (KEYX, NKEX)
        KEYX(NOFF1+1) = IDT1
        KEYX(NOFF1+2) = INFIN
        KEYX(NOFFP)   = IPROG
*
*  *    Topological Module
*
        ITYP  = I/3 + 1
        KEYX(NOFF2+1) = 100*ITYP + I
*
*  *    Set up Module Attributes (i.e. prescribed properties)
*
        CALL CDMATT (KEYX(NOFF2+1), IDT1)
*
*  *    Serial Number of Module
*
        J     = (I-1)/10 + 1
        MODU1(1:1) = MODU5(J)
        CALL UCTOH (MODU1, KEYX(NOFF2+2), 4, 4)
        IMODU = 10*I
        WRITE (MODU2, 1001) IMODU
        CALL UCTOH (MODU2, KEYX(NOFF2+3), 4, 4)
*
*  **   Store Module Properties
*
        SERNO = MODU1//MODU2
        SEROL = SERNO
        CALL CDMODP (SEROL, SERNO, ITYP, IDT1)
        CHOPT = 'CDY'
        CALL MZBOOK (IDIVCU, LUSECU, LUSECU, 2, 'TPSR', 0, 0, NDAT, 5,0)
*
*  *    Comment
*
        COMNT = 'TOPSER AT 870731 103000'
        CALL UCTOH (COMNT, IQ(LUSECU+1), 4, 40)
        CALL CDSTOR ('//DBL3/TRIG/TOPSER', LUSECU, LKTRCU(1),
     +               IDIVCU, KEYX, CHOPT, IRC)
        IF (LUSECU.NE.0)  THEN
#if defined(CERNLIB__DEBUG)
          CALL DZSHOW ('TOPSER  ', IDIVCU, LUSECU, 'B', 0, 0, 0, 0)
#endif
          CALL MZDROP (IDIVCU, LUSECU, ' ')
        ENDIF
        CALL UOPTC (CHOPT, 'C', IOPTC)
        IF (IOPTC.NE.0)  THEN
#if defined(CERNLIB__DEBUG)
          IF (I.EQ.1.OR.I.EQ.50) THEN
#endif
#if !defined(CERNLIB__DEBUG)
          IF (I.EQ.1) THEN
#endif
            CALL DZSHOW ('LKTOPSER', IDIVCU, LKTRCU(1), 'BLV', 0,0, 0,0)
          ENDIF
          CALL MZDROP (IDIVCU, LKTRCU(1), 'L')
        ENDIF
   30 CONTINUE
*
*  ** Change first 20 TOPSER at time IT2  and date ID2
*
      ID2   = 880731
      IT2   = 163000
      CALL CDPKTS (ID2, IT2, IDT2, IRC)
*
      DO 40 I = 1, 20
        CALL VZERO (KEYX, NKEX)
        KEYX(NOFF1+1) = IDT2
        KEYX(NOFF1+2) = INFIN
        KEYX(NOFFP)   = IPROG
*
*  *    Topological Module
*
        ITYP  = I/3 + 1
        KEYX(NOFF2+1) = 100*ITYP + I
*
*  *    Serial Number of Module
*
        J     = (I-1)/10 + 1
        MODU1(1:1) = 'X'
        CALL UCTOH (MODU1, KEYX(NOFF2+2), 4, 4)
        IMODU = 10*I
        WRITE (MODU2, 1001) IMODU
        CALL UCTOH (MODU2, KEYX(NOFF2+3), 4, 4)
*
*  **   Store Module Properties
*
        SERNO = MODU1//MODU2
        MODU1(1:1) = MODU5(J)
        SEROL  = MODU1//MODU2
        CALL CDMODP (SEROL, SERNO, ITYP, IDT2)
        CALL UCOPY (KEYX, KEYO, NKEX)
        CALL UCTOH (SEROL, KEYO(NOFF2+2), 4, 8)
        CHOPT = 'CDY'
        CALL MZBOOK (IDIVCU, LUSECU, LUSECU, 2, 'TPSR', 0, 0, NDAT, 5,0)
*
*  *    Comment
*
        COMNT = 'TOPSER AT 880731 163000'
        CALL UCTOH (COMNT, IQ(LUSECU+1), 4, 40)
        CALL CDREPL ('//DBL3/TRIG/TOPSER', LUSECU, LKTRCU(1),
     +               IDIVCU, KEYO, KEYX, CHOPT, IRC)
        IF (LUSECU.NE.0)  THEN
#if defined(CERNLIB__DEBUG)
          CALL DZSHOW ('TOPSER  ', IDIVCU, LUSECU, 'B', 0, 0, 0, 0)
#endif
          CALL MZDROP (IDIVCU, LUSECU, ' ')
        ENDIF
        CALL UOPTC (CHOPT, 'C', IOPTC)
        IF (IOPTC.NE.0)  THEN
#if defined(CERNLIB__DEBUG)
          IF (I.EQ.1.OR.I.EQ.20) THEN
#endif
#if !defined(CERNLIB__DEBUG)
          IF (I.EQ.1) THEN
#endif
            CALL DZSHOW ('LKTOPSER', IDIVCU, LKTRCU(1), 'BLV', 0,0, 0,0)
          ENDIF
          CALL MZDROP (IDIVCU, LKTRCU(1), 'L')
        ENDIF
   40 CONTINUE
*
*  ** Change last 20 TOPSER at time IT3  and date ID3
*
      ID3   = 900131
      IT3   = 221500
      CALL CDPKTS (ID3, IT3, IDT3, IRC)
*
      DO 50 I = 31, 50
        CALL VZERO (KEYX, NKEX)
        KEYX(NOFF1+1) = IDT3
        KEYX(NOFF1+2) = INFIN
        KEYX(NOFFP)   = IPROG
*
*  *    Topological Module
*
        ITYP  = I/3 + 1
        KEYX(NOFF2+1) = 100*ITYP + I
*
*  *    Serial Number of Module
*
        J     = (I-1)/10 + 1
        MODU1(1:1) = 'Y'
        CALL UCTOH (MODU1, KEYX(NOFF2+2), 4, 4)
        IMODU = 10*I
        WRITE (MODU2, 1001) IMODU
        CALL UCTOH (MODU2, KEYX(NOFF2+3), 4, 4)
*
*  **   Store Module Properties
*
        SERNO = MODU1//MODU2
        MODU1(1:1) = MODU5(J)
        SEROL  = MODU1//MODU2
        CALL CDMODP (SEROL, SERNO, ITYP, IDT3)
        CALL UCOPY (KEYX, KEYO, NKEX)
        CALL UCTOH (SEROL, KEYO(NOFF2+2), 4, 8)
        CHOPT = 'DY'
        CALL MZBOOK (IDIVCU, LUSECU, LUSECU, 2, 'TPSR', 0, 0, NDAT, 5,0)
*
*  *    Comment
*
        COMNT = 'TOPSER AT 900131 221500'
        CALL UCTOH (COMNT, IQ(LUSECU+1), 4, 40)
        CALL CDREPL ('//DBL3/TRIG/TOPSER', LUSECU, LKTRCU(1),
     +               IDIVCU, KEYO, KEYX, CHOPT, IRC)
        IF (LUSECU.NE.0)  THEN
#if defined(CERNLIB__DEBUG)
          CALL DZSHOW ('TOPSER  ', IDIVCU, LUSECU, 'B', 0, 0, 0, 0)
#endif
          CALL MZDROP (IDIVCU, LUSECU, ' ')
        ENDIF
        CALL UOPTC (CHOPT, 'C', IOPTC)
        IF (IOPTC.NE.0)  THEN
#if defined(CERNLIB__DEBUG)
          IF (I.EQ.31.OR.I.EQ.50) THEN
#endif
#if !defined(CERNLIB__DEBUG)
          IF (I.EQ.31) THEN
#endif
            CALL DZSHOW ('LKTOPSER', IDIVCU, LKTRCU(1), 'BLV', 0,0, 0,0)
          ENDIF
          CALL MZDROP (IDIVCU, LKTRCU(1), 'L')
        ENDIF
   50 CONTINUE
*
*  ** The propeerties of the last two modules are changed at ID4 and IT4
*
      ID4   = 920101
      IT4   = 103000
      CALL CDPKTS (ID4, IT4, IDT4, IRC)
      CALL CDMODP ('YMOD 490', 'YMOD 490', ITYP, IDT4)
      CALL CDMODP ('YMOD 500', 'YMOD 500', ITYP, IDT4)
*
*  ** Set up Cable Connections
*
      NICKN = 0
      DO 60 ITYP = 1, 17
        NICKN = NICKN + 1
        CALL CDCBCN (ITYP, NICKN, IDT1)
   60 CONTINUE
*
 1001 FORMAT (I4)
*                                                             END CDL1ET
      END
