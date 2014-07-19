*
* $Id: cdcrfk.F,v 1.1.1.1 1996/02/28 16:23:46 mclareni Exp $
*
* $Log: cdcrfk.F,v $
* Revision 1.1.1.1  1996/02/28 16:23:46  mclareni
* Hepdb, cdlib, etc
*
*
#include "hepdb/pilot.h"
      SUBROUTINE CDCRFK (ITIM0)
*     =========================
*
************************************************************************
*                                                                      *
*        SUBR. CDCRFK (ITIM0)                                          *
*                                                                      *
*   Creates a complete fictitious Data Base to accommodate 50 user keys*
*                                                                      *
*   Arguments :                                                        *
*                                                                      *
*     ITIM1    Start time of validity                                  *
*                                                                      *
*   Called by CEXAM03                                                  *
*                                                                      *
************************************************************************
*
#include "cstor3.inc"
#include "cuser3.inc"
      PARAMETER       (NOFF1=10, NOFF2=12, NOFFP=6, NKEX=NOFF2+50)
      DIMENSION       KEYX(NKEX), K(500,2), P(500,5)
      INTEGER         BITS(20)
      CHARACTER       CHOPT*8, CRATE(20)*4
      DATA            IPROG /1/, XRANG / 1000.0/, XCENT / 500.0/
      DATA            CRATE /'CR01','CR02','CR03','CR04','CR05',
     +                       'CR06','CR07','CR08','CR09','CR10',
     +                       'CR11','CR12','CR13','CR14','CR15',
     +                       'CR16','CR17','CR18','CR19','CR20'/
#if defined(CERNLIB_APOLLO)
      DATA            BITS /16#0001, 16#0008, 16#0040, 16#0200, 16#1000,
     +                      16#0001, 16#0009, 16#0041, 16#0201, 16#1001,
     +                      16#0001, 16#0009, 16#0049, 16#0209, 16#1009,
     +                      16#0001, 16#0009, 16#0049, 16#0249, 16#1049/
#endif
#if defined(CERNLIB_CRAY)
      DATA            BITS /X'0001', X'0008', X'0040', X'0200', X'1000',
     +                      X'0001', X'0009', X'0041', X'0201', X'1001',
     +                      X'0001', X'0009', X'0049', X'0209', X'1009',
     +                      X'0001', X'0009', X'0049', X'0249', X'1049'/
#endif
#if defined(CERNLIB_IBM)
      DATA            BITS /Z0001, Z0008, Z0040, Z0200, Z1000,
     +                      Z0001, Z0009, Z0041, Z0201, Z1001,
     +                      Z0001, Z0009, Z0049, Z0209, Z1009,
     +                      Z0001, Z0009, Z0049, Z0249, Z1049/
#endif
#if (!defined(CERNLIB_APOLLO))&&(!defined(CERNLIB_CRAY))&&(!defined(CERNLIB_IBM))
      DATA            BITS /'0001'X, '0008'X, '0040'X, '0200'X, '1000'X,
     +                      '0001'X, '0009'X, '0041'X, '0201'X, '1001'X,
     +                      '0001'X, '0009'X, '0049'X, '0209'X, '1009'X,
     +                      '0001'X, '0009'X, '0049'X, '0249'X, '1049'X/
#endif
*
*     ------------------------------------------------------------------
*
      CALL TIMEX (TIMINT)
      CALL CDUVTX (IDAY, ITIM1)
*
* *** Prepare the keys for the structure
*
      CALL CDPKTS (991231, 235000, INFIN, IRC)
      CALL VZERO (KEYX, NKEX)
      KEYX(NOFF1+1) = ITIM0
      KEYX(NOFF1+2) = INFIN
      KEYX(NOFFP)   = IPROG
      DO 10 I = NOFF2+1, NOFF2+3
        KEYX(I) = 50000*RNDM(I)
   10 CONTINUE
      DO 15 I = NOFF2+4, NOFF2+23
        CALL UCTOH (CRATE(I-NOFF2-3), KEYX(I), 4, 4)
   15 CONTINUE
      CALL UCOPY (BITS(1), KEYX(NOFF2+24), 20)
      DO 20 I = NOFF2+44, NKEX
        KEYX(I) = 50000*RNDM(I)
   20 CONTINUE
*
* *** Create Data to store with DBOUT 'R' Option  - most general I/O
* ***     (Example is taken from the Generators)
*
      N      = 25
      NDAT   = 2 + 5 + 7*N
      CALL MZFORM ('CRAT', '1I 3F 1I -S', IOCRA)
      CALL MZBOOK (IDIVCU, LUSECU, LUSECU, 2, 'CRAT', 0,0,NDAT, IOCRA,0)
*
      IE     = 10001
      DO 35 I = 1, N
        DO 25 J = 1, 2
          K(I,J) = (J-1)*25 + I
   25   CONTINUE
        DO 30 J = 1, 5
          P(I,J) = (J-1)*250. + I*0.1
   30   CONTINUE
   35 CONTINUE
      L      = LUSECU
      IQ(L+1) = IE
      Q(L+2)  = 0.
      Q(L+3)  = 0.
      Q(L+4)  = 0.
      IQ(L+5) = N
      L      = L + 5
      IQ(L+1) = 16*N*2 + 2
      L      = L + 1
      DO 40 I = 1, N
        IQ(L+1) = K(I,1)
        IQ(L+2) = K(I,2)
        L      = L + 2
   40 CONTINUE
      IQ(L+1) = 16*N*5 + 3
      L      = L + 1
      DO 50 I = 1, N
        DO 45 J = 1, 5
          Q(L+J) = P(I,J)
   45   CONTINUE
        L      = L + 5
   50 CONTINUE
*
      CHOPT = 'CDY'
      CALL CDSTOR ('//DBL3/FAKE/CRATES', LUSECU, LKTRCU(1), IDIVCU,
     +             KEYX, CHOPT, IRC)
      IF (LUSECU.NE.0)  THEN
        CALL DZSHOW ('CRAT    ', IDIVCU, LUSECU, 'B', 0, 0, 0, 0)
        CALL MZDROP (IDIVCU, LUSECU, ' ')
      ENDIF
      CALL UOPTC (CHOPT, 'C', IOPTC)
      IF (IOPTC.NE.0)  THEN
        CALL DZSHOW ('LKTRCU  ', IDIVCU, LKTRCU(1), 'LVB', 0, 0, 0, 0)
        CALL MZDROP (IDIVCU, LKTRCU(1), 'L')
      ENDIF
*
      CALL TIMEX (TIMNOW)
      TIME  = TIMNOW - TIMINT
      CALL CDUVTX (IDAY, ITIM2)
      ITIME = ITIM2 - ITIM1
      WRITE (LPRTCU, 1001) IDAY, ITIM1, ITIM2, ITIME
      WRITE (LPRTCU, 1002) TIMINT, TIMNOW, TIME
*
 1001 FORMAT (/,' CDCRFK : IDAY/ITIM1/ITIM2/ITIME ', 4I12)
 1002 FORMAT (/,' CDCRFK : TIMINT/TIMNOW/TIME ',4G12.4)
*                                                             END CDCRFK
      END
