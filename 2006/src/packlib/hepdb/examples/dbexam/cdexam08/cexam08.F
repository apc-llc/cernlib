*
* $Id: cexam08.F,v 1.1.1.1 1996/02/28 16:23:51 mclareni Exp $
*
* $Log: cexam08.F,v $
* Revision 1.1.1.1  1996/02/28 16:23:51  mclareni
* Hepdb, cdlib, etc
*
*
#include "hepdb/pilot.h"
      PROGRAM CEXAM08
*     ===============
*
************************************************************************
*                                                                      *
*        PROG. CEXAM08                                                 *
*                                                                      *
*   Program to debug CDUSEM on a fictitious Data Base for partitioned  *
*   directories                                                        *
*                                                                      *
*   This program uses /CSTOR1/ as store 1 (ZEBRA store 0) where the    *
*   ZEBRA system division is created and /CSTOR8/ as store 2 (ZEBRA    *
*   store 1) where the DB-divisions (both system and user) are         *
*   created. This tests the complete generality of DBL3 (independence  *
*   from store allocation). The user can make use of this example to   *
*   initialize the store(s) and DBL3 for his own application program.  *
*                                                                      *
************************************************************************
*
      PARAMETER       (NLAS1=100000, NLAS2=150000, NDIV1=5000)
      COMMON /CSTOR1/ IFENC1(4), LQ1(NLAS1)
      COMMON /CSTOR8/ IFENC2(22), LQ2(NLAS2)
      COMMON /QUEST/  IQUEST(100)
#include "cuser8.inc"
      CHARACTER       CHOPT*8
*
*     ------------------------------------------------------------------
*
* *** Open the RZ-file with an Open Statement (or JCL, etc)
*
      LUNRZ  = 1
      LPRTCU = 6
      LREC   = 1024
      NBYTE  = 4*LREC
      NREC   = 0
#if defined(CERNLIB_IBM)
      CALL CDROPN (LUNRZ, ' ', 'DBUVDS.DATA.A4', NBYTE, IOERR)
#endif
#if !defined(CERNLIB_IBM)
      CALL CDROPN (LUNRZ, ' ', 'DBUVDS.DAT', NBYTE, IOERR)
#endif
      IF (IOERR.NE.0)        GO TO 999
      CALL MZEBRA (-1)
*
* *** Open two Stores one for system and one for user division
*
      CALL MZSTOR (IST1DU, '/CSTOR1/', ' ', IFENC1(1), LQ1(1), LQ1(1)
     +           , LQ1(1), LQ1(NDIV1), LQ1(NLAS1))
      CALL MZSTOR (IST2DU, '/CSTOR8/', ' ', IFENC2(1), LQ2(1), LQ2(1)
     +           , LQ2(1), LQ2(NDIV1), LQ2(NLAS2))
*
* *** Log-Level
*
#if !defined(CERNLIB__DEBUG)
      CALL MZLOGL (IST1DU, 0)
      CALL MZLOGL (IST2DU, 0)
#endif
#if defined(CERNLIB__DEBUG)
      CALL MZLOGL (IST1DU, 1)
      CALL MZLOGL (IST2DU, 1)
#endif
*
* *** Define user division and link area like:
*
      CALL MZDIV  (IST2DU, IDIVCU, 'DB-USERS', 20000, 30000, 'L')
      CALL MZLINK (IST2DU, '/CUSER8/', LDUVCU(1), LKUVCU(16), LDUVCU(1))
*
* *** Expand the system-division to its maximum
*
      LARGE  = NLAS1/2
      IDIVS  = 20
      CALL MZNEED (IDIVS, LARGE, 'G')
      ND     = LARGE + IQUEST(11) - 20
      IF (ND.GT.0) THEN
        CALL MZBOOK (IDIVS, L, 0, 2, 'DUMY', 0, 0, ND, 2, -1)
        CALL MZDROP (IDIVS, L, ' ')
      ENDIF
*
* *** Initialize DB-Package
*
      CALL TIMEST (999999.)
      NPAIR = 1
      LUFZ  = 0
      NTOP  = 0
      CALL CDINIT (IDIVCU, LUNRZ, LUFZ, 'DBL3', NPAIR, NREC, NTOP,
     +             ' ', IRC)
#if !defined(CERNLIB__DEBUG)
      CALL CDLOGL ('DBL3',  0, ' ', IRC)
#endif
#if defined(CERNLIB__DEBUG)
      CALL CDLOGL ('DBL3',  2, ' ', IRC)
      CALL DZSTOR ('CSTOR8', IST2DU)
      CALL DZAREA ('DB-LINKS', IST2DU, ' ', 0, ' ')
#endif
*
* *** Read the Partitioned Data Base
*
      IYEAR = 80
      MONTH =  1
      IDAY  =  3
      CALL TIMEX (TIMINT)
      CALL CDUVTX (IDATE, ITIM1)
      CALL CDONUV (0)
      DO 10 I = 1, 100
        IDATX = IYEAR*10000 + MONTH*100 + IDAY
        CALL CDPKTS (IDATX, 0, ITIME, IRC)
        CALL CDONUV (ITIME)
        IDAY  = IDAY  + 10
        IF (IDAY.GT.25)  THEN
          MONTH = MONTH + 1
          IDAY  = 3
          IF (MONTH.GT.12) THEN
            IYEAR = IYEAR + 1
            MONTH = 1
          ENDIF
        ENDIF
   10 CONTINUE
*
      CALL TIMEX (TIMNOW)
      TIME  = TIMNOW - TIMINT
      CALL CDUVTX (IDATE, ITIM2)
      ITIME = ITIM2 - ITIM1
      WRITE (LPRTCU, 1001) IDATE, ITIM1, ITIM2, ITIME
      WRITE (LPRTCU, 1002) TIMINT, TIMNOW, TIME
#if defined(CERNLIB__DEBUG)
*
* *** Test subroutine CDPRIN
*
      CHOPT  = 'K'
      CALL CDPRIN ('//DBL3/MUCH/MCALB/UVDS', CHOPT, IRC)
#endif
*
* *** Plot some data objects and validity period
*
      CALL CDPTES
*
* *** Close DataBase
*
      CALL CDEND  ('*', 'A', IRC)
*
 1001 FORMAT (/,' CEXAM08 : IDAY/ITIM1/ITIM2/ITIME ', 4I12)
 1002 FORMAT (/,' CEXAM08 : TIMINT/TIMNOW/TIME ',4G12.4)
*                                                            END CEXAM08
  999 END
