*
* $Id: cexam07.F,v 1.1.1.1 1996/02/28 16:23:50 mclareni Exp $
*
* $Log: cexam07.F,v $
* Revision 1.1.1.1  1996/02/28 16:23:50  mclareni
* Hepdb, cdlib, etc
*
*
#include "hepdb/pilot.h"
      PROGRAM CEXAM07
*     ===============
*
************************************************************************
*                                                                      *
*        PROG. CEXAM07                                                 *
*                                                                      *
*   Program to create a fictitious database to test the partitioned    *
*   directory (CDSTOR with option P at the time of creating the user   *
*   directory)                                                         *
*                                                                      *
*   This program uses a different store from /GCBANK/ and hence both   *
*   the user division and the ZEBRA system division refer to the new   *
*   store /CSTOR7/ and tests the new feature of version 3.00 of DBL3   *
*                                                                      *
************************************************************************
*
      PARAMETER       (NLAST=150000, NDIV1=5000)
      COMMON /CSTOR7/ FENCCS(22), LQ(NLAST)
      COMMON /QUEST/  IQUEST(100)
*
#include "cuser7.inc"
*     ------------------------------------------------------------------
*
* *** Open the RZ-file with an Open Statement (or JCL, etc)
*
      LUNRZ  = 1
      LPRTCU = 6
      NREC   = 1000
      LREC   = 1024
      NBYTE  = 4*LREC
#if defined(CERNLIB_IBM)
      CALL CDROPN (LUNRZ, 'ZO', 'DBUVDS.DATA.A4', NBYTE, IOERR)
#endif
#if !defined(CERNLIB_IBM)
      CALL CDROPN (LUNRZ, 'ZO', 'DBUVDS.DAT', NBYTE, IOERR)
#endif
      IF (IOERR.NE.0)        GO TO 999
      CALL MZEBRA(-1)
*
* *** Open the 1 and only 1 Store used in the reconstruction
*
      CALL MZSTOR (ISTOCU, '/CSTOR7/', ' ', FENCCS(1), LQ(1), LQ(1)
     +           , LQ(1), LQ(NDIV1), LQ(NLAST))
*
* *** Log-Level
*
#if !defined(CERNLIB__DEBUG)
      CALL MZLOGL (ISTOCU, 0)
#endif
#if defined(CERNLIB__DEBUG)
      CALL MZLOGL (ISTOCU, 1)
#endif
*
* *** Define user division and link area like:
*
      CALL MZDIV  (ISTOCU, IDIVCU, 'DB-USERS', 50000, 100000, 'L')
      CALL MZLINK (ISTOCU, '/CUSER7/', LDUVCU, LUSECU, LDUVCU)
*
* *** Expand the system-division to its maximum
*
      LARGE  = NLAST/2
      IDIVS  = IDIVCU - 1
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
     +             'UZ', IRC)
#if !defined(CERNLIB__DEBUG)
      CALL CDLOGL ('DBL3',  0, ' ', IRC)
#endif
#if defined(CERNLIB__DEBUG)
      CALL CDLOGL ('DBL3',  2, ' ', IRC)
#endif
*
* *** Create the Data Base with partitioned directory
*
      IPREC  = 3
      DELTA  = 0.
      CALL CDMDIR ('//DBL3/MUCH/MCALB/UVDS', 1, 'I', 'US_KEY_1', 200,
     +             IPREC, DELTA, 'CP', IRC)
      IYEAR = 80
      MONTH =  1
      IDAY  =  1
      CALL TIMEX (TIMINT)
      CALL CDUVTX (IDATE, ITIM1)
      DO 10 I = 1, 500
        IDATX = IYEAR*10000 + MONTH*100 + IDAY
        CALL CDPKTS (IDATX, 0, ISTRV, IRC)
        CALL CDCRUV (ISTRV)
        IDAY  = IDAY  + 10
        IF (IDAY.GT.25)  THEN
          MONTH = MONTH + 1
          IDAY  = 1
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
*
* *** Close Data Base
*
      CALL CDEND  ('*', 'A', IRC)
*
 1001 FORMAT (/,' CEXAM07 : IDAY/ITIM1/ITIM2/ITIME ', 4I12)
 1002 FORMAT (/,' CEXAM07 : TIMINT/TIMNOW/TIME ',4G12.4)
*                                                            END CEXAM07
  999 END
