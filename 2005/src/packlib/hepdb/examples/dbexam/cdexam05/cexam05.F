*
* $Id: cexam05.F,v 1.1.1.1 1996/02/28 16:23:47 mclareni Exp $
*
* $Log: cexam05.F,v $
* Revision 1.1.1.1  1996/02/28 16:23:47  mclareni
* Hepdb, cdlib, etc
*
*
#include "hepdb/pilot.h"
      PROGRAM CEXAM05
*     ===============
*
************************************************************************
*                                                                      *
*        PROG. CEXAM05                                                 *
*                                                                      *
*   Program to create a fictitious Data Base to test the Package       *
*           including CDSTOR, CDMDIR, CDUSEM (opt.'S') and nkeys > 14  *
*                                                                      *
*   Creates an immitation of the Trigger Data Base                     *
*                                                                      *
************************************************************************
*
      PARAMETER       (NLAST=150000, NDIV1=5000)
      COMMON /GCBANK/ FENCCS(22), LQ(NLAST)
*
#include "cuser5.inc"
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
      CALL CDROPN (LUNRZ, 'ZO', 'DBTRIG.DATA.A4', NBYTE, IOERR)
#endif
#if !defined(CERNLIB_IBM)
      CALL CDROPN (LUNRZ, 'ZO', 'DBTRIG.DAT', NBYTE, IOERR)
#endif
      IF (IOERR.NE.0)        GO TO 999
      CALL MZEBRA (-1)
*
* *** Open the 1 and only 1 Store used in the reconstruction
*
      CALL MZSTOR (ISTOCU, '/GCBANK/', ' ', FENCCS(1), LQ(1), LQ(1)
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
      CALL MZLINK (ISTOCU, '/CUSER5/', LDTRCU(1), LUSECU, LDTRCU(1))
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
* *** Create the Trigger Data Base
*
      CALL TIMEX (TIMINT)
      CALL CDUVTX (IDAY, ITIM1)
      CALL CDL1ET
*
      CALL TIMEX (TIMNOW)
      TIME  = TIMNOW - TIMINT
      CALL CDUVTX (IDAY, ITIM2)
      ITIME = ITIM2 - ITIM1
      WRITE (LPRTCU, 1001) IDAY, ITIM1, ITIM2, ITIME
      WRITE (LPRTCU, 1002) TIMINT, TIMNOW, TIME
*
* *** Close Data Base
*
      CALL CDEND  ('*', 'A', IRC)
*
 1001 FORMAT (/,' CEXAM05 : IDAY/ITIM1/ITIM2/ITIME ', 4I12)
 1002 FORMAT (/,' CEXAM05 : TIMINT/TIMNOW/TIME ',4G12.4)
*                                                            END CEXAM05
  999 END
