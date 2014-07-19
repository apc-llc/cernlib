*
* $Id: cexam03.F,v 1.1.1.1 1996/02/28 16:23:45 mclareni Exp $
*
* $Log: cexam03.F,v $
* Revision 1.1.1.1  1996/02/28 16:23:45  mclareni
* Hepdb, cdlib, etc
*
*
#include "hepdb/pilot.h"
      PROGRAM CEXAM03
*     ===============
*
************************************************************************
*                                                                      *
*        PROG. CEXAM03                                                 *
*                                                                      *
*   Program to create a fictitious Data Base to test the Package       *
*           including CDSTOR, CDMDIR                                   *
*                                                                      *
*   The Data Base structure is completely arbitary                     *
*                                                                      *
************************************************************************
*
      PARAMETER       (NLAST=150000, NDIV1=5000)
      COMMON /GCBANK/ FENCCS(22), LQ(NLAST)
*
#include "cuser3.inc"
      CHARACTER       CFNM1*80
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
      CFNM1  = 'DBFAKE.DATA.A4'
#endif
#if !defined(CERNLIB_IBM)
      CFNM1  = 'DBFAKE.DAT'
#endif
      CALL CDROPN (LUNRZ, 'ZO',  CFNM1, NBYTE, IOERR)
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
      CALL MZLINK (ISTOCU, '/CUSER3/', LKTRCU(1), LUSECU, LKTRCU(1))
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
* *** Create the Fake Data Base
*
      CALL CDCRFD
      IDATX = 800101
      DO 10 I = 1, 3
        CALL CDPKTS (IDATX, 0, ISTRV, IRC)
        CALL CDCRFK (ISTRV)
        IDATX = IDATX + 10000
   10 CONTINUE
*
* *** Close Data Base
*
      CALL CDEND  ('*', 'A', IRC)
*                                                            END CEXAM03
  999 END
