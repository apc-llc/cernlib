*
* $Id: cexam01.F,v 1.1.1.1 1996/02/28 16:23:43 mclareni Exp $
*
* $Log: cexam01.F,v $
* Revision 1.1.1.1  1996/02/28 16:23:43  mclareni
* Hepdb, cdlib, etc
*
*
#include "hepdb/pilot.h"
      PROGRAM CEXAM01
*     ===============
*
************************************************************************
*                                                                      *
*        PROG. CEXAM01                                                 *
*                                                                      *
*   Program to create a fictitious database to test CDMDIR, CDSTOR     *
*                                                                      *
*   The Data Base structure is an extraction from the Muon Chamber     *
*   database as proposed by B.Adeva and E.Gonzalez in L3 Report No.496 *
*                                                                      *
************************************************************************
*
      PARAMETER       (NLAST=150000, NDIV1=5000)
      COMMON /GCBANK/ FENCCS(22), LQ(NLAST)
*
#include "cuser1.inc"
      CHARACTER       CFNM1*80, CFNM2*80
*
*     ------------------------------------------------------------------
*
      CALL MZEBRA(-1)
*
* *** Open the 1 and only 1 Store used in the reconstruction
*
      CALL MZSTOR (ISTOCU, '/GCBANK/', ' ', FENCCS(1), LQ(1), LQ(1)
     +           , LQ(1), LQ(NDIV1), LQ(NLAST))
*
* *** Open the RZ-file with an Open Statement (or JCL, etc)
*
      LUNRZ  = 1
      LUNFZ  = 2
      LPRTCU = 6
      NREC   = 1000
      LREC   = 1024
      NBYTE  = 4*LREC
      NBYTF  = 80
#if defined(CERNLIB_IBM)
      CFNM1  = 'DBMUON.DATA.A4'
      CFNM2  = 'DBMUFZ.DATA.A'
#endif
#if !defined(CERNLIB_IBM)
      CFNM1  = 'DBMUON.DAT'
      CFNM2  = 'DBMUFZ.DAT'
#endif
      CALL CDROPN (LUNRZ, 'ZO',  CFNM1, NBYTE, IOERR)
      IF (IOERR.NE.0)        GO TO 999
      CALL CDSOPN (LUNFZ, 'AZO', CFNM2, NBYTF, IOERR)
      IF (IOERR.NE.0)        GO TO 999
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
      CALL MZLINK (ISTOCU, '/CUSER1/', LDUVCU(1), LUSECU, LDUVCU(1))
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
      CALL CDFZOP (LUNFZ, 'DBL3', ' ', IRC)
*
* *** Create the Muon Data Base
*
      CALL CDCRMD
      IDATX = 800101
      DO 10 I = 1, 3
        CALL CDPKTS (IDATX, 0, ISTRV, IRC)
        CALL CDCRMU (ISTRV)
        IDATX = IDATX + 10000
   10 CONTINUE
*
* *** Close Data Base
*
      CALL FZENDO (LUNFZ, 'T')
      CALL CDEND  ('*', 'A', IRC)
*                                                            END CEXAM01
  999 END
