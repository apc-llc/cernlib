*
* $Id: cexam02.F,v 1.1.1.1 1996/02/28 16:23:44 mclareni Exp $
*
* $Log: cexam02.F,v $
* Revision 1.1.1.1  1996/02/28 16:23:44  mclareni
* Hepdb, cdlib, etc
*
*
#include "hepdb/pilot.h"
      PROGRAM CEXAM02
*     ===============
*
************************************************************************
*                                                                      *
*        PROG. CEXAM02                                                 *
*                                                                      *
*   Program to debug DBUSE on a fictitious Data Base. It can also      *
*   debug the routines CDPRIN, CDPURG and CDSTAT.                      *
*                                                                      *
*   The Data Base structure is an extraction from the Muon Chamber     *
*   database as proposed by B.Adeva and E.Gonzalez in L3 Report No.496 *
*                                                                      *
************************************************************************
*
      PARAMETER       (NLAST=150000, NDIV1=5000)
      COMMON /GCBANK/ FENCCS(22), LQ(NLAST)
#include "cuser2.inc"
#include "cvser2.inc"
      PARAMETER       (NOFF2=12)
      CHARACTER       CHOPT*8, CFNM1*80, CFNM2*80, CFNM3*80
*
*     ------------------------------------------------------------------
*
      CALL MZEBRA (-1)
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
      LU2RZ  = 3
      LPRTCU = 6
      NREC   = 1000
      LREC   = 1024
      NBYTE  = 4*LREC
      NBYTF  = 80
#if defined(CERNLIB_IBM)
      CFNM1  = 'DBMUON.DATA.A4'
      CFNM2  = 'DBMUFZ.DATA.A'
      CFNM3  = 'DBMUOM.DATA.A4'
#endif
#if !defined(CERNLIB_IBM)
      CFNM1  = 'DBMUON.DAT'
      CFNM2  = 'DBMUFZ.DAT'
      CFNM3  = 'DBMUOM.DAT'
#endif
      CALL CDROPN (LUNRZ, '  ',  CFNM1, NBYTE, IOERR)
      IF (IOERR.NE.0)        GO TO 999
      CALL CDROPN (LU2RZ, 'ZO',  CFNM3, NBYTE, IOERR)
      IF (IOERR.NE.0)        GO TO 999
      CALL CDSOPN (LUNFZ, 'AI',  CFNM2, NBYTF, IOERR)
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
      CALL MZDIV  (ISTOCU, IDIVCU, 'DB-USERS', 20000, 30000, 'L')
      CALL MZLINK (ISTOCU, '/CUSER2/', LKUVCU(1), LDYBCU(17), LKUVCU(1))
      CALL MZLINK (ISTOCU, '/CVSER2/', LKUVCV(1), LDYBCV(17), LKUVCV(1))
*
* *** Initialize DB-Package
*
      CALL TIMEST (999999.)
      NPAIR = 1
      LUFZ  = 0
      NTOP1 = 0
      NTOP2 = 0
      CALL CDINIT (IDIVCU, LU2RZ, LUFZ, 'DBL3', NPAIR, NREC, NTOP1,
     +             'UZ', IRC)
      CALL CDINIT (IDIVCU, LUNRZ, LUFZ, 'DVL3', NPAIR, NREC, NTOP2,
     +             ' ', IRC)
#if !defined(CERNLIB__DEBUG)
      CALL CDLOGL ('DBL3',  0, ' ', IRC)
      CALL CDLOGL ('DVL3',  0, ' ', IRC)
#endif
#if defined(CERNLIB__DEBUG)
      CALL CDLOGL ('DBL3',  2, ' ', IRC)
      CALL CDLOGL ('DVL3',  2, ' ', IRC)
      CALL DZSTOR ('DBSTORE', ISTOCU)
      CALL DZAREA ('DB-LINKS', ISTOCU, ' ', 0, ' ')
#endif
*
* *** Retrieve the data base from the journal file
*
      CALL TIMEX (TIMINT)
      CALL CDUVTX (IDAY, ITIM1)
      CALL CDFZUP (LUNFZ, ' ', IRC)
      CALL FZENDI (LUNFZ, 'T')
*
      CALL TIMEX (TIMNOW)
      TIME  = TIMNOW - TIMINT
      CALL CDUVTX (IDAY, ITIM2)
      ITIME = ITIM2 - ITIM1
      WRITE (LPRTCU, 1001) IDAY, ITIM1, ITIM2, ITIME
      WRITE (LPRTCU, 1002) TIMINT, TIMNOW, TIME
*
* *** Initialize the Muon Data Base structure
*
      CALL CDINMU
*
* *** Read the Muon Data Base
*
      IDATX = 800601
      DO 10 I = 1, 3
        CALL CDPKTS (IDATX, 120000, ITIME, IRC)
        CALL CDONMU (ITIME)
        IDATX = IDATX + 10000
   10 CONTINUE
*
* *** Test subroutine CDPRIN
*
#if !defined(CERNLIB__DEBUG)
      CHOPT  = 'K'
#endif
#if defined(CERNLIB__DEBUG)
      CHOPT  = 'D'
#endif
      CALL CDPRIN ('//DBL3/MUCH/MCALB/UVDS', CHOPT, IRC)
      CALL CDPRIN ('//DVL3/MUCH/MCALB/UVDS', CHOPT, IRC)
*
* *** Test subroutine DBPURG
*
      KYDAT  = NOFF2 + 1
      CALL CDPURG ('//DBL3/MUCH/MCALB/UVDS', KYDAT, 10, 'K', IRC)
      CALL CDPRIN ('//DBL3/MUCH/MCALB/UVDS', CHOPT, IRC)
*
* *** Print out the summary of usage
*
      CALL CDSTAT (LPRTCU, IRC)
*
* *** Test subroutine DBDELT
*
      CALL RZSTAT ('//DBL3', 20, ' ')
      CALL CDDDIR ('//DBL3/MUCH/MPARS/MGEOM', ' ', IRC)
      CALL RZSTAT ('//DBL3', 20, ' ')
*
* *** Close DataBase
*
      CALL CDEND  ('*', 'A', IRC)
*
 1001 FORMAT (/,' CEXAM02 : IDAY/ITIM1/ITIM2/ITIME ', 4I12)
 1002 FORMAT (/,' CEXAM02 : TIMINT/TIMNOW/TIME ',4G12.4)
*                                                            END CEXAM02
  999 END
