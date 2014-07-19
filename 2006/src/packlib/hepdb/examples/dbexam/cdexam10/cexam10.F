*
* $Id: cexam10.F,v 1.1.1.1 1996/02/28 16:23:53 mclareni Exp $
*
* $Log: cexam10.F,v $
* Revision 1.1.1.1  1996/02/28 16:23:53  mclareni
* Hepdb, cdlib, etc
*
*
#include "hepdb/pilot.h"
      PROGRAM CEXAM10
*     ===============
*
************************************************************************
*                                                                      *
*        PROG. CEXAM10                                                 *
*                                                                      *
*   Program to debug CDPURG and CDPRIN on a fictitious Data Base       *
*                                                                      *
************************************************************************
*
      PARAMETER       (NLAST=150000, NDIV1=5000)
      COMMON /GCBANK/ FENCCS(22), LQ(NLAST)
      CHARACTER       CHOPT*8
      PARAMETER       (NOFF2=12)
      DIMENSION       KEYS(100), MASK(100)
*     ------------------------------------------------------------------
*
* *** Open the RZ-file with an Open Statement (or JCL, etc)
*
      LUNRZ  = 1
      NREC   = 1000
      LREC   = 1024
      NBYTE  = 4*LREC
#if defined(CERNLIB_IBM)
      CALL CDROPN (LUNRZ, 'O', 'DBMUON.DATA.A4', NBYTE, IOERR)
#endif
#if !defined(CERNLIB_IBM)
      CALL CDROPN (LUNRZ, 'O', 'DBMUON.DAT', NBYTE, IOERR)
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
      CALL MZDIV  (ISTOCU, IDIVCU, 'DB-USERS', 20000, 30000, 'L')
*
* *** Initialize DB-Package
*
      NPAIR = 1
      LUFZ  = 0
      NTOP  = 0
      CALL CDINIT (IDIVCU, LUNRZ, LUFZ, 'DBL3', NPAIR, NREC, NTOP,
     +             'U', IRC)
#if !defined(CERNLIB__DEBUG)
      CALL CDLOGL ('DBL3',  0, ' ', IRC)
#endif
#if defined(CERNLIB__DEBUG)
      CALL CDLOGL ('DBL3',  2, ' ', IRC)
#endif
*
* *** Test subroutine CDPRIN
*
      PRINT 1001
      READ  (*, 1002) CHOPT
      CALL CDPRIN ('//DBL3/MUCH/MCALB/UVDS', 'K', IRC)
*
* *** Test subroutine CDPURG
*
      MASK(NOFF2+1) = 1
      KEYS(NOFF2+1) = 10
      CALL CDPKTS (801222, 0, ITIME, IRC)
      KYDAT  = NOFF2 + 1
      CALL CDPURK ('//DBL3/MUCH/MCALB/UVDS', ITIME, MASK,KEYS, ' ', IRC)
      CALL CDPRIN ('//DBL3/MUCH/MCALB/UVDS', 'K', IRC)
      CALL CDPURG ('//DBL3/MUCH/MCALB/UVDS', KYDAT, 10, CHOPT, IRC)
      CALL CDPRIN ('//DBL3/MUCH/MCALB/UVDS', 'K', IRC)
*
* *** Close DataBase
*
      CALL CDEND  ('*', 'A', IRC)
*
 1001 FORMAT (' Give the character option in purging : ')
 1002 FORMAT (A)
*                                                            END CEXAM10
  999 END
