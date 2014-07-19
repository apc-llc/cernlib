*
* $Id: cexam04.F,v 1.1.1.1 1996/02/28 16:23:46 mclareni Exp $
*
* $Log: cexam04.F,v $
* Revision 1.1.1.1  1996/02/28 16:23:46  mclareni
* Hepdb, cdlib, etc
*
*
#include "hepdb/pilot.h"
      PROGRAM CEXAM04
*     ===============
*
************************************************************************
*                                                                      *
*        PROG. CEXAM04                                                 *
*                                                                      *
*   Program to debug CDUSEM on a fictitious Data Base                  *
*                                                                      *
*   Program is used to test option 'S' in CDUSEM for 50 user keys      *
*                                                                      *
************************************************************************
*
      PARAMETER       (NLAST=150000, NDIV1=5000)
      COMMON /GCBANK/ FENCCS(22), LQ(NLAST)
*
#include "cuser4.inc"
*     ------------------------------------------------------------------
*
* *** Open the RZ-file with an Open Statement (or JCL, etc)
*
      LUNRZ  = 1
      LPRTCU = 6
      LREC   = 1024
      NREC   = 0
      NBYTE  = 4*LREC
#if defined(CERNLIB_IBM)
      CALL CDROPN (LUNRZ, ' ', 'DBFAKE.DATA.A4', NBYTE, IOERR)
#endif
#if !defined(CERNLIB_IBM)
      CALL CDROPN (LUNRZ, ' ', 'DBFAKE.DAT', NBYTE, IOERR)
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
      CALL MZLINK (ISTOCU, '/CUSER4/', LKTRCU(1), LDTRCU(5), LKTRCU(1))
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
#endif
*
* *** Read the Data Base
*
      CALL CDONFK
*
* *** Close DataBase
*
      CALL CDEND  ('*', 'A', IRC)
*                                                            END CEXAM04
  999 END
