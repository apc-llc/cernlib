*
* $Id: cexam06.F,v 1.1.1.1 1996/02/28 16:23:49 mclareni Exp $
*
* $Log: cexam06.F,v $
* Revision 1.1.1.1  1996/02/28 16:23:49  mclareni
* Hepdb, cdlib, etc
*
*
#include "hepdb/pilot.h"
      PROGRAM CEXAM06
*     ===============
*
************************************************************************
*                                                                      *
*        PROG. CEXAM06                                                 *
*                                                                      *
*   Program to use a fictitious Trigger Data Base for energy level     *
*   trigger as suggested by T. Hebecker                                *
*                                                                      *
*   This is an example how one can use DBL3 to imitate a relational    *
*   Data base system                                                   *
*                                                                      *
************************************************************************
*
      PARAMETER       (NLAST=150000, NDIV1=5000)
      COMMON /GCBANK/ FENCCS(22), LQ(NLAST)
*
#include "cuser6.inc"
*     ------------------------------------------------------------------
*
* *** Open the RZ-file with an Open Statement (or JCL, etc)
*
      LUNRZ  = 1
      LPRTCU = 6
      LREC   = 1024
      NBYTE  = 4*LREC
#if defined(CERNLIB_IBM)
      CALL CDROPN (LUNRZ, ' ', 'DBTRIG.DATA.A4', NBYTE, IOERR)
#endif
#if !defined(CERNLIB_IBM)
      CALL CDROPN (LUNRZ, ' ', 'DBTRIG.DAT', NBYTE, IOERR)
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
      CALL MZLINK (ISTOCU, '/CUSER6/', LDTRCU(1), LKTRCU(12), LDTRCU(1))
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
* *** Read the Trigger Data Base - Query it for a given time IDT1
*
      CALL TIMEX (TIMINT)
      CALL CDUVTX (IDAY, ITIM1)
      ID1    = 890101
      IT1    = 220000
      CALL CDPKTS (ID1, IT1, IDT1, IRC)
      CALL CDRL1E (IDT1)
*
      CALL TIMEX (TIMNOW)
      TIME  = TIMNOW - TIMINT
      CALL CDUVTX (IDAY, ITIM2)
      ITIME = ITIM2 - ITIM1
      WRITE (LPRTCU, 1001) IDAY, ITIM1, ITIM2, ITIME
      WRITE (LPRTCU, 1002) TIMINT, TIMNOW, TIME
*
* *** Close DataBase
*
      CALL CDEND  ('*', 'A', IRC)
*
 1001 FORMAT (/,' CEXAM06 : IDAY/ITIM1/ITIM2/ITIME ', 4I12)
 1002 FORMAT (/,' CEXAM06 : TIMINT/TIMNOW/TIME ',4G12.4)
*                                                            END CEXAM06
  999 END
