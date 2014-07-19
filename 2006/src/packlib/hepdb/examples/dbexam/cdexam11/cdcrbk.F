*
* $Id: cdcrbk.F,v 1.1.1.1 1996/02/28 16:23:54 mclareni Exp $
*
* $Log: cdcrbk.F,v $
* Revision 1.1.1.1  1996/02/28 16:23:54  mclareni
* Hepdb, cdlib, etc
*
*
#include "hepdb/pilot.h"
      SUBROUTINE CDCRBK (LUNI, LPRNT, KEYS)
*
************************************************************************
*
*        SUBR. CDCRBK (LUNI, LPRNT, KEYS)
*
*   Creates user bank to be entered into data base
*   (Version prepared to transfer the field map to data base)
*
*   Arguments :
*
*     LUNI     Logical unit number of input file
*     LPRNT    Logical unit number of log   file
*     KEYS     Corresponding Key vector
*
*   Called by CEXAM11
*
************************************************************************
*
      PARAMETER     (KWBANK=69000, KWWORK=5200)
      COMMON /GCBANK/ NZEBRA, GVERSN, ZVERSN, IXSTOR, IXDIV, IXCONS
     +              , FENDQ(16), LMAIN, LR1, WS(KWBANK)
      DIMENSION       IQ(2), Q(2), LQ(8000), IWS(2)
      EQUIVALENCE     (Q(1), IQ(1), LQ(9)), (LQ(1), LMAIN)
      EQUIVALENCE     (IWS(1), WS(1))
      PARAMETER       (NOFF1=10, NOFFP=6)
      COMMON /CUSERB/ LKBKCU, LLBKCU
*
      DIMENSION       KEYS(9)
*                                              start User part of CDCRBK
      REAL            BZM(5000), BRM(5000)
*
*     ------------------------------------------------------------------
*
* *** Read BZ map-limits
*
      READ (LUNI, 2001) ZMINZ,ZMAXZ,RMINZ,RMAXZ,NZZ,NRZ,DZZ,DRZ
      NMAX = NZZ * NRZ
      WRITE (LPRNT, *) ' CDCRBK : NMAX, NZ, NR = ', NMAX, NZZ, NRZ
*
*  ** Read the main-component map
*
      READ (LUNI, 2002) (BZM(I),I=1,NMAX)
*
*  ** Read BR map-limits
*
      READ (LUNI, 2001) ZMINR,ZMAXR,RMINR,RMAXR,NZR,NRR,DZR,DRR
      NMAX = NZR * NRR
      WRITE (LPRNT, *) ' CDCRBK : NMAX, NZ, NR = ', NMAX, NZR, NRR
*
*  ** Read the minor-component map
*
      READ (LUNI, 2002) (BRM(I),I=1,NMAX)
*
* *** Create the FMAP bank
*
      ND   = 2*NMAX + 4
      CALL MZFORM ('FMAP', '2I -F', IOFM)
      CALL MZBOOK (IXDIV, LKBKCU, LKBKCU, 1, 'FMAP', 0, 0, ND, IOFM, -1)
      IQ(LKBKCU+1) = NZZ
      IQ(LKBKCU+2) = NRZ
       Q(LKBKCU+3) = DZZ
       Q(LKBKCU+4) = DRZ
      CALL UCOPY (BZM, Q(LKBKCU+5), NMAX)
      CALL UCOPY (BRM, Q(LKBKCU+NMAX+5), NMAX)
*
* *** Prepare the key vector
*
      CALL CDPKTS (991231, 235959, KEYS(NOFF1+2), IRC)
      KEYS(NOFFP) = 1
*
 2001 FORMAT (4E15.5/2I15,2E15.5)
 2002 FORMAT (4E15.5)
*                                                             END CDCRBK
  999 END
