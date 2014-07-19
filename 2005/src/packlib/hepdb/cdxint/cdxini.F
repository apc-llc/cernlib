*
* $Id: cdxini.F,v 1.1.1.1 1996/02/28 16:24:53 mclareni Exp $
*
* $Log: cdxini.F,v $
* Revision 1.1.1.1  1996/02/28 16:24:53  mclareni
* Hepdb, cdlib, etc
*
*
#include "hepdb/pilot.h"
      SUBROUTINE CDXINI (LCORE)
*     =========================
*
************************************************************************
*                                                                      *
*        SUBR. CDXINI (LCORE)                                          *
*                                                                      *
*   Routine to Initialize the DB Interactive Run                       *
*                                                                      *
*   Arguments :                                                        *
*                                                                      *
*     LCORE    Core Size of /GCBANK/                                   *
*                                                                      *
*   Called by CDMAIN                                                   *
*                                                                      *
************************************************************************
*
#include "hepdb/cxlink.inc"
#include "hepdb/ccdisp.inc"
      PARAMETER       (NDIV1=5000, LCORQ=100000)
      COMMON /GCBANK/ FENCCD(22), LQ(LCORQ)
      DIMENSION       IAR(3)
*
*     ------------------------------------------------------------------
*
      LURZCX = 1
      L3PRCX = 6
      LCORU  = MAX (LCORE, LCORQ)
*
      CALL MZSTOR (ISTOCX, '/GCBANK/', ' ', FENCCD(1), LQ(1), LQ(1),
     +             LQ(1), LQ(NDIV1), LQ(LCORU))
      NDIV2  = 3*LCORU/4
*
      CALL MZDIV  (ISTOCX, IDIV, 'DB-USERS', NDIV2/2, NDIV2, 'LC')
      CALL MZLINK (ISTOCX, '/CXLINK/', LFRSCX, LFRSCX, LASTCX)
*
      IAR(1) = LURZCX
      IAR(2) = ISTOCX
      IAR(3) = IDIV
      CALL CDPRNT (L3PRCX, '(/,''  ----- Interactive Session on HEPDB'//
     +     ' Test-Run Starts -----'',//,10X,''RZ-Unit Number : '',I5'//
     +     ','' HEPDB_Store :'',I10,''  User-Division :'',I10,/)',IAR,3)
*
*  ** Set Default Display Range
*
      MXDPCC = 80
      WRITE (CFMTCC, '(''(A'',I3,'')'')') MXDPCC
*                                                             END CDXINI
  999 END
