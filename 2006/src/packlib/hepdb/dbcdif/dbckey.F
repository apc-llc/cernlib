*
* $Id: dbckey.F,v 1.1.1.1 1996/02/28 16:25:03 mclareni Exp $
*
* $Log: dbckey.F,v $
* Revision 1.1.1.1  1996/02/28 16:25:03  mclareni
* Hepdb, cdlib, etc
*
*
#include "hepdb/pilot.h"
      SUBROUTINE DBCKEY (KEYS, KEYO, NWKEY)
*     =====================================
*
************************************************************************
*                                                                      *
*        SUBR. DBCKEY (KEYS, KEYO*, NWKEY)                             *
*                                                                      *
*   Changes the convention of KEYS and MASK vector between DB and CD   *
*   It updates MASKDX in /DXUSED/                                      *
*                                                                      *
*   Arguments :                                                        *
*                                                                      *
*     KEYS     Input key vector                                        *
*     KEYO     Output key vector                                       *
*     NWKEY    Number of key elements in KEYO                          *
*                                                                      *
*   Called by various routines in the patch DBCDFC                     *
*                                                                      *
************************************************************************
*
#include "hepdb/cdcblk.inc"
#include "hepdb/ckkeys.inc"
#include "dxused.inc"
      DIMENSION       KEYS(29), KEYO(29)
*
*     ------------------------------------------------------------------
*
      NCOPY  = MIN (MXDMCK-NOF2CK-2, NWKEY-NOF2CK-2)
      CALL UCOPY (KEYS(NSYSDX+1), KEYO(NOF2CK+3), NCOPY)
      KEYO(IDHKSN)     = KEYS(1)
      KEYO(IDHPTR)     = KEYS(2)
      KEYO(NOF1CK+1)   = KEYS(3)
      KEYO(NOF1CK+2)   = KEYS(4)
      KEYO(IDHUSI)     = KEYS(5)
      KEYO(IDHFLG)     = KEYS(6)
      KEYO(IDHINS)     = KEYS(7)
      CALL UCOPY (IOKYDX(NSYSDX+1), MASKDX(NOF2CK+3), MXDMCK-NOF2CK-2)
      MASKDX(IDHKSN)   = IOKYDX(1)
      MASKDX(IDHPTR)   = IOKYDX(2)
      MASKDX(NOF1CK+1) = IOKYDX(3)
      MASKDX(NOF1CK+2) = IOKYDX(4)
      MASKDX(IDHUSI)   = IOKYDX(5)
      MASKDX(IDHFLG)   = IOKYDX(6)
      MASKDX(IDHINS)   = IOKYDX(7)
*                                                             END DBCKEY
      END
