*
* $Id: dbkvin.F,v 1.1.1.1 1996/02/28 16:24:59 mclareni Exp $
*
* $Log: dbkvin.F,v $
* Revision 1.1.1.1  1996/02/28 16:24:59  mclareni
* Hepdb, cdlib, etc
*
*
#include "hepdb/pilot.h"
      SUBROUTINE DBKVIN (PATHN, ITIME, USER, NDAT, IDTYP, IPRVS, NWKEY,
     +                   KEY, IPREC, CHOPT)
*     =================================================================
*
************************************************************************
*                                                                      *
*        SUBR. DBKVIN (PATHN, ITIME, USER*, *NDAT*, IDTYP*, IPRVS,     *
*                      NWKEY*, *KEY*, IPREC*, CHOPT)                   *
*                                                                      *
*   Fetches from disk to a FORTRAN array data valid for a given time   *
*                                                                      *
*   Restrictions : USER should contain variables of the same type      *
*                  (Integer, Real or Holllereith)                      *
*                  One should be very careful in using this routine.   *
*                  The use of character option should be handled with  *
*                  caution.                                            *
*                                                                      *
*   Arguments :                                                        *
*                                                                      *
*     PATHN    Character string describing the pathname                *
*     ITIME    Time for which data are required to be valid            *
*     USER     User array where data are stored                        *
*     NDAT     Maximum dimension of USER array (on input)              *
*              Number of data words fetched from disk (on output)      *
*     IDTYP    Type of the data (2 - integer; 3 - real; 5 - Hollereith)*
*     IPRVS    Program version number for selecting the data           *
*              (if 0 no selection on program version number to be made)*
*     NWKEY    Length of the key vector                                *
*     KEY      Key vector elements (If any User key or key 5 is nonzero*
*              at input, data are retrieved according to that key      *
*              content; on output it contains key elements for the     *
*              data)                                                   *
*     IPREC    Precision word; (If IPREC > 0, data are stored with     *
*              IPREC significant digits right to the decimal points; if*
*              IPREC < 0, data are stored with IPREC insignificant     *
*              digits leftto the decimal point.)                       *
*     CHOPT    Character string with any of the following characters   *
*          N   If data do not exist for ITIME, take the nearest data   *
*              object in time                                          *
*                                                                      *
*   Called by user                                                     *
*                                                                      *
*   Error Condition :                                                  *
*                                                                      *
*     IQUEST(1) =  0 : No error                                        *
*               = 31 : Illegal path name                               *
*               = 32 : No keys/data in this directory                  *
*               = 36 : Data bank address zero on return from DBKXIN    *
*               = 37 : Insufficient space in USER store array          *
*                                                                      *
************************************************************************
*
#include "hepdb/caopts.inc"
#include "hepdb/cdcblk.inc"
#include "hepdb/cinitl.inc"
#include "hepdb/ckkeys.inc"
#include "dxused.inc"
      DIMENSION       KEY(9), USER(2)
      CHARACTER       CHOPT*(*), PATHN*(*), CHOPF*28
*
*     ------------------------------------------------------------------
*
* *** Reformat option
*
      CALL DBOPTS (CHOPT, IRC)
      IF (IRC.NE.0) THEN
        IQUEST(1) = IRC
        GO TO 999
      ENDIF
      IOPFDX = 1
      CALL DBOPTM (CHOPF)
*
      CALL DBCKEY (KEY, KEYSDX, MXKYDX)
      IF (IPRVS.NE.0) THEN
        KEYSDX(IDHUSI) = IPRVS
        MASKDX(IDHUSI) = 1
      ENDIF
      DO 10 I = NOF2CK+3, MXDMCK
        IF (KEYSDX(I).NE.0) MASKDX(I) = 1
   10 CONTINUE
*
* *** Read in the data
*
      LKEYDX = 0
      CALL CDUSEM (PATHN, LKEYDX, ITIME, MASKDX, KEYSDX, CHOPF, IRC)
      IF (IRC.NE.0) THEN
        IQUEST(1) = IRC
        GO TO 999
      ELSE IF (LKEYDX.EQ.0.OR.LQ(KOFUCD+LKEYDX-KLDACD).EQ.0) THEN
        IQUEST(1) = 36
        GO TO 999
      ENDIF
*
      LBN    = LQ(KOFUCD+LKEYDX-KLNOCD)
      NDK    = IQ(KOFUCD+LKEYDX-1)
      NWKEY  = IQ(KOFUCD+LBN+MNDNWK)
      IPREC  = IQ(KOFUCD+LKEYDX+NDK+MKYPRE)
      CALL UCOPY  (IQ(KOFUCD+LKEYDX+1), KEY, NWKEY)
      LDATDX = LQ(KOFUCD+LKEYDX-KLDACD)
*
*  ** See if the USER array size is sufficient to store the data
*
      ND     = IQ(KOFUCD+LDATDX-1)
      IF (ND.GT.NDAT)  THEN
*
*  *    Insufficient space
*
        CALL MZDROP (IDIVCD, LDATDX, 'L')
        IQUEST(1) = 37
        IQUEST(11)= NDAT
        IQUEST(12)= ND
        NDAT      = 0
#if defined(CERNLIB__DEBUG)
        IF (IDEBCD.GT.0) THEN
          CALL CDPRNT (LPRTCD, '(/,'' DBKVIN : Insufficient space'//
     +         ' '',I10,'' to store data - a minimum of '',I10,'' '//
     +         'storage is needed'')', IQUEST(11), 2)
        ENDIF
#endif
*
      ELSE
*
*  *    Every thing is OK
*
        IDTYP     = ICDTYP(LDATDX)
        CALL UCOPY (Q(KOFUCD+LDATDX+1), USER(1), ND)
        NDAT      = ND
        CALL MZDROP (IDIVCD, LDATDX, 'L')
        IQUEST(1) = 0
*
      ENDIF
*                                                             END DBKVIN
  999 END
