*
* $Id: cdstom.F,v 1.1.1.1 1996/02/28 16:24:34 mclareni Exp $
*
* $Log: cdstom.F,v $
* Revision 1.1.1.1  1996/02/28 16:24:34  mclareni
* Hepdb, cdlib, etc
*
*
#include "hepdb/pilot.h"
      SUBROUTINE CDSTOM (PATHN, LADDR, LKYBK, IUDIV, NWDIM, NOBJ, KEYO,
     +                   KEYN, CHOPT, IRC)
*     =================================================================
*
************************************************************************
*                                                                      *
*        SUBR. CDSTOM (PATHN, LADDR, *LKYBK*, IUDIV, NWDIM, NOBJ, KEYO,*
*                      KEYN, CHOPT, IRC*)                              *
*                                                                      *
*   Stores data from memory to disk and also enters in the memory ala  *
*   DBUSE in NODE/KEY structure                                        *
*                                                                      *
*   Arguments :                                                        *
*                                                                      *
*     PATHN    Character string describing the pathname                *
*     LADDR    Vector of NOBJ bank addresses                           *
*     LKYBK    Address of the first Key bank KYCD   (INPUT or OUTPUT)  *
*     IUDIV    Division index of the user data bank                    *
*     NWDIM    First dimension of the array KEY                        *
*     NOBJ     Number of objects to be inserted. The key vectors KEYO  *
*              and KEYN must be dimensioned (NWDIM,NOBJ)               *
*     KEYO     Vector/matrix of old keys                               *
*     KEYN     Vector/matrix of new keys                               *
*     CHOPT    Character string with any of the following characters   *
*          A   Trust LBK address if non-zero                           *
*          B   Save in the special backup file; not in standard Journal*
*          C   Create Node/Key data structure ala DBUSE                *
*          D   Store only the differences from an existing object      *
*          F   Updates with a fully matched data object (in user keys) *
*          K   Store data only inside the keys (not yet installed)     *
*          H   Insertion time as supplied by user to be honoured       *
*          P   Store data compressed (bit packing)                     *
*          R   Replace existing object as specified by the vector KEYO *
*          T   Special text type of data (to be used with R)           *
*          Y   Store with full RZ option (No compression to be made)   *
*          Z   Store only nonzero elements. An element is considered to*
*              to be zero if its absolute value is less than DELTA     *
*     IRC      Return code (see below)                                 *
*                                                                      *
*   Called by user                                                     *
*                                                                      *
*   Error Condition :                                                  *
*                                                                      *
*     IRC       =  0 : No error                                        *
*               = 62 : Illegal character option                        *
*               = 65 : Illegal number of objects                       *
*                                                                      *
************************************************************************
*
#include "hepdb/cdcblk.inc"
#include "hepdb/ctpath.inc"
      DIMENSION       LADDR(9), LKYBK(9), KEYO(NWDIM,2), KEYN(NWDIM,2)
      CHARACTER       CHOPT*(*), PATHN*(*)
*
*     ------------------------------------------------------------------
*
* *** Branch according to NOBJ and R option
*
      IOPR   = INDEX (CHOPT, 'R')
      IOPC   = INDEX (CHOPT, 'C')
*
      IF (NOBJ.EQ.1) THEN
*
        IF (IOPR.NE.0) THEN
          CALL CDREPL (PATHN, LADDR,LKYBK, IUDIV, KEYO,KEYN, CHOPT, IRC)
        ELSE
          CALL CDSTOR (PATHN, LADDR,LKYBK, IUDIV,      KEYN, CHOPT, IRC)
        ENDIF
*
      ELSE IF (NOBJ.GT.1) THEN
*
        IF (IOPR.EQ.0.AND.IOPC.EQ.0) THEN
          CALL CDENTB (PATHN, LADDR,IUDIV, NWDIM,NOBJ, KEYN, CHOPT, IRC)
        ELSE
          IRC    = 62
#if defined(CERNLIB__DEBUG)
          IF (IDEBCD.GT.0) THEN
            TOP1CT = CHOPT
            CALL CDPRNT (LPRTCD, '(/,'' CDSTOM : Character option '//
     +           TOP1CT//' incompatible with NOBJ '',I12)', NOBJ, 1)
          ENDIF
#endif
        ENDIF
*
      ELSE
*
        IRC     = 65
#if defined(CERNLIB__DEBUG)
        IF (IDEBCD.GT.0) CALL CDPRNT (LPRTCD, '(/,'' CDSTOM : Too few'//
     +     ' objects '',I12)', NOBJ, 1)
#endif
      ENDIF
*                                                             END CDSTOM
      END
