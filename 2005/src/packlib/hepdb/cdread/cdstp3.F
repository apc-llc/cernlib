*
* $Id: cdstp3.F,v 1.1.1.1 1996/02/28 16:24:27 mclareni Exp $
*
* $Log: cdstp3.F,v $
* Revision 1.1.1.1  1996/02/28 16:24:27  mclareni
* Hepdb, cdlib, etc
*
*
#include "hepdb/pilot.h"
#if defined(CERNLIB__P3CHILD)
      SUBROUTINE CDSTP3 (IFLG, LCOND, NBKP3, IND)
*     ===========================================
*
************************************************************************
*                                                                      *
*        SUBR. CDSTP3 (IFLG, LCOND, NBKP3, IND)                        *
*                                                                      *
*   Stores the several KYCD banks in a stack to be sorted out at the   *
*   end of the pass.                                                   *
*                                                                      *
*   Arguments :                                                        *
*                                                                      *
*     IFLG     Flag (1 for initialization; 2 for storage in stack)     *
*     LCOND    Condition for single or multiple retrieval              *
*     NBKP3    Object number restored in the host                      *
*     IND      Index for the keybank                                   *
*                                                                      *
*   Called by CDGETDB,CDUSE,  CDUSEDB,CDUSEM                           *
*                                                                      *
************************************************************************
*
#include "hepdb/cdcblk.inc"
#include "hepdb/p3dbl3.inc"
      LOGICAL         LCOND
      SAVE            NKBP3
*
*     ------------------------------------------------------------------
*
      IF (IFLG.EQ.1) THEN
*
*  **   Initialization
*
        IF (LCOND) THEN
          IPASP3 = 1
          NDIRP3 = 0
          NBKDP3 = 0
          NBKYP3 = 0
          INDXP3 = 1
        ELSE
          IPASP3 = 0
        ENDIF
        NKBP3  = 0
      ELSE
*
*  **   Storage
*
        NKBP3  = NKBP3 + 1
        IF (NKBP3.GT.IQ(KOFUCD+LNK2P3-1))
     +    CALL MZPUSH (IXDBP3, LNK2P3, 100, 100, '....')
        LQ(KOFUCD+LNK2P3-NKBP3) = LBKYCD
        IQ(KOFUCD+LNK2P3+NKBP3) = NBKP3 + IND*1000000
      ENDIF
*                                                             END CDSTP3
      END
#endif
