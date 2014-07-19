*
* $Id: cdkmld.F,v 1.1.1.1 1996/02/28 16:24:26 mclareni Exp $
*
* $Log: cdkmld.F,v $
* Revision 1.1.1.1  1996/02/28 16:24:26  mclareni
* Hepdb, cdlib, etc
*
*
#include "hepdb/pilot.h"
      SUBROUTINE CDKMLD (NK, KEYS)
*     ============================
*
************************************************************************
*                                                                      *
*        SUBR. CDKMLD (NK, KEYS)                                       *
*                                                                      *
*   Loads the key vector in KYVMCK in the context of M option          *
*                                                                      *
*   Arguments :                                                        *
*                                                                      *
*     NK       Occurence number of the object                          *
*     KEYS     Vector of keys                                          *
*                                                                      *
*   Called by CDFREE, CDKYDB                                           *
*                                                                      *
************************************************************************
*
#include "hepdb/ckkeys.inc"
#include "hepdb/cmulop.inc"
      DIMENSION       KEYS(9)
*
*     ------------------------------------------------------------------
*
      IF (NMULCM.GT.0)  THEN
*
*  **   For option 'M', find the corresponding key values
*
        CALL UCOPY (KEYS, KYVMCK, NSYSCK)
        DO 20 I = 1, NMULCM
          NK0    = NK
          NU     = KTYPCM(I)
          IF (I.NE.1)  THEN
            DO 10 J = 1, I-1
              NK0    = (NK0 - INDXCM(J)) / NOCCCM(J) + 1
   10       CONTINUE
          ENDIF
          NK0    = MOD (NK0, NOCCCM(I))
          IF (NK0.EQ.0) NK0 = NOCCCM(I)
          INDXCM(I)  = NK0
          KYVMCK(NU) = KEYS(LFKYCM(I)+NK0)
   20   CONTINUE
*
      ELSE
*
*  **   For standard option, copy the KEYS vector
*
        CALL UCOPY (KEYS, KYVMCK, NWKYCK)
*
      ENDIF
*                                                             END CDKMLD
      END
