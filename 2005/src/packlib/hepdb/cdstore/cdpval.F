*
* $Id: cdpval.F,v 1.1.1.1 1996/02/28 16:24:34 mclareni Exp $
*
* $Log: cdpval.F,v $
* Revision 1.1.1.1  1996/02/28 16:24:34  mclareni
* Hepdb, cdlib, etc
*
*
#include "hepdb/pilot.h"
      SUBROUTINE CDPVAL (KEYS)
*     ========================
*
************************************************************************
*                                                                      *
*        SUBR. CDPVAL (KEYS)                                           *
*                                                                      *
*   Sets the validity range of the partition (assumed to be in KYP2CK) *
*   from the current object                                            *
*                                                                      *
*   Arguments :                                                        *
*                                                                      *
*     KEYS     Vector of keys for the current object                   *
*                                                                      *
*   Called by CDDONT, CDENFZ, CDENTB, CDKOUT, CDPART, CDRTFZ           *
*                                                                      *
************************************************************************
*
#include "hepdb/cdcblk.inc"
#include "hepdb/ckkeys.inc"
      DIMENSION       KEYS(9)
*     ------------------------------------------------------------------
*
* *** For hierarchial validity range keys
*
      IF (IHFLCD.EQ.0) THEN
*
* ***   Decide if the partitition keys to be updated
*
        IBEG   = 0
        IEND   = 0
        DO 10 I = 1, NPARCD
          IF (IBEG.EQ.0) THEN
            IF (KEYS(NOF1CK+2*I-1).LT.KYP2CK(NOF1CK+2*I-1)) THEN
              IBEG   =  1
            ELSE IF (KEYS(NOF1CK+2*I-1).GT.KYP2CK(NOF1CK+2*I-1)) THEN
              IBEG   = -1
            ENDIF
          ENDIF
          IF (IEND.EQ.0) THEN
            IF (KEYS(NOF1CK+2*I)  .GT.KYP2CK(NOF1CK+2*I)  ) THEN
              IEND   =  1
            ELSE IF (KEYS(NOF1CK+2*I)  .GT.KYP2CK(NOF1CK+2*I)  ) THEN
              IEND   = -1
            ENDIF
          ENDIF
   10   CONTINUE
*
* ***   Take action now
*
        IF (IBEG.GT.0) THEN
          DO 15 I = 1, NPARCD
            KYP2CK(NOF1CK+2*I-1) = KEYS(NOF1CK+2*I-1)
   15     CONTINUE
        ENDIF
        IF (IEND.GT.0) THEN
          DO 20 I = 1, NPARCD
            KYP2CK(NOF1CK+2*I)   = KEYS(NOF1CK+2*I)
   20     CONTINUE
        ENDIF
*
      ELSE
*
* ***   For flat validity range keys
*
        DO 30 I = 1, NPARCD
          IF (KEYS(NOF1CK+2*I-1).LT.KYP2CK(NOF1CK+2*I-1))
     +      KYP2CK(NOF1CK+2*I-1) = KEYS(NOF1CK+2*I-1)
          IF (KEYS(NOF1CK+2*I)  .GT.KYP2CK(NOF1CK+2*I)  )
     +      KYP2CK(NOF1CK+2*I)   = KEYS(NOF1CK+2*I)
   30   CONTINUE
      ENDIF
*                                                             END CDPVAL
      END
