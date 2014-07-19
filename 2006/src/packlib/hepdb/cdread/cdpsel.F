*
* $Id: cdpsel.F,v 1.1.1.1 1996/02/28 16:24:27 mclareni Exp $
*
* $Log: cdpsel.F,v $
* Revision 1.1.1.1  1996/02/28 16:24:27  mclareni
* Hepdb, cdlib, etc
*
*
#include "hepdb/pilot.h"
      SUBROUTINE CDPSEL (ITIME, KEYS, KEYP, IFLG, ISEL)
*     =================================================
*
************************************************************************
*                                                                      *
*        SUBR. CDPSEL (ITIME, KEYS, KEYP, IFLG, ISEL*)                 *
*                                                                      *
*   Select a partitition depending on the options                      *
*                                                                      *
*   Arguments :                                                        *
*                                                                      *
*     ITIME    Vector with the selection time                          *
*     KEYS     User key vector for selection                           *
*     KEYP     Key vector of the partition                             *
*     IFLG     0 selection on range of validity;                       *
*              99 no selection on range of validity                    *
*     ISEL     0 if the partition is selected; 1 if not                *
*                                                                      *
*   Called by CDBKKS, CDCHCK, CDGETDB,CDKXIN                           *
*                                                                      *
************************************************************************
*
#include "hepdb/caopts.inc"
#include "hepdb/cdcblk.inc"
#include "hepdb/ckkeys.inc"
      DIMENSION       ITIME(9), KEYS(9), KEYP(9)
*     ------------------------------------------------------------------
*
* *** Decide if the partition keys to be updated
*
      ISEL   = 1
      IF (IOKYCA(IDHINS).NE.0.AND.KEYP(IDHINS).GT.KEYS(IDHINS))GO TO 999
      IF (MXINCD.GT.0        .AND.KEYP(IDHINS).GT.MXINCD)      GO TO 999
      IF (IFLG.EQ.99)                                          GO TO 100
      IF (IPRBCA.EQ.0.AND.IPRECA.EQ.0) THEN
*
        IBEG   = 0
        IF (IHFLCD.EQ.0) THEN
          IEND   = 0
          DO 10 I = 1, NPARCD
            IF (IBEG.EQ.0) THEN
              IF (ITIME(I).LT.KEYP(NOF1CK+2*I-1))              IBEG =-1
              IF (ITIME(I).GT.KEYP(NOF1CK+2*I-1))              IBEG = 1
            ENDIF
            IF (IEND.EQ.0) THEN
              IF (ITIME(I).GT.KEYP(NOF1CK+2*I))                GO TO 999
              IF (ITIME(I).LT.KEYP(NOF1CK+2*I))                IEND = 1
            ENDIF
   10     CONTINUE
        ELSE
          DO 15 I = 1, NPARCD
            IF (ITIME(I).LT.KEYP(NOF1CK+2*I-1))                IBEG =-1
            IF (ITIME(I).GT.KEYP(NOF1CK+2*I))                  GO TO 999
   15     CONTINUE
        ENDIF
*
        IF (IBEG.LT.0) THEN
          IF (IHFLCD.EQ.0) THEN
            KYEN   = 0
            DO 20 I = 1, NPARCD
              IF (KYEN.EQ.0) THEN
                IF (KYENCD(I).LT.KEYP(NOF1CK+2*I-1))           GO TO 999
                IF (KYENCD(I).GT.KEYP(NOF1CK+2*I-1))           KYEN = 1
              ENDIF
   20       CONTINUE
          ELSE
            DO 25 I = 1, NPARCD
              IF (KYENCD(I).LT.KEYP(NOF1CK+2*I-1))             GO TO 999
   25       CONTINUE
          ENDIF
          DO 30 I = 1, NPARCD
            KYENCD(I) = KEYP(NOF1CK+2*I-1)
   30     CONTINUE
          GO TO 999
        ENDIF
*
      ELSE IF (IPRBCA.GT.0.AND.IPRECA.EQ.0) THEN
*
        IF (IHFLCD.EQ.0) THEN
          IBEG   = 0
          DO 35 I = 1, NPARCD
            IF (IBEG.EQ.0) THEN
              IF (KEYP(NOF1CK+2*I-1).GT.KEYS(NOF1CK+2*I-1))    GO TO 999
              IF (KEYP(NOF1CK+2*I-1).LT.KEYS(NOF1CK+2*I-1))    IBEG = 1
            ENDIF
   35     CONTINUE
        ELSE
          DO 40 I = 1, NPARCD
            IF (KEYP(NOF1CK+2*I-1).GT.KEYS(NOF1CK+2*I-1))      GO TO 999
   40     CONTINUE
        ENDIF
*
      ELSE IF (IPRBCA.EQ.0.AND.IPRECA.GT.0) THEN
*
        IF (IHFLCD.EQ.0) THEN
          IEND   = 0
          DO 45 I = 1, NPARCD
            IF (IEND.EQ.0) THEN
              IF (KEYP(NOF1CK+2*I)  .LT.KEYS(NOF1CK+2*I))      GO TO 999
              IF (KEYP(NOF1CK+2*I)  .GT.KEYS(NOF1CK+2*I))      IEND = 1
            ENDIF
   45     CONTINUE
        ELSE
          DO 50 I = 1, NPARCD
            IF (KEYP(NOF1CK+2*I)  .LT.KEYS(NOF1CK+2*I))        GO TO 999
   50     CONTINUE
        ENDIF
*
      ELSE IF (IPRBCA.GT.0.AND.IPRECA.GT.0) THEN
*
        IF (IHFLCD.EQ.0) THEN
          IBEG   = 0
          IEND   = 0
          DO 55 I = 1, NPARCD
            IF (IBEG.EQ.0) THEN
              IF (KEYP(NOF1CK+2*I-1).GT.KEYS(NOF1CK+2*I-1))    GO TO 999
              IF (KEYP(NOF1CK+2*I-1).LT.KEYS(NOF1CK+2*I-1))    IBEG = 1
            ENDIF
            IF (IEND.EQ.0) THEN
              IF (KEYP(NOF1CK+2*I)  .LT.KEYS(NOF1CK+2*I))      GO TO 999
              IF (KEYP(NOF1CK+2*I)  .GT.KEYS(NOF1CK+2*I))      IEND = 1
            ENDIF
   55     CONTINUE
        ELSE
          DO 60 I = 1, NPARCD
            IF (KEYP(NOF1CK+2*I-1).GT.KEYS(NOF1CK+2*I-1))      GO TO 999
            IF (KEYP(NOF1CK+2*I)  .LT.KEYS(NOF1CK+2*I))        GO TO 999
   60     CONTINUE
        ENDIF
*
      ELSE
*
        IF (IHFLCD.EQ.0) THEN
          IBEG   = 0
          IEND   = 0
          DO 65 I = 1, NPARCD
            IF (IBEG.EQ.0.AND.IPRBCA.NE.0) THEN
              IF (KEYP(NOF1CK+2*I)  .LT.KEYS(NOF1CK+2*I-1))    GO TO 999
              IF (KEYP(NOF1CK+2*I)  .GT.KEYS(NOF1CK+2*I-1))    IBEG = 1
            ENDIF
            IF (IEND.EQ.0.AND.IPRECA.NE.0) THEN
              IF (KEYP(NOF1CK+2*I-1).GT.KEYS(NOF1CK+2*I))      GO TO 999
              IF (KEYP(NOF1CK+2*I-1).LT.KEYS(NOF1CK+2*I))      IEND = 1
            ENDIF
   65     CONTINUE
        ELSE
          DO 70 I = 1, NPARCD
            IF (IPRBCA.NE.0) THEN
              IF (KEYP(NOF1CK+2*I)  .LT.KEYS(NOF1CK+2*I-1))    GO TO 999
            ENDIF
            IF (IPRECA.NE.0) THEN
              IF (KEYP(NOF1CK+2*I-1).GT.KEYS(NOF1CK+2*I))      GO TO 999
            ENDIF
   70     CONTINUE
        ENDIF
*
      ENDIF
*
  100 ISEL   = 0
*                                                             END CDPSEL
  999 END
