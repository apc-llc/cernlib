*
* $Id: cdksel.F,v 1.1.1.1 1996/02/28 16:24:26 mclareni Exp $
*
* $Log: cdksel.F,v $
* Revision 1.1.1.1  1996/02/28 16:24:26  mclareni
* Hepdb, cdlib, etc
*
*
#include "hepdb/pilot.h"
      SUBROUTINE CDKSEL (ITIME, KEYS, KEYV, IFLG, ISEL, INBR)
*     =======================================================
*
************************************************************************
*                                                                      *
*        SUBR. CDKSEL (ITIME, KEYS, KEYV, IFLG, ISEL*, INBR*)          *
*                                                                      *
*   Select an object depending on the options                          *
*                                                                      *
*   Arguments :                                                        *
*                                                                      *
*     ITIME    Vector with the selection time                          *
*     KEYS     User key vector for selection                           *
*     KEYV     Key vector of the partition                             *
*     IFLG     0 if selection required only; -1 if logical end of      *
*              validity to be done; 1 if also neighbours needed;       *
*              99 no selection on range of validity                    *
*     ISEL     0 if the object is selected; 1 if not                   *
*     INBR     0 if the object is the nearest neighbour; 1 if not      *
*                                                                      *
*     Called by CDCHKY, CDGETDB,CDKYSE, CDSEKY                         *
*                                                                      *
************************************************************************
*
#include "hepdb/caopts.inc"
#include "hepdb/cdcblk.inc"
#include "hepdb/ckkeys.inc"
#include "hepdb/ctkxin.inc"
      DIMENSION       ITIME(10), KEYS(10), KEYV(10), ICLOS(MXPACD)
#include "zebra/q_jbit.inc"
* Ignoring t=pass
*     ------------------------------------------------------------------
*
* *** Decide if the partitition keys to be updated
*
      ISEL   = 1
      INBR   = 1
*
*     Selection on insertion time
*
      IF (IOKYCA(IDHINS).NE.0.AND.KEYV(IDHINS).GT.KEYS(IDHINS))GO TO 999
*
*     Selection on 'experiment' keys
*
      IF (IOKYCA(IDHUSI).NE.0.AND.KEYV(IDHUSI).NE.KEYS(IDHUSI))GO TO 999
      IF (IOKYCA(IDHSRN).NE.0.AND.KEYV(IDHSRN).NE.KEYS(IDHSRN))GO TO 999
      IF (IOKYCA(IDHEX1).NE.0.AND.KEYV(IDHEX1).NE.KEYS(IDHEX1))GO TO 999
      IF (IOKYCA(IDHEX2).NE.0.AND.KEYV(IDHEX2).NE.KEYS(IDHEX2))GO TO 999
      IF (IOKYCA(IDHEX3).NE.0.AND.KEYV(IDHEX3).NE.KEYS(IDHEX3))GO TO 999
*
      IF (MXINCD.GT.0        .AND.KEYV(IDHINS).GT.MXINCD)      GO TO 999
*
      IF (NWKYCK.GT.NSYSCK)  THEN
        DO 10 J = NSYSCK+1, NWKYCK
          IF (IOKYCA(J).NE.0.AND.KEYV(J).NE.KEYS(J))           GO TO 999
   10   CONTINUE
      ENDIF
      IF (IFLG.EQ.99)                                          GO TO 100
*
      IF (IPRBCA.EQ.0.AND.IPRECA.EQ.0) THEN
*
        IBEG   = 0
        IEND   = 0
        DO 15 I = 1, NPARCD
          IF (IHFLCD.EQ.0) THEN
            IF (IBEG.EQ.0) THEN
              IF (ITIME(I).LT.KEYV(NOF1CK+2*I-1))              IBEG =-I
              IF (ITIME(I).GT.KEYV(NOF1CK+2*I-1))              IBEG = I
            ENDIF
            IF (IEND.EQ.0) THEN
              IF (ITIME(I).GT.KEYV(NOF1CK+2*I))                IEND =-I
              IF (ITIME(I).LT.KEYV(NOF1CK+2*I))                IEND = I
            ENDIF
          ELSE
            IF (IBEG.EQ.0) THEN
              IF (ITIME(I).LT.KEYV(NOF1CK+2*I-1))              IBEG =-I
            ENDIF
            IF (IEND.EQ.0) THEN
              IF (ITIME(I).GT.KEYV(NOF1CK+2*I))                IEND =-I
            ENDIF
          ENDIF
   15   CONTINUE
        IF (IFLG.EQ.0.AND.(IBEG.LT.0.OR.IEND.LT.0))            GO TO 999
*
*  **   If satisfied on end-validity criteria, use it for determining
*  **   logical end-validity
*
        IF (IBEG.LT.0.AND.IEND.GE.0) THEN
          IF (IHFLCD.EQ.0) THEN
            KYEN   = 0
            DO 20 I = 1, NPARCD
              IF (KYEN.EQ.0) THEN
                IF (KYENCD(I).LT.KEYV(NOF1CK+2*I-1))           KYEN =-1
                IF (KYENCD(I).GT.KEYV(NOF1CK+2*I-1))           KYEN = 1
              ENDIF
   20       CONTINUE
            IF (KYEN.GE.0) THEN
              DO 25 I = 1, NPARCD
                KYENCD(I) = KEYV(NOF1CK+2*I-1)
   25         CONTINUE
            ENDIF
          ELSE
            DO 30 I = 1, NPARCD
              IF (KYENCD(I).GT.KEYV(NOF1CK+2*I-1)) THEN
                KYENCD(I) = KEYV(NOF1CK+2*I-1)
              ENDIF
   30       CONTINUE
          ENDIF
        ENDIF
        IF (IFLG.LT.0.AND.(IBEG.LT.0.OR.IEND.LT.0))            GO TO 999
*
*  **   Closest neighbour if not selected
*
        IF (IBEG.LT.0.OR.IEND.LT.0) THEN
          IF (JBIT(KEYNCK(IDHFLG),JIGNCD).NE.0)                GO TO 999
          IF (IBEG.LT.0) THEN
            IBG1   = -IBEG
          ELSE
            IBG1   = MXPACD
          ENDIF
          IF (IEND.LT.0) THEN
            IEN1   = -IEND
          ELSE
            IEN1   = MXPACD
          ENDIF
          DO 35 I = 1, MIN(IBG1,IEN1)
            ICLOS(I) = 0
   35     CONTINUE
          IF (IBG1.GT.IEN1)      THEN
            ICLOS(IEN1) = ITIME(IEN1) - KEYV(NOF1CK+2*IEN1)
          ELSE IF (IBG1.LT.IEN1) THEN
            ICLOS(IBG1) = KEYV(NOF1CK+2*IBG1-1) - ITIME(IBG1)
          ELSE
            ICLOS(IBG1) = MIN ((KEYV(NOF1CK+2*IBG1-1) - ITIME(IBG1)),
     +                         (ITIME(IEN1) - KEYV(NOF1CK+2*IEN1))  )
          ENDIF
          DO 40 I = 1, NPARCD
            IF (ICLOS(I).LT.INRSCT(I)) INBR   = 0
   40     CONTINUE
          IF (INBR.EQ.0) THEN
            DO 45 I = 1, NPARCD
              INRSCT(I) = ICLOS(I)
   45       CONTINUE
          ENDIF
          GO TO 999
        ENDIF
*
      ELSE IF (IPRBCA.GT.0.AND.IPRECA.EQ.0) THEN
*
        IF (IHFLCD.EQ.0) THEN
          IBEG   = 0
          DO 50 I = 1, NPARCD
            IF (IBEG.EQ.0) THEN
              IF (KEYV(NOF1CK+2*I-1).GT.KEYS(NOF1CK+2*I-1))    GO TO 999
              IF (KEYV(NOF1CK+2*I-1).LT.KEYS(NOF1CK+2*I-1))    IBEG = 1
            ENDIF
   50     CONTINUE
        ELSE
          DO 55 I = 1, NPARCD
            IF (KEYV(NOF1CK+2*I-1).GT.KEYS(NOF1CK+2*I-1))      GO TO 999
   55     CONTINUE
        ENDIF
*
      ELSE IF (IPRBCA.EQ.0.AND.IPRECA.GT.0) THEN
*
        IF (IHFLCD.EQ.0) THEN
          IEND   = 0
          DO 60 I = 1, NPARCD
            IF (IEND.EQ.0) THEN
              IF (KEYV(NOF1CK+2*I)  .LT.KEYS(NOF1CK+2*I))      GO TO 999
              IF (KEYV(NOF1CK+2*I)  .GT.KEYS(NOF1CK+2*I))      IEND = 1
            ENDIF
   60     CONTINUE
        ELSE
          DO 65 I = 1, NPARCD
            IF (KEYV(NOF1CK+2*I)  .LT.KEYS(NOF1CK+2*I))        GO TO 999
   65     CONTINUE
        ENDIF
*
      ELSE IF (IPRBCA.GT.0.AND.IPRECA.GT.0) THEN
*
        IF (IHFLCD.EQ.0) THEN
          IBEG   = 0
          IEND   = 0
          DO 70 I = 1, NPARCD
            IF (IBEG.EQ.0) THEN
              IF (KEYV(NOF1CK+2*I-1).GT.KEYS(NOF1CK+2*I-1))    GO TO 999
              IF (KEYV(NOF1CK+2*I-1).LT.KEYS(NOF1CK+2*I-1))    IBEG = 1
            ENDIF
            IF (IEND.EQ.0) THEN
              IF (KEYV(NOF1CK+2*I)  .LT.KEYS(NOF1CK+2*I))      GO TO 999
              IF (KEYV(NOF1CK+2*I)  .GT.KEYS(NOF1CK+2*I))      IEND = 1
            ENDIF
   70     CONTINUE
        ELSE
          DO 75 I = 1, NPARCD
            IF (KEYV(NOF1CK+2*I-1).GT.KEYS(NOF1CK+2*I-1))      GO TO 999
            IF (KEYV(NOF1CK+2*I)  .LT.KEYS(NOF1CK+2*I))        GO TO 999
   75     CONTINUE
        ENDIF
*
      ELSE
*
        IF (IHFLCD.EQ.0) THEN
          IBEG   = 0
          IEND   = 0
          DO 80 I = 1, NPARCD
            IF (IBEG.EQ.0.AND.IPRBCA.NE.0) THEN
              IF (KEYV(NOF1CK+2*I-1).LT.KEYS(NOF1CK+2*I-1))    GO TO 999
              IF (KEYV(NOF1CK+2*I-1).GT.KEYS(NOF1CK+2*I-1))    IBEG = 1
            ENDIF
            IF (IEND.EQ.0.AND.IPRECA.NE.0) THEN
              IF (KEYV(NOF1CK+2*I-1).GT.KEYS(NOF1CK+2*I))      GO TO 999
              IF (KEYV(NOF1CK+2*I-1).LT.KEYS(NOF1CK+2*I))      IEND = 1
            ENDIF
   80     CONTINUE
        ELSE
          DO 85 I = 1, NPARCD
            IF (IPRBCA.NE.0) THEN
              IF (KEYV(NOF1CK+2*I-1).LT.KEYS(NOF1CK+2*I-1))    GO TO 999
            ENDIF
            IF (IPRECA.NE.0) THEN
              IF (KEYV(NOF1CK+2*I-1).GT.KEYS(NOF1CK+2*I))      GO TO 999
            ENDIF
   85     CONTINUE
        ENDIF
      ENDIF
*
  100 ISEL   = 0
*                                                             END CDKSEL
  999 END
