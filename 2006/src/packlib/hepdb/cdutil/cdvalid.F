*
* $Id: cdvalid.F,v 1.1.1.1 1996/02/28 16:24:42 mclareni Exp $
*
* $Log: cdvalid.F,v $
* Revision 1.1.1.1  1996/02/28 16:24:42  mclareni
* Hepdb, cdlib, etc
*
*
#include "hepdb/pilot.h"
      SUBROUTINE CDVALID (IVECT, IRC)
*     ===============================
*
************************************************************************
*                                                                      *
*        SUBR. CDVALID (IVECT*, IRC*)                                  *
*                                                                      *
*   Finds the overlapping validity range of all data base objects      *
*   used since the last call to CDINFO.                                *
*                                                                      *
*   Arguments :                                                        *
*                                                                      *
*     IVECT    Vector returning the minimum and maximum values for the *
*              key pairs since the last call to CDINFO                 *
*     IRC      Return code (see below)                                 *
*                                                                      *
*   Called by user                                                     *
*                                                                      *
*   Error Condition :                                                  *
*                                                                      *
*     IRC       =  0 : No error                                        *
*               = 94 : Non-matching NPAR in the different UPCD banks   *
*               = 97 : No UPCD bank has been found                     *
*                                                                      *
************************************************************************
*
#include "hepdb/cdcblk.inc"
#include "hepdb/ckkeys.inc"
      PARAMETER       (NLEVM=20)
      DIMENSION       ISDI(NLEVM), NSDI(NLEVM), IVECT(9)
*
*     ------------------------------------------------------------------
*
      NPARCD = -1
      IF (LTOPCD.EQ.0) THEN
        IRC    = 97
#if defined(CERNLIB__DEBUG)
        IF (IDEBCD.GT.0) CALL CDPRNT (LPRTCD, '(/,'' CDVALID : Cannot'//
     +  ' find the UPCD bank '')', IARGCD, 0)
#endif
        GO TO 999
      ELSE
        IRC    = 0
      ENDIF
*
* *** Loop over all top directories
*
      LBUPCD = LTOPCD
   10 IF (LBUPCD.GT.0) THEN
        IF (NPARCD.LT.0) THEN
          NPARCD = IQ(KOFUCD+LBUPCD+MUPAIR)
          IHFLCD = IQ(KOFUCD+LBUPCD+MUPHFL)
          DO 15 I = 1, NPARCD
            IVECT(2*I-1) = 0
            IVECT(2*I)   = IBIGCD
   15     CONTINUE
        ELSE IF (NPARCD.NE.IQ(KOFUCD+LBUPCD+MUPAIR)) THEN
          IRC    = 94
#if defined(CERNLIB__DEBUG)
          IF (IDEBCD.GT.0) THEN
            IARGCD(1) = NPARCD
            IARGCD(2) = IQ(KOFUCD+LBUPCD+MUPAIR)
            CALL CDPRNT (LPRTCD, '(/,'' CDVALID : Mismatching Npair'//
     +          ' '',2I8)', IARGCD, 2)
          ENDIF
#endif
          GO TO 999
        ENDIF
        NLEV   = 0
        LBNOCD = LQ(KOFUCD+LBUPCD-1)
*
*  **   Scan down the nodes to find all the subdirectories
*
   20   IF (LBNOCD.GT.0) THEN
          NLEV   = NLEV + 1
          ISDI(NLEV) = 0
          NSDI(NLEV) = IQ(KOFUCD+LBNOCD-2)
*
   25     ISDI(NLEV) = ISDI(NLEV) + 1
          IF (ISDI(NLEV).LE.NSDI(NLEV)) THEN
*
*  **       If a new subdirectory go down one level
*
            LBD    = LQ(KOFUCD+LBNOCD-ISDI(NLEV))
            IF (LBD.GT.0) THEN
              LBNOCD = LBD
              GO TO 20
            ELSE
              GO TO 25
            ENDIF
*
          ELSE
*
*  **       Loop over all the key banks
*
            NDK    = IQ(KOFUCD+LBNOCD+MNDNWD)
            LBKYCD = LQ(KOFUCD+LBNOCD)
   30       IF (LBKYCD.GT.0) THEN
              IOFF   = KOFUCD + LBKYCD + NOF1CK
              IF (IQ(KOFUCD+LBKYCD+NDK+MKYCEV).GT.0) THEN
                IF (IHFLCD.EQ.0) THEN
                  IBEG   = 0
                  IEND   = 0
                  DO 35 I = 1, NPARCD
                    IF (IBEG.EQ.0) THEN
                      IF (IQ(IOFF+2*I-1).LT.IVECT(2*I-1)) IBEG =-I
                      IF (IQ(IOFF+2*I-1).GT.IVECT(2*I-1)) IBEG = I
                    ENDIF
                    IF (IEND.EQ.0) THEN
                      IF (IQ(IOFF+2*I)  .GT.IVECT(2*I))   IEND =-I
                      IF (IQ(IOFF+2*I)  .LT.IVECT(2*I))   IEND = I
                    ENDIF
   35             CONTINUE
                  DO 40 I = 1, NPARCD
                    IF (IBEG.GT.0) IVECT(2*I-1) = IQ(IOFF+2*I-1)
                    IF (IEND.GT.0) IVECT(2*I)   = IQ(IOFF+2*I)
   40             CONTINUE
                ELSE
                  DO 45 I = 1, NPARCD
                    IF (IQ(IOFF+2*I-1).GT.IVECT(2*I-1))
     +                             IVECT(2*I-1) = IQ(IOFF+2*I-1)
                    IF (IQ(IOFF+2*I)  .LT.IVECT(2*I))
     +                             IVECT(2*I)   = IQ(IOFF+2*I)
   45             CONTINUE
                ENDIF
              ENDIF
              LBKYCD = LQ(KOFUCD+LBKYCD)
              GO TO 30
            ENDIF
*
*  **       Now go up one level
*
   50       NLEV   = NLEV - 1
            IF (NLEV.GT.0) THEN
              LBNOCD = LQ(KOFUCD+LBNOCD+1)
              GO TO 20
            ENDIF
          ENDIF
        ENDIF
*
        LBUPCD = LQ(KOFUCD+LBUPCD)
        GO TO 10
      ENDIF
*                                                            END CDVALID
  999 END
