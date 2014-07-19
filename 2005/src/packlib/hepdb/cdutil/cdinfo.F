*
* $Id: cdinfo.F,v 1.1.1.1 1996/02/28 16:24:41 mclareni Exp $
*
* $Log: cdinfo.F,v $
* Revision 1.1.1.1  1996/02/28 16:24:41  mclareni
* Hepdb, cdlib, etc
*
*
#include "hepdb/pilot.h"
      SUBROUTINE CDINFO (IUDIV, LAD, LSUP, JBIAS, IRC)
*     ================================================
*
************************************************************************
*                                                                      *
*        SUBR. CDINFO (IUDIV, LAD*, LSUP, JBIAS, IRC*)                 *
*                                                                      *
*   Creates DBTB bank with information of the data base objects used   *
*   for this event (till the last call to CDINFO). It stores 2 words   *
*   per object used, a unique identifier corresponding to the path     *
*   name and the serial number of the object (KEY(1) value)            *
*                                                                      *
*   Arguments :                                                        *
*                                                                      *
*     IUDIV    User division where the DBTB bank has to be created     *
*     LAD      Address of the DBTB bank (should be in the same store   *
*              as all DB objects)                                      *
*     LSUP     Address of the supporting bank                          *
*     JBIAS    Link bias as described in ZEBRA manual                  *
*     IRC      Return code (see below)                                 *
*                                                                      *
*   Called by user                                                     *
*                                                                      *
*   Error Condition :                                                  *
*                                                                      *
*     IRC       =  0 : No error                                        *
*                                                                      *
************************************************************************
*
#include "hepdb/cdcblk.inc"
      PARAMETER       (NLEVM=20, NDMAX=100)
      DIMENSION       LSUP(9), LAD(9), ISDI(NLEVM), NSDI(NLEVM)
*
*     ------------------------------------------------------------------
*
* *** Loop over all top directories
*
      LREFCD(1) = LSUP(1)
      NDAT   = 0
      IRC    = 0
      LBUPCD = LTOPCD
   10 IF (LBUPCD.GT.0) THEN
        NLEV   = 0
        LBNOCD = LQ(KOFUCD+LBUPCD-1)
*
*  **   Scan down the nodes to find all the subdirectories
*
   15   IF (LBNOCD.GT.0) THEN
          NLEV   = NLEV + 1
          ISDI(NLEV) = 0
          NSDI(NLEV) = IQ(KOFUCD+LBNOCD-KLNOCD)
*
   20     ISDI(NLEV) = ISDI(NLEV) + 1
          IF (ISDI(NLEV).LE.NSDI(NLEV)) THEN
*
*  **       If a new subdirectory go down one level
*
            LBD    = LQ(KOFUCD+LBNOCD-ISDI(NLEV))
            IF (LBD.GT.0) THEN
              LBNOCD = LBD
              GO TO 15
            ELSE
              GO TO 20
            ENDIF
*
          ELSE
*
*  **       Loop over all the key banks
*
            NDK    = IQ(KOFUCD+LBNOCD+MNDNWD)
            IDIC   = IQ(KOFUCD+LBNOCD+MNDDIC)
            LBKYCD = LQ(KOFUCD+LBNOCD-KLKYCD)
   25       IF (LBKYCD.GT.0) THEN
              IF (IQ(KOFUCD+LBKYCD+NDK+MKYCEV).GT.0) THEN
                IF (NDAT.EQ.0) THEN
                  NDTOT  = NDMAX
                  CALL CDBANK (IUDIV, LAD(1), LREFCD(1), JBIAS, 'DBTB',
     +                         0, 0, NDTOT, 2, -1, IRC)
                  IF (IRC.NE.0) GO TO 999
                  LREFCD(2) = LAD(1)
                ELSE IF (NDAT.GE.NDTOT) THEN
                  NDTOT  = NDTOT + NDMAX
                  CALL CDBANK (IUDIV, LAD(1), LREFCD(1), JBIAS, 'DBTB',
     +                         0, 0, NDTOT, 2, -1, IRC)
                  IF (IRC.NE.0) GO TO 999
                  LREFCD(2) = LAD(1)
                  LBD    = LQ(KOFUCD+LREFCD(1))
                  CALL UCOPY (IQ(KOFUCD+LBD+1), IQ(KOFUCD+LREFCD(2)+1),
     +                        NDAT)
                  CALL MZDROP (IUDIV, LBD, 'L')
                ENDIF
                IQ(KOFUCD+LREFCD(2)+NDAT+1) = IDIC
                IQ(KOFUCD+LREFCD(2)+NDAT+2) = IQ(KOFUCD+LBKYCD+IDHKSN)
                NDAT   = NDAT + 2
                IQ(KOFUCD+LBKYCD+NDK+MKYCEV)  = 0
              ENDIF
              LBKYCD = LQ(KOFUCD+LBKYCD)
              GO TO 25
            ENDIF
*
*  **       Now go up one level
*
   30       NLEV   = NLEV - 1
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
*
* *** Now shrink DBTB bank if needed
*
      IF (NDAT.GT.0) THEN
        IF (NDAT.LT.NDTOT) THEN
          NDPUS  = NDAT - NDTOT
          CALL MZPUSH (IUDIV, LREFCD(2), 0, NDPUS, 'I')
        ENDIF
      ENDIF
*                                                             END CDINFO
  999 END
