*
* $Id: ftrend.F,v 1.1.1.1 1996/03/07 15:18:01 mclareni Exp $
*
* $Log: ftrend.F,v $
* Revision 1.1.1.1  1996/03/07 15:18:01  mclareni
* Fatmen
*
*
#include "fatmen/pilot.h"
      SUBROUTINE FTREND (PATH)
*
************************************************************************
*
*        SUBR. FTREND (PATH)
*
*   Closes one output stream
*
*   Arguments :
*
*     PATH     Path name of the directory
*
*   Called by User
*
*   Error Condition :
*
*     IQUEST(1) =  0 : No error
*               = 31 : Illegal Path name
*               = 32 : Path name cannot be found in the list of those
*                      initialized
*
************************************************************************
*
#include "fatmen/fatbank.inc"
      CHARACTER       PATH*(*), PATHX*80, TOPN*16
*
*     ------------------------------------------------------------------
*
* *** Find number of characters in the path name
*
      NCH    = LENOCC (PATH)
      IF (PATH(1:1).NE.'*') THEN
        IF (NCH.LT.3.OR.PATH(1:2).NE.'//') THEN
          IQUEST(1) = 31
          IF (IDEBFA.GE.-3) WRITE (LPRTFA, 1001) PATH(1:NCH)
          GO TO 999
        ENDIF
      ENDIF
*
* *** Check if the Path name matches
*
    5 LSAVFA = LTOPFA
   10 IF (LSAVFA.NE.0) THEN
        NCHR   = IQ(KOFUFA+LSAVFA+MNCHFA)
        CALL UHTOC (IQ(KOFUFA+LSAVFA+MCHRFA), 4, PATHX, NCHR)
        IF (PATH(1:1).EQ.'*')               GO TO 15
        IF (PATH(1:NCH).NE.PATHX(1:NCHR)) THEN
          LSAVFA = LQ(KOFUFA+LSAVFA)
          GO TO 10
        ENDIF
      ELSE
        IQUEST(1) = 32
        IF (IDEBFA.GE.-3) WRITE (LPRTFA, 1002) PATH(1:NCH)
        GO TO 999
      ENDIF
*
* *** See if any other stream open with the same RZ file
*
   15 LUNRZ  = IQ(KOFUFA+LSAVFA+MLUNFA)
      IROPN  = 0
      LBFXFA = LTOPFA
   20 IF (LBFXFA.NE.0) THEN
        IF (LBFXFA.NE.LSAVFA) THEN
          IF (IQ(KOFUFA+LBFXFA+MLUNFA).EQ.LUNRZ) THEN
            IROPN  = 1
            GO TO 30
          ENDIF
        ENDIF
        LBFXFA = LQ(KOFUFA+LBFXFA)
        GO TO 20
      ENDIF
*
* *** Print out summary
*
   30 WRITE (LPRTFA,1000) PATHX(1:NCHR), LUNRZ, IQ(KOFUFA+LSAVFA+MTOTFA)
     +              , IQ(KOFUFA+LSAVFA+MINPFA), IQ(KOFUFA+LSAVFA+MOUTFA)
      IF (IROPN.EQ.0) THEN
        II     = INDEX (PATHX(3:NCHR), '/') - 1
        IF (II.LE.0) THEN
          II   = NCHR
        ELSE
          II   = II + 2
        ENDIF
        TOPN   = PATHX(3:II)
        CALL RZEND (TOPN)
      ENDIF
      CALL MZDROP (IDIVFA, LSAVFA, ' ')
      LSAVFA = 0
      IF (LTOPFA.NE.0.AND.PATH(1:1).EQ.'*') GO TO 5
*
 1000 FORMAT (/,' FTREND : Close stream for ',A,' on unit ',I4,
     +        /,'          Total number of data structures   ',I10,
     +        /,'          Number of data structures read    ',I10,
     +        /,'          Number of data structures written ',I10)
 1001 FORMAT (/,' FTREND : Illegal path name ',A)
 1002 FORMAT (/,' FTREND : RZ file is not initialized for ',A)
*                                                             END FTREND
  999 END
 
