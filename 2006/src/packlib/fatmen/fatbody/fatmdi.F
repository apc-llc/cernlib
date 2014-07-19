*
* $Id: fatmdi.F,v 1.1.1.1 1996/03/07 15:18:02 mclareni Exp $
*
* $Log: fatmdi.F,v $
* Revision 1.1.1.1  1996/03/07 15:18:02  mclareni
* Fatmen
*
*
#include "fatmen/pilot.h"
      SUBROUTINE FATMDI (PATH, NWKEY, CHFOR, CHTAG)
*
************************************************************************
*
*        SUBR. FATMDI (PATH, NWKEY, CHFOR, CHTAG)
*
*   Creates Directories with the conventions of FAT package
*
*   Arguments :
*
*     PATH     Path name of the directory
*     NWKEY    Number of words associated to the keys at the lowest
*              level (If some directory in the pathname at a higher
*              level does not exist, it is created with same keys )
*     CHFOR    Character variable describing each element of the key
*              vector at the lowest level
*     CHTAG    Character array defined as CHARACTER*8 (NWKEY)
*
*   Called by FATINI
*
*   Error Condition :
*
*     IQUEST(1) =  0 : No error
*               = 21 : Too many key elements
*               = 22 : Illegal Path name
*               = 23 : RZ file is opened with no write access
*               = 24 : Path name cannot be found in the list of those
*                      initialized
*               = 25 : The Directory already exists
*               = 26 : Error in directory creation (RZMDIR)
*
************************************************************************
*
#include "fatmen/fatbank.inc"
      CHARACTER       CHFOR*(*), PATH*(*)
      CHARACTER       PATHX*255, TOPN*16, TOP*16
      CHARACTER*(*)   CHTAG(*)
*
*     ------------------------------------------------------------------
*
* *** Check on number of keys
*
      IF (NWKEY.GT.MXDMFA) THEN
         IQUEST(1) = 21
         IQUEST(11)= NWKEY
         IQUEST(12)= MXDMFA
         IF (IDEBFA.GE.-3) WRITE (LPRTFA, 9003) IQUEST(11), IQUEST(12)
         GO TO 999
      ENDIF
*
* *** Find number of characters in the path name
*
      NCH    = LENOCC (PATH)
      IF (NCH.LT.3.OR.PATH(1:2).NE.'//') THEN
         IQUEST(1) = 22
         IF (IDEBFA.GE.-3) WRITE (LPRTFA, 9004) PATH
         GO TO 999
      ENDIF
      II     = INDEX (PATH(3:NCH), '/') - 1
      IF (II.LE.0) THEN
         II = NCH
      ELSE
         II = II + 2
      ENDIF
      TOPN   = PATH(3:II)
*SELF,IF=-*FATSRV.
* What is this for?
#if defined(CERNLIB_NEVER)
*
* *** Check if the Path name matches
*
      LSAVFA = LTOPFA
      IFND   = 0
   10 IF (LSAVFA.NE.0) THEN
         NCHR = IQ(KOFUFA+LSAVFA+MNCHFA)
         CALL UHTOC (IQ(KOFUFA+LSAVFA+MCHRFA), 4, PATHX, NCHR)
         II = INDEX (PATHX(3:NCHR), '/') - 1
         IF (II.LE.0) THEN
            II = NCHR
         ELSE
            II = II + 2
         ENDIF
         TOP = PATHX(3:II)
         IF (TOPN.NE.TOP) THEN
            LSAVFA = LQ(KOFUFA+LSAVFA)
            GO TO 10
         ENDIF
         IFND = IFND + 1
         IFLG = IQ(KOFUFA+LSAVFA+MFLGFA)
         IF (IFLG.NE.0) GO TO 20
         LSAVFA = LQ(KOFUFA+LSAVFA)
         GO TO 10
      ELSE IF (IFND.EQ.0) THEN
         IQUEST(1) = 23
         IF (IDEBFA.GE.-3) WRITE (LPRTFA, 9005) PATH(1:NCH)
         GO TO 999
      ELSE
         IQUEST(1) = 24
         IF (IDEBFA.GE.-3) WRITE (LPRTFA, 9006) PATH(1:NCH)
         GO TO 999
      ENDIF
#endif
*
* *** Create a new (tree of) director(ies) - from the first non-existing
* *** name. Go down to the lowest existing directory in PATH
*
   20 ICH    = 2
      ISTCH  = ICH + 1
   30 ICH    = ICH + 1
      IF (PATH(ICH:ICH).EQ.'/'.OR.ICH.GT.NCH) THEN
         CALL RZCDIR (PATH(1:ICH-1), ' ')
         IF (IQUEST(1).EQ.0) THEN
            IF (ICH.GT.NCH) THEN
               IQUEST(1) = 25
               IF (IDEBFA.GE.-3) WRITE (LPRTFA, 9007) PATH(1:NCH)
               GO TO 999
            ELSE
               IF(IDEBFA.GE.3) WRITE (LPRTFA, 9001) PATH(1:ICH-1)
 9001 FORMAT(' FATMDI. directory ',A,' exists (OK)')
               ISTCH = ICH + 1
               GO TO 30
            ENDIF
         ELSE
*
*  **     Create new directories
*
            PATHX = PATH(ISTCH:ICH-1)
C          IF (ICH.GT.NCH) THEN
            IF(IDEBFA.GE.3) THEN
               CALL RZCDIR(' ','P')
               PRINT 9002,PATHX(1:LENOCC(PATHX))
 9002 FORMAT(' FATMDI. attempting creation of subdirectory ',A)
            ENDIF
            CALL RZMDIR (PATHX, NWKEY, CHFOR, CHTAG)
C          ELSE
C            CALL RZMDIR (PATHX, 1, 'I', 'DUMMY   ')
C          ENDIF
            ICHR   = ICH - ISTCH
            IF (IQUEST(1).NE.0) THEN
               IQUEST(1) = 26
               IF (IDEBFA.GE.-3) WRITE (LPRTFA, 9008) PATHX(1:ICHR),
     +         PATH(1:NCH)
               GO TO 999
            ELSE
               IF (IDEBFA.GE.-3) WRITE (LPRTFA, 9009) PATHX(1:ICHR),
     +         PATH(1:NCH)
            ENDIF
            IF (ICH.GT.NCH) GO TO 999
            CALL RZCDIR (PATH(1:ICH-1), ' ')
            IF (IQUEST(1).NE.0) THEN
               IQUEST(1) = 26
               ICHR = ICH - ISTCH
               IF (IDEBFA.GE.-3) WRITE (LPRTFA, 9008) PATHX(1:ICHR),
     +         PATH(1:NCH)
               GO TO 999
            ENDIF
            ISTCH = ICH + 1
            GO TO 30
         ENDIF
      ELSE
         GO TO 30
      ENDIF
*
 9003 FORMAT (/,' FATMDI : Too many key elements requested = ',I6
     +,' maximum permitted ',I5)
 9004 FORMAT (/,' FATMDI : Illegal path name ',A)
 9005 FORMAT (/,' FATMDI : RZ file opened with no write access for ',A)
 9006 FORMAT (/,' FATMDI : RZ file is not initialized for ',A)
 9007 FORMAT (/,' FATMDI : Directory ',A,' already exists')
 9008 FORMAT (/,' FATMDI : Error creating part ',A,' of path ',A)
 9009 FORMAT (/,' FATMDI : Part ',A,' of path ',A,' created.')
*                                                             END FATMDI
  999 END
