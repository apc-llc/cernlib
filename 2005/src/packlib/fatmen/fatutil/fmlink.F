*
* $Id: fmlink.F,v 1.1.1.1 1996/03/07 15:18:05 mclareni Exp $
*
* $Log: fmlink.F,v $
* Revision 1.1.1.1  1996/03/07 15:18:05  mclareni
* Fatmen
*
*
#include "fatmen/pilot.h"
      SUBROUTINE FMLINK(GENEN,LBANK,CHOPT,IRC)
*
************************************************************************
*
*         SUBR. FMLINK(GENEN,LBANK*,CHOPT,IRC)
*
*        Return the address of the bank corresponding to GENEN
*
*        GENEN  generic file name
*
*        LBANK  bank address
*
*        CHOPT  character option: Q = 'QUIET'
*
*   Called by User
*
*   Error Condition :
*
*     IQUEST(1) =  0 : No error
*               = 55 : Illegal Path name
*               = 56 : Illegal file name
*               = 57 : Path name cannot be found in the list of those
*                      initialized
*               = 58 : File not "opened"
*
************************************************************************
*
#include "fatmen/fatbank.inc"
#include "fatmen/fatpara.inc"
*
      CHARACTER*(*)   CHOPT
      CHARACTER       GENEN*(*),  PATHN*255, PATH*255,
     +                PATHX*255,  FNAME*20
      DIMENSION       LBANK(9)
*
*______________________________________________________________________
*
      LBANK(1)  = 0
      IQUEST(1) = 0
      IRC       = 0
*
* *** Find number of characters in the path name
*
      NCH    = LENOCC (GENEN)
      CALL CLTOU(GENEN)
      IF (NCH.LT.3.OR.GENEN(1:2).NE.'//'.OR.GENEN(NCH:NCH).EQ.'/') THEN
        IQUEST(1) = 55
        IF ((CHOPT(1:1) .NE. 'Q') .AND. (IDEBFA.GT.-3))
     +  WRITE (LPRTFA, 1001) GENEN
        GO TO 999
      ENDIF
*
* ***  Find file name
*
      ICH =  INDEXB(GENEN(1:NCH-1),'/')
      IF (ICH.LE.3.OR. NCH-ICH.GT.20) THEN
        IQUEST(1) = 56
        IF ((CHOPT(1:1) .NE. 'Q') .AND. (IDEBFA.GT.-3))
     +  WRITE (LPRTFA, 1002) GENEN
        GO TO 999
      ENDIF
      PATHN = GENEN(1:ICH-1)
CSELF,IF=-CZ.
*
*
* *** Set the current directory (or create a new one)
*
C     CALL FACDIR (PATHN, ' ')
C     IF (IQUEST(1).NE.0)  CALL FATMDI (PATHN, LKEYFA, CHFOR, CHTAG)
C     CALL FACDIR (PATH, 'R')
CSELF,IF=CZ.
      PATH=PATHN
CSELF.
      NCHD   = INDEX (PATH, ' ') - 1
      IF (NCHD.LE.0) NCHD = MAXLFA
*
* *** Check if the Path name matches
*
      LSAVFA = LTOPFA
   10 IF (LSAVFA.NE.0) THEN
        NCHR   = IQ(KOFUFA+LSAVFA+MNCHFA)
        CALL UHTOC (IQ(KOFUFA+LSAVFA+MCHRFA), 4, PATHX, NCHR)
        IF (PATH(1:NCHR).NE.PATHX(1:NCHR)) THEN
          LSAVFA = LQ(KOFUFA+LSAVFA)
          GO TO 10
        ENDIF
      ELSE
        IQUEST(1) = 57
        IF ((CHOPT(1:1) .NE. 'Q') .AND. (IDEBFA.GT.-3))
     +  WRITE (LPRTFA, 1003) PATH(1:NCHD)
        GO TO 999
      ENDIF
*
* ***    Book-keeping
*
      LBBKFA = LQ(KOFUFA+LSAVFA-KLBKFA)
      LBGNFA = LQ(KOFUFA+LSAVFA-KLGNFA)
*        find the bank adress
      NOPF = IQ(KOFUFA+LSAVFA+MNOPFA)
      IF (NOPF.NE.0) THEN
        LB = KOFUFA+LBBKFA
        DO 20 I=1,NOPF
          NCHR = IQ(LB+MCGNFA)
          IF (NCHR.NE.NCH) GO TO 21
          IPT = IQ(LB+MPNTFA)
          CALL UHTOC (IQ(KOFUFA+LBGNFA+IPT), 4, PATHX, NCHR)
          IF (GENEN(1:NCHR).EQ.PATHX(1:NCHR)) THEN
*              file found
            LBANK(1) = LQ(KOFUFA+LSAVFA-NSLUFA-I)
            GO TO 999
          ENDIF
21        LB = LB + NWBKFA
20      CONTINUE
      ENDIF
      IQUEST(1) = 58
        IF ((CHOPT(1:1) .NE. 'Q') .AND. (IDEBFA.GT.-3))
     + WRITE (LPRTFA, 1004) GENEN
 
*
 1001 FORMAT (/,' FMLINK : Illegal path name ',A)
 1002 FORMAT (/,' FMLINK : Illegal file name ',A)
 1003 FORMAT (/,' FMLINK : RZ file is not initialized for ',A)
 1004 FORMAT (/,' FMLINK : File not opened for ',A)
*                                                            END FMLINK
  999 IF(IQUEST(1).NE.0) IRC = 1
      END
 
