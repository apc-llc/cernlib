*
* $Id: fmlift.F,v 1.1.1.1 1996/03/07 15:18:01 mclareni Exp $
*
* $Log: fmlift.F,v $
* Revision 1.1.1.1  1996/03/07 15:18:01  mclareni
* Fatmen
*
*
#include "fatmen/pilot.h"
      SUBROUTINE FMLIFT (GENEN, KEYS, MEDIA, CHOPT, IRC)
*
************************************************************************
*
*        SUBR. FMLIFT (GENEN, *KEYS*, MEDIA, CHOPT ,IRC)
*
*   Create a new entry into the data set data base
*
*   Arguments :
*
*     GENEN    File generic name
*     KEYS     Keys vector
*     MEDIA    Media type
*     CHOPT    Character option - U = user will allocate media
*
*   Called by User
*
*   Error Condition :
*
*     IQUEST(1) =  0 : No error
*               = 51 : Illegal Path name
*               = 52 : Illegal file name
*               = 53 : Path name cannot be found in the list of those
*                      initialized
*               = 54 : File already "opened"
*
************************************************************************
*
#include "fatmen/fatbank.inc"
#include "fatmen/fatpara.inc"
*
      CHARACTER       GENEN*(*),  PATHN*255, PATH*255,
     +                PATHX*255,  FNAME*20, MEDIA*(*)
      DIMENSION       LSUP(9), KEYS(*)
*
      PARAMETER     (LKEYFA=10)
      DIMENSION     INKEYS(LKEYFA)
      CHARACTER*8   CHTAG(LKEYFA)
      CHARACTER*10  CHFOR
      CHARACTER*(*) CHOPT
      DATA CHTAG/'Num.Id.',5*'Fname'
     +          ,'cp.level','loc.code','medium','nm.banks'/
      DATA CHFOR/'IHHHHHIIII'/
*
*______________________________________________________________________
      IRC       = 0
      IQUEST(1) = 0
*
* *** Find number of characters in the path name
*
      NCH    = LENOCC (GENEN)
      CALL CLTOU(GENEN)
      IF (NCH.LT.3.OR.GENEN(1:2).NE.'//'.OR.GENEN(NCH:NCH).EQ.'/') THEN
        IQUEST(1) = 51
        IF (IDEBFA.GE.-3) WRITE (LPRTFA, 1001) GENEN(1:NCH)
        GO TO 999
      ENDIF
*
* ***  Find file name
*
      ICH =  INDEXB(GENEN(1:NCH-1),'/')
      IF (ICH.LE.3.OR. NCH-ICH.GT.20) THEN
        IQUEST(1) = 52
        IF (IDEBFA.GE.-3) WRITE (LPRTFA, 1002) GENEN(1:NCH)
        GO TO 999
      ENDIF
      PATHN = GENEN(1:ICH-1)
      FNAME = GENEN(ICH+1:NCH)
      CALL VBLANK(KEYS(2),5)
      CALL UCTOH(FNAME,KEYS(2),4,NCH-ICH)
*------ These lines not really useful in client. Remove 23/01/90 -------
CSELF,IF=-CZ.
*
*
* *** Set the current directory (or create a new one)
*
C     CALL FACDIR (PATHN, ' ')
C     IF (IQUEST(1).NE.0)  CALL FATMDI (PATHN, LKEYFA, CHFOR, CHTAG)
C     CALL FACDIR (PATH, 'R')
 
C     NCHD   = INDEX (PATH, ' ') - 1
C     IF (NCHD.LE.0) NCHD = MAXLFA
*------ These lines not really useful in client. Remove 23/01/90 -------
CSELF,IF=CZ.
      PATH = PATHN
      NCHD = LENOCC(PATHN)
CSELF.
*
*     Check if this generic name already exists in RZ file
*
      CALL FMEXST(GENEN(1:NCH),IGET)
      IQUEST(11) = IGET
*
*     Check if this directory already exists
*
      CALL FACDIR(PATHN(1:NCHD),' ')
      IF((IQUEST(1).NE.0).AND.(IDEBFA.GE.1)) PRINT *,
     +  'FMLIFT. directories will be automatically created by server'
      IQUEST(12) = IQUEST(1)
 
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
        IQUEST(1) = 53
        IF (IDEBFA.GE.-3) WRITE (LPRTFA, 1003) PATH(1:NCHD)
        GO TO 999
      ENDIF
*
* ***    Book-keeping
*
      LBBKFA = LQ(KOFUFA+LSAVFA-KLBKFA)
      LBGNFA = LQ(KOFUFA+LSAVFA-KLGNFA)
*        check that this file is not already used
      NOPF = IQ(KOFUFA+LSAVFA+MNOPFA)
      IF (NOPF.NE.0) THEN
        LB = KOFUFA+LBBKFA
        DO 20 I=1,NOPF
          NCHR = IQ(LB+MCGNFA)
          IF (NCHR.NE.NCH) GO TO 21
          IPT = IQ(LB+MPNTFA)
          CALL UHTOC (IQ(KOFUFA+LBGNFA+IPT), 4, PATHX, NCHR)
          IF (GENEN(1:NCHR).EQ.PATHX(1:NCHR)) THEN
*              file already opened
            IQUEST(1) = 54
            IF (IDEBFA.GE.-3) WRITE (LPRTFA, 1004) GENEN(1:NCH)
            GO TO 999
          ENDIF
21        LB = LB + NWBKFA
20      CONTINUE
      ENDIF
*         create a new entry
      NOPF = NOPF + 1
      IQ(KOFUFA+LSAVFA+MNOPFA) = NOPF
      IF (NOPF.GT.IQ(KOFUFA+LSAVFA-2)-NSLUFA)  THEN
         CALL MZPUSH(IDIVFA,LSAVFA,10,0,' ')
         CALL MZPUSH(IDIVFA,LBBKFA,0,10*NWBKFA,' ')
      ENDIF
      NWUD = IQ(KOFUFA+LBGNFA-5)
      NNW  = (NCH+3)/4
      IF (NWUD+NNW.GT.IQ(KOFUFA+LBGNFA-1))  THEN
         CALL MZPUSH(IDIVFA,LBGNFA,0,800,' ')
      ENDIF
      LB = KOFUFA+LBBKFA+NWBKFA*(NOPF-1)
      IQ(LB+MSTAFA) = 0
      IQ(LB+MPNTFA) = NWUD+1
      IQ(LB+MCGNFA) = NCH
      IQ(KOFUFA+LBGNFA-5) = NWUD+NNW
      CALL UCTOH(GENEN,IQ(KOFUFA+LBGNFA+NWUD+1),4,NCH)
*
* ***  Create the bank structure  ...
*
      JB = -(NOPF+NSLUFA)
      CALL MZBOOK (IDIVFA, LTDSFA, LSAVFA, JB, 'DSFA', 0, 0, NWDSFA,
     +             IODSFA, 0)
      LBDSFA = LTDSFA
*
*     Zero/blank it according to I/O characteristic
*
      CALL DZZERO(IDIVFA,LTDSFA)
      IQUEST(1) = 0
*
* ***  ... and fill it in
*
      CALL FAFILL(LTDSFA+KOFUFA,PATHN,FNAME,KEYS,MEDIA,CHOPT)
 
 
 1001 FORMAT (' FMLIFT. Illegal path name ',A)
 1002 FORMAT (' FMLIFT. Illegal file name ',A)
 1003 FORMAT (' FMLIFT. RZ file is not initialized for ',A)
 1004 FORMAT (' FMLIFT. File already opened for ',A)
*                                                            END FMLIFT
  999 IF(IQUEST(1) .NE. 0) IRC = 1
      IQUEST(11) = 0
      IF(IGET.EQ.0) IQUEST(11) = 1
      END
 
