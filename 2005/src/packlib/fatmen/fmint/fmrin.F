*
* $Id: fmrin.F,v 1.1.1.1 1996/03/07 15:18:10 mclareni Exp $
*
* $Log: fmrin.F,v $
* Revision 1.1.1.1  1996/03/07 15:18:10  mclareni
* Fatmen
*
*
#include "fatmen/pilot.h"
#if defined(CERNLIB_CZ)
      SUBROUTINE FMRIN (PATHN, FNAME, IUDIV, LSUP, JBIAS, NWKEY, KEYS)
*
************************************************************************
*
*      SUBR. FMRIN (PATHN, FNAME, IUDIV, *LSUP*, JBIAS, *NWKEY*, KEYS*)
*
*   Inputs one data structure from remote Zebra server
*
*   Arguments :
*
*     PATHN    Path name of the directory
*     FNAME    file name (character*20)
*     IUDIV    Division index where the data structure exists
*     LSUP     Address supporting the data structure
*     JBIAS    < 1 : LSUP is the supporting bank and JBIAS is the link
*                    bias specifying where the data structure has to
*                    be introduced
*              = 1 : LSUP is the supporting link
*              = 2 : Stand alone data structure with address at LSUP
*     NWKEY    Maximum size available for KEYS vector
*              On return it carries the actual size of the KEYS
*     KEYS     Key vector
*
*   Called by User
*
*   Error Condition :
*
*     IQUEST(1) =  0 : No error
*               = 11 : Illegal Path name
*
************************************************************************
*
#include "fatmen/fmpath.inc"
#include "fatmen/fatbank.inc"
#include "fatmen/fatpara.inc"
#include "fatmen/fatsys.inc"
      DIMENSION IQSAVE(100)
      CHARACTER*80    LINE
      CHARACTER       PATHN*255, FNAME*20
      DIMENSION       LSUP(9), KEYS(*)
      PARAMETER       (MAXKEY=500)
      PARAMETER       (LKEYFA=10)
      DIMENSION       KEYV(LKEYFA,MAXKEY)
*
*     ------------------------------------------------------------------
*
* *** Find number of characters in the path name
*
      NCH    = LENOCC (PATHN)
      IF (NCH.LT.3.OR.PATHN(1:2).NE.'//') THEN
        IQUEST(1) = 11
        IF (IDEBFA.GT.-3) WRITE (LPRTFA, 1001) PATHN(1:NCH)
        GO TO 999
      ENDIF
      LFILE = LENOCC(FNAME)
*
*     Get keys from remote server
*
      CALL CZPUTA('MESS :KE',ISTAT)
      NKEYS = 0
      IFOUND = 1
1     CONTINUE
 
      CALL CZGETA(LINE,ISTAT)
*
      IF (LINE(1:1) .EQ. '0') GOTO 2
      IF (INDEX(LINE,'No files found') .NE. 0) THEN
         IFOUND = 0
         ELSE
         NKEYS = NKEYS + 1
         READ(LINE,9001) (KEYV(J,NKEYS),J=1,10)
         ENDIF
 
9001  FORMAT(2X,I5,5A4,4I5)
      IF (LINE(1:1) .EQ. '2') GOTO 1
2     CONTINUE
      IF (IFOUND .EQ. 0) GOTO 20
*
*     Loop over keys returned
*
      IFOUND = 0
      DO 10 I=1,NKEYS
      CALL UHTOC(KEYV(MKFNFA,I),4,FILE2,(MKCLFA-MKFNFA)*4)
      LFILE2 = MIN(LENOCC(FILE2),20)
 
      IF ((FILE2(1:1) .EQ. ' ') .OR.
     +    (FILE2(1:LFILE2) .NE. FNAME(1:LFILE))) GOTO 10
*
*     Get banks from remote server
*
      WRITE(LINE,9011) (KEYV(J,I),J=1,10)
9011  FORMAT(' XX',I5,5A4,4I5)
      CALL CZPUTA('MESS :IN'//LINE,ISTAT)
      NH = 100
      CALL FZIN(999,IUDIV,LSUP,JBIAS,' ',NH,IQSAVE)
      CALL DZSHOW('FMRIN.',IUDIV,LSUP,'B',0,0,0,0)
      CALL UCOPY(IQSAVE,IQUEST,100)
      CALL UCOPY(KEYV(I,1),KEYS,10)
10    CONTINUE
20    CONTINUE
 
 1001 FORMAT (/,' FMRIN : Illegal path name ',A)
 1003 FORMAT (/,' FMRIN : Error in RZIN for ',A)
 1004 FORMAT (/,' FMRIN : File ',A,' not found')
*                                                              END FMRIN
999   END
 
#endif
