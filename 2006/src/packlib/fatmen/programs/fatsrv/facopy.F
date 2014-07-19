*
* $Id: facopy.F,v 1.1.1.1 1996/03/07 15:17:41 mclareni Exp $
*
* $Log: facopy.F,v $
* Revision 1.1.1.1  1996/03/07 15:17:41  mclareni
* Fatmen
*
*
#include "fatmen/pilot.h"
      SUBROUTINE FACOPY(OLDDSN,NEWDSN,IRC)
*CMZ :          17/05/91  14.46.08  by  Jamie Shiers
*-- Author :    Jamie Shiers   17/05/91
      CHARACTER*(*) OLDDSN,NEWDSN
      CHARACTER*255 SRCDSN,TGTDSN
      CHARACTER*80  CARD
#if defined(CERNLIB_UNIX)
      INTEGER       SYSTEMF
#endif
#include "fatmen/fattyp.inc"
#include "fatmen/fatbug.inc"
 
      IRC = 0
 
      LOLD = LENOCC(OLDDSN)
      LNEW = LENOCC(NEWDSN)
      IF(IDEBFA.GE.3) PRINT *,'FACOPY. copy ',OLDDSN(1:LOLD),
     +   ' to ',NEWDSN(1:LNEW)
#if defined(CERNLIB_IBMMVS)
      OPEN(11,FILE='/'//OLDDSN(1:LOLD),STATUS='OLD',ERR=98,
     +     FORM='FORMATTED',ACTION='READWRITE')
*
*     Create new CARD file
*
      LUNIT = LENOCC(CHMGEN(1))
      CALL FILEINF(ISTAT,'DEVICE',CHMGEN(1)(1:LUNIT),'TRK',1,
     +             'SECOND',1,'DIR',0,
     +             'RECFM','FB','LRECL',80,'BLKSIZE',9040)
      OPEN(12,FILE='/'//NEWDSN(1:LNEW),STATUS='NEW',ERR=99,
     +     FORM='FORMATTED',ACTION='READWRITE')
#endif
#if !defined(CERNLIB_IBMMVS)
      OPEN(11,FILE=OLDDSN(1:LOLD),STATUS='OLD',ERR=98,
     +     FORM='FORMATTED')
      OPEN(12,FILE=NEWDSN(1:LNEW),STATUS='NEW',ERR=99,
     +     FORM='FORMATTED')
#endif
 
1     READ(11,'(A)',END=97) CARD
      WRITE(12,'(A)')       CARD
      GOTO 1
 
97    CLOSE(11,STATUS='KEEP')
      CLOSE(12,STATUS='KEEP')
*
*     Now rename the file
*
      SRCDSN = NEWDSN(1:LNEW)
      TGTDSN = NEWDSN(1:LNEW)
#if defined(CERNLIB_VAXVMS)
      LSTA  = INDEX(TGTDSN(1:LNEW),']') + 1
      TGTDSN(LSTA:LSTA+1) = 'AA'
      ISTAT  = LIB$RENAME_FILE(SRCDSN(1:LNEW),TGTDSN(1:LNEW),,,,,,,,,,)
      IF(.NOT.ISTAT) CALL LIB$SIGNAL(%VAL(ISTAT))
#endif
#if defined(CERNLIB_UNIX)
      LSTA  = INDEXB(TGTDSN(1:LNEW),'/') + 1
      TGTDSN(LSTA:LSTA+1) = 'aa'
      ISTAT = SYSTEMF('mv '//SRCDSN(1:LNEW)//' '//TGTDSN(1:LNEW))
#endif
 
      RETURN
 
98    IRC = 1
      IF(IDEBFA.GE.-3) PRINT *,'FACOPY. cannot open input file - ',
     +   OLDDSN(1:LOLD)
      RETURN
99    IRC = 1
      IF(IDEBFA.GE.-3) PRINT *,'FACOPY. cannot open output file - ',
     +   NEWDSN(1:LNEW)
      RETURN
      END
