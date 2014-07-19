*
* $Id: famove.F,v 1.1.1.1 1996/03/07 15:17:40 mclareni Exp $
*
* $Log: famove.F,v $
* Revision 1.1.1.1  1996/03/07 15:17:40  mclareni
* Fatmen
*
*
#include "fatmen/pilot.h"
#if defined(CERNLIB_IBMMVS)
      SUBROUTINE FAMOVE(OLDDSN,NEWDSN,IRC)
*CMZ :          17/05/91  14.46.08  by  Jamie Shiers
*-- Author :    Jamie Shiers   17/05/91
      CHARACTER*(*) OLDDSN,NEWDSN
      CHARACTER*80  CARD
#include "fatmen/fattyp.inc"
 
      IRC = 0
 
      LOLD = LENOCC(OLDDSN)
      LNEW = LENOCC(NEWDSN)
      OPEN(11,FILE='/'//OLDDSN(1:LOLD),STATUS='OLD',ERR=99,
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
 
1     READ(11,'(A)',END=98) CARD
      WRITE(12,'(A)')       CARD
      GOTO 1
 
98    CLOSE(11,STATUS='DELETE')
      CLOSE(12,STATUS='KEEP')
 
      RETURN
 
99    IRC = 1
      END
#endif
