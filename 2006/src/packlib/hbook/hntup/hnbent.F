*
* $Id: hnbent.F,v 1.1.1.1 1996/01/16 17:07:57 mclareni Exp $
*
* $Log: hnbent.F,v $
* Revision 1.1.1.1  1996/01/16 17:07:57  mclareni
* First import
*
*
#include "hbook/pilot.h"
*CMZ :  4.19/01 11/05/93  10.30.35  by  Fons Rademakers
*-- Author :    Fons Rademakers   18/02/93
      SUBROUTINE HNBENT(IDD,BLOCK,NUMB)
*.==========>
*.
*.           Returns the number of entries in block BLOCK of Ntuple IDD.
*.
*..=========> ( A.A.Rademakers )
*
#include "hbook/hcbook.inc"
#include "hbook/hcntpar.inc"
*
      CHARACTER*(*)  BLOCK
      CHARACTER*8    BLKNAM
      INTEGER        HNBPTR
*
      NUMB   = -1
      BLKNAM = BLOCK
*
      CALL HFIND(IDD,'HNBENT')
*
      I4 = JBIT(IQ(LCID+KBITS),4)
      IF (I4.NE.0 .AND. IQ(LCID-2).EQ.ZLINK) THEN
         LBLOK = HNBPTR(BLKNAM)
         IF (LBLOK .EQ. 0) THEN
            CALL HBUG('Block does not exist','HNBENT',IDD)
            RETURN
         ENDIF
         NUMB = IQ(LBLOK+ZNOENT)
      ENDIF
*
      END
