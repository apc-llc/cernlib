*
* $Id: dzdcrd.F,v 1.1.1.1 1996/03/04 16:12:53 mclareni Exp $
*
* $Log: dzdcrd.F,v $
* Revision 1.1.1.1  1996/03/04 16:12:53  mclareni
* Dzdoc/Zebpack
*
*
#include "dzdoc/pilot.h"
      SUBROUTINE DZDCRD(LUN,STRING,IRC)
      CHARACTER*(*) STRING
      INTEGER LUN,IRC
      SAVE
*     read a character string form LUN
      IRC=0
#if defined(CERNLIB_QMIBM)
      READ(LUN,NUM=NBYT,END=10,ERR=20)STRING
      STRING(NBYT+1:)=' '
#endif
#if !defined(CERNLIB_QMIBM)
      READ(LUN,'(A)',END=10,ERR=20)STRING
#endif
*     remove tabs
      ITABCO=0
      DO 5 I=1,LNBLNK(STRING)
         IF(ICHAR(STRING(I:I)).EQ.9)THEN
            IF(ITABCO.EQ.0)THEN
               WRITE(*,*)'Ignore tabs in: ',STRING
               ITABCO=ITABCO+1
            ENDIF
            STRING(I:I)=' '
         ENDIF
 5    CONTINUE
      GOTO 99
10    IRC=-1
20    IRC=IRC-1
99    RETURN
      END
***********************************************************************
