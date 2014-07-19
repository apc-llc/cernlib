*
* $Id: fmkuwd.F,v 1.1.1.1 1996/03/07 15:17:43 mclareni Exp $
*
* $Log: fmkuwd.F,v $
* Revision 1.1.1.1  1996/03/07 15:17:43  mclareni
* Fatmen
*
*
#include "fatmen/pilot.h"
      SUBROUTINE FMKUWD
*CMZ :          30/04/91  16.52.25  by  Jamie Shiers
*-- Author :    Jamie Shiers   30/04/91
#include "fatmen/fatpara.inc"
#include "fatmen/fatsys.inc"
#include "fatmen/fatbug.inc"
#include "fatmen/fatuwd.inc"
      CHARACTER*80 CHWORD(10)
      DIMENSION    LCH(10)
#include "fatmen/fatinit.inc"
 
      CALL KUGETC(CHWORD(1),LCH(1))
      CALL KUGETC(CHWORD(2),LCH(2))
      CALL KUGETC(CHWORD(3),LCH(3))
      CALL KUGETC(CHWORD(4),LCH(4))
      CALL KUGETC(CHWORD(5),LCH(5))
      CALL KUGETC(CHWORD(6),LCH(6))
      CALL KUGETC(CHWORD(7),LCH(7))
      CALL KUGETC(CHWORD(8),LCH(8))
      CALL KUGETC(CHWORD(9),LCH(9))
      CALL KUGETC(CHWORD(10),LCH(10))
 
      IF(LCH(1).EQ.0) RETURN
 
      DO 10 I=1,10
         IF(LCH(I).EQ.0) GOTO 10
*
*     -1?
*
         IF(CHWORD(I)(1:LCH(I)).EQ.'-1') THEN
            IFUSER(1,I) = -1
            IFUSER(2,I) = -1
            GOTO 10
         ENDIF
*
*     Range?
*
         IMINUS = INDEX(CHWORD(I)(1:LCH(I)),'-')
         IF(IMINUS.EQ.0) THEN
*
*     Is it numeric?
*
            IF(ICNUM(CHWORD(I),1,LCH(I)).LE.LCH(I)) THEN
               IF(IDEBFA.GE.0) PRINT *,'FMKUWD. element ',I, ' of ',
     +         CHWORD(I)(1:LCH(I)),' is not numeric - ignored'
               GOTO 10
            ENDIF
 
            IFUSER(1,I) = ICDECI(CHWORD(I),1,LCH(I))
            IFUSER(2,I) = IFUSER(1,I)
 
            IF(IDEBFA.GE.3) PRINT *,'FMKUWD. user word # ',I,
     +         ' = ',IFUSER(1,I)
 
         ELSE
            IFUSER(1,I) = ICDECI(CHWORD(I),1,IMINUS-1)
            IFUSER(2,I) = ICDECI(CHWORD(I),IMINUS+1,LCH(I))
            IF(IDEBFA.GE.3) PRINT *,'FMKUWD. user word # ',I,
     +         ' = ',IFUSER(1,I),' to ',IFUSER(2,I)
         ENDIF
 
   10 CONTINUE
 
      END
