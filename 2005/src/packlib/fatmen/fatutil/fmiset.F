*
* $Id: fmiset.F,v 1.1.1.1 1996/03/07 15:18:05 mclareni Exp $
*
* $Log: fmiset.F,v $
* Revision 1.1.1.1  1996/03/07 15:18:05  mclareni
* Fatmen
*
*
#include "fatmen/pilot.h"
      INTEGER FUNCTION FMISET(CHOPT,CHSET)
*CMZ :          30/01/91  16.44.16  by  Jamie Shiers
*-- Author :    Jamie Shiers   30/01/91
      CHARACTER*(*) CHOPT,CHSET
#include "fatmen/fatbug.inc"
      FMISET = 0
*
*     Check whether all characters in CHOPT are in CHSET
*
      LCHOPT = LENOCC(CHOPT)
      LCHSET = LENOCC(CHSET)
 
      IF(IDEBFA.GE.4) PRINT *,'FMISET. checking that all characters ',
     +   'in ',CHOPT(1:LCHOPT),' are in ',CHSET(1:LCHSET)
 
      DO 10 I=1,LCHOPT
      IF(INDEX(CHSET(1:LCHSET),
     +         CHOPT(I:I)).EQ.0) GOTO 20
10    CONTINUE
 
      RETURN
 
20    FMISET = I
      IF(IDEBFA.GE.4) PRINT *,'FMISET. first char. not in set = ',
     +   CHOPT(I:I)
      END
