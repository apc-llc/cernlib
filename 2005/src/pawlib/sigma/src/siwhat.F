*
* $Id: siwhat.F,v 1.1.1.1 1995/12/12 14:36:21 mclareni Exp $
*
* $Log: siwhat.F,v $
* Revision 1.1.1.1  1995/12/12 14:36:21  mclareni
* Imported sources
*
*
#include "sigma/pilot.h"
*CMZ :  1.10/00 26/10/93  14.36.04  by  Carlo E. Vandoni
*-- Author :
      LOGICAL FUNCTION SIWHAT(I)
#include "sigma/sicph1.inc"
      CHARACTER CSTRI*28
      CHARACTER NUMSTR*10
      CALL SITRAX(' SIWHAT   ')
C I=0, search for digit
C I=1, search for alpha
      SIWHAT=.FALSE.
      IF(I.EQ.1) GOTO 1
      NUMSTR='1234567890'
C     CHECK IF DIGIT
      IF(INDEX(NUMSTR,ISGN).NE.0) SIWHAT=.TRUE.
      GOTO 999
1     CSTRI='ABCDEFGHIJKLMNOPQRSTUVWXYZ?_'
C     CHECK IF ALPHABETIC   OR DOLLAR SIGN
      IF(INDEX(CSTRI,ISGN).NE.0) SIWHAT=.TRUE.
 999  END
