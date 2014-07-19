*
* $Id: siersy.F,v 1.1.1.1 1995/12/12 14:36:16 mclareni Exp $
*
* $Log: siersy.F,v $
* Revision 1.1.1.1  1995/12/12 14:36:16  mclareni
* Imported sources
*
*
#include "sigma/pilot.h"
*CMZ :  1.10/01 26/10/93  15.12.06  by  Carlo E. Vandoni
*-- Author :
      SUBROUTINE SIERSY(N)
*CDE,PAWC
*CDE,SIGC
#include "sigma/sicipa.inc"
C
*     IQUEST(1)=-N
C
      GO TO (1,2,3,4,5),N
 1    CONTINUE
      PRINT *,'----> Comp.error SYNTAX ERROR IN THIS STATEMENT. '
      GO TO 6
 2    CONTINUE
      PRINT *,'----> Comp.error WRONG SYNTAX OF NUMBER OR NAME'
      GO TO 6
 3    CONTINUE
      PRINT *,'----> Comp.error SYSTEM FUNCTION NAME ON L.H.S. OF EXPR.'
      GO TO 6
 4    CONTINUE
      PRINT *,'-'
      GO TO 6
 5    CONTINUE
      PRINT *,'----> Comp.error INCORRECT N.OF ARGUMENTS IN FUN.CALL'
 6    CONTINUE
C
      PRINT *, ' **** THE  OFFENDING STATEMENT IS'
      LE=LENOCC(CSTA1)
      PRINT *, CSTA1(1:LE)
C
      END
