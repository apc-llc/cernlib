*
* $Id: recovz.F,v 1.1.1.1 1996/03/08 12:01:14 mclareni Exp $
*
* $Log: recovz.F,v $
* Revision 1.1.1.1  1996/03/08 12:01:14  mclareni
* Zbook
*
*
#include "zbook/pilot.h"
      SUBROUTINE RECOVZ(Z,IERROR,LABEL,ID)
C
      DIMENSION Z(1), LABEL(1), ILABEL(6)
C
      CALL UCTOH1(LABEL,ILABEL,6)
C
      WRITE(6,1010)
      WRITE(6,1020)
      WRITE(6,1030) ILABEL, IERROR
      WRITE(6,1020)
      WRITE(6,1010)
      WRITE(6,1040)
C
      RETURN
 1010 FORMAT (1X,80('*'))
 1020 FORMAT (1X,20('*'),40X,20('*'))
 1030 FORMAT (1X,20('*'),6X,'CALL ',6A1,'(Z,...) ERROR',I4,6X,20('*'))
 1040 FORMAT ('0')
      END
