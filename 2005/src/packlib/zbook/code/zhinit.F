*
* $Id: zhinit.F,v 1.1.1.1 1996/03/08 12:01:12 mclareni Exp $
*
* $Log: zhinit.F,v $
* Revision 1.1.1.1  1996/03/08 12:01:12  mclareni
* Zbook
*
*
#include "zbook/pilot.h"
      SUBROUTINE ZHINIT(IZ,IW,NWORDS)
C
C     ******************************************************************
C     *                                                                *
C     *       INITIALISES A DYNAMIC AREA IN BLANK COMMON               *
C     *       EXTEND MEMORY  USING THE HBOOK ROUTINE HISTGO            *
C     *                                                                *
C     ******************************************************************
C
      DIMENSION IZ(1),IW(1)
C
C     ------------------------------------------------------------------
C
      CALL HISTGO(NWORDS)
C
      CALL ZINIT(IZ,IW,NWORDS)
      JZ     = IZ(1)
      IZ(JZ + 11) = 1000000
      RETURN
      END
