*
* $Id: zbook.F,v 1.1.1.1 1996/03/08 12:01:11 mclareni Exp $
*
* $Log: zbook.F,v $
* Revision 1.1.1.1  1996/03/08 12:01:11  mclareni
* Zbook
*
*
#include "zbook/pilot.h"
      SUBROUTINE ZBOOK(IZ,ID,ND)
C
C     ******************************************************************
C     *                                                                *
C     *       CREATION OF A BANK (DATA ONLY)                           *
C     *                                                                *
C     ******************************************************************
C
      DIMENSION IZ(1),ID(1)
C
C     ------------------------------------------------------------------
C
      JZ     = IZ(1)
      IZ(JZ + 17) = 1
      CALL ZBOOKN(IZ,ID,ND,0,'    ',0)
      RETURN
      END
