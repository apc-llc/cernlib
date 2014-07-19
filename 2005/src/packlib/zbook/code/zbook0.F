*
* $Id: zbook0.F,v 1.1.1.1 1996/03/08 12:01:11 mclareni Exp $
*
* $Log: zbook0.F,v $
* Revision 1.1.1.1  1996/03/08 12:01:11  mclareni
* Zbook
*
*
#include "zbook/pilot.h"
      SUBROUTINE ZBOOK0(IZ,ID,ND)
C
C     ******************************************************************
C     *                                                                *
C     *       SAME AS ZBOOK                                            *
C     *       PRESETS CONTENTS TO 0.                                   *
C     *                                                                *
C     ******************************************************************
C
      DIMENSION IZ(1),ID(1)
C
C     ------------------------------------------------------------------
C
      CALL ZBOOKN(IZ,ID,ND,0,'    ',0)
      RETURN
      END
