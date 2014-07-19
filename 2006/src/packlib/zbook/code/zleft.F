*
* $Id: zleft.F,v 1.1.1.1 1996/03/08 12:01:12 mclareni Exp $
*
* $Log: zleft.F,v $
* Revision 1.1.1.1  1996/03/08 12:01:12  mclareni
* Zbook
*
*
#include "zbook/pilot.h"
      SUBROUTINE ZLEFT(IZ,NLEFT)
C
C     ******************************************************************
C     *                                                                *
C     *       RETURNS THE TOTAL NUMBER OF FREE LOCATIONS               *
C     *                                                                *
C     ******************************************************************
C
      DIMENSION IZ(1)
C
C     ------------------------------------------------------------------
C
      JZ     = IZ(1)
      NLEFT  = IZ(JZ + 5) + IZ(JZ + 1)
      RETURN
      END
