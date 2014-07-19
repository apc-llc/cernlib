*
* $Id: zloc.F,v 1.1.1.1 1996/03/08 12:01:12 mclareni Exp $
*
* $Log: zloc.F,v $
* Revision 1.1.1.1  1996/03/08 12:01:12  mclareni
* Zbook
*
*
#include "zbook/pilot.h"
      SUBROUTINE ZLOC(IZ,NAME,NR,LOC)
C
C     ******************************************************************
C     *                                                                *
C     *       RETURN ADDRESS OF BANK (NAME,NR)                         *
C     *                                                                *
C     ******************************************************************
C
      DIMENSION IZ(1)
C
C     ------------------------------------------------------------------
C
      LOC    = 0
      JZ     = IZ(1)
      N      = IZ(JZ)
      IFIRST = JZ - IZ(JZ + N + 1) - 1
      CALL UCTOH(NAME,NAMEC,4,4)
  10  N      = IZ(IFIRST)
      IF (N.EQ.0)                                GO TO 99
      IF (N.LT.0)                                GO TO 20
      INAME  = IFIRST + N - 4
      IF (IZ(INAME).EQ.NAMEC.AND.IZ(INAME + 1).EQ.NR) GO TO 30
      IFIRST = IFIRST + N
      GO TO 10
  20  IFIRST = IFIRST - N
      GO TO 10
  30  LOC    = IFIRST + IZ(INAME + 2) + 1
  99  RETURN
      END
