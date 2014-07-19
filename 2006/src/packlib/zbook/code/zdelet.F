*
* $Id: zdelet.F,v 1.1.1.1 1996/03/08 12:01:11 mclareni Exp $
*
* $Log: zdelet.F,v $
* Revision 1.1.1.1  1996/03/08 12:01:11  mclareni
* Zbook
*
*
#include "zbook/pilot.h"
      SUBROUTINE ZDELET(IZ,ID)
C
C     ******************************************************************
C     *                                                                *
C     *       DELETES BANK ID                                          *
C     *                                                                *
C     ******************************************************************
C
      DIMENSION IZ(1),ID(1)
      LOGICAL ZIDOK
C
C     ------------------------------------------------------------------
C
      JZ     = IZ(1)
      IF (ZIDOK(IZ,ID))                          GO TO 5
      CALL ZERROR(IZ,300,'ZDELET',ID)
      RETURN
   5  IZ(JZ + 6) = 0
      IDATA=ID(1)
      ILINK=IDATA+IZ(IDATA)+1
      IF (IZ(ILINK).NE.0)                        GO TO 20
      IFNEW  = ILINK + 2
      IFIRST=IDATA-1
      NLESS  = IZ(IFIRST)
      NID    = IZ(ILINK + 1)
      IF (IZ(JZ - 1).EQ.0)                       GO TO 10
C
C             UPDATE LOCAL POINTERS
C
      CALL ZUPLOC(IZ,IFIRST,ILINK,0)
C
  10  IMAX   = IZ(JZ + 15)
      CALL ZSHIFT(IZ,IFNEW,IMAX,-NLESS,'ZDELET')
      IF (IZ(JZ + 6).NE.0)RETURN
C
      ID(1)  = 0
      IZ(NID) = 0
      RETURN
C
C             BANK OF POINTERS
C
  20  CALL ZDROP(IZ,ID)
      CALL ZGARB(IZ)
      RETURN
      END
