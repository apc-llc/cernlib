*
* $Id: zidok.F,v 1.1.1.1 1996/03/08 12:01:12 mclareni Exp $
*
* $Log: zidok.F,v $
* Revision 1.1.1.1  1996/03/08 12:01:12  mclareni
* Zbook
*
*
#include "zbook/pilot.h"
      LOGICAL FUNCTION ZIDOK(IZ,ID)
C
C     ******************************************************************
C     *                                                                *
C     *                                                                *
C     *       CHECKS IF 'ID' IS A VALID POINTER                        *
C     *                                                                *
C     *                                                                *
C     ******************************************************************
C
      DIMENSION IZ(1)
C
C     ------------------------------------------------------------------
C
      ZIDOK  = .FALSE.
C
      IDS    = IZ(1)
      IF (ID.LT.1)                               GO TO 99
      IF (ID.GT.IZ(IDS + 15))                    GO TO 99
C
      ILAST  = ID + IZ(ID) + 2
      IF (ILAST.LT.1)                            GO TO 99
      IF (ILAST.GT.IZ(IDS + 15))                 GO TO 99
C
      NID    = IZ(ILAST)
      IF (NID.LT.IZ(IDS + 12))                   GO TO 99
      IF (NID.GT.IZ(IDS + 13))                   GO TO 99
      IF (IZ(NID).NE.ID)                         GO TO 99
C
      ZIDOK  = .TRUE.
C
   99 RETURN
      END
