*
* $Id: zreset.F,v 1.1.1.1 1996/03/08 12:01:13 mclareni Exp $
*
* $Log: zreset.F,v $
* Revision 1.1.1.1  1996/03/08 12:01:13  mclareni
* Zbook
*
*
#include "zbook/pilot.h"
      SUBROUTINE ZRESET(IZ,ID)
C
C     ******************************************************************
C     *                                                                *
C     *       CLEARS THE CONTENT OF BANK ID                            *
C     *                                                                *
C     ******************************************************************
C
      DIMENSION IZ(1),ID(1)
      LOGICAL ZIDOK
C
C     ------------------------------------------------------------------
C
      IDATA=ID(1)
      IF (ZIDOK(IZ,ID))                          GO TO 10
      CALL ZERROR(IZ,300,'ZRESET',ID)
      RETURN
C
#if !defined(CERNLIB_BESM6)
  10  CALL VZERO(IZ(IDATA+1),IZ(IDATA)-2)
#endif
#if defined(CERNLIB_BESM6)
  10  CALL UZERO(IZ(IDATA+1),1,IZ(IDATA)-2)
#endif
      RETURN
      END
