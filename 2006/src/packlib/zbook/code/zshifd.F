*
* $Id: zshifd.F,v 1.1.1.1 1996/03/08 12:01:13 mclareni Exp $
*
* $Log: zshifd.F,v $
* Revision 1.1.1.1  1996/03/08 12:01:13  mclareni
* Zbook
*
*
#include "zbook/pilot.h"
      SUBROUTINE ZSHIFD(IZ,IDATA,ILAST,NPUSH,NDP)
C
C     ******************************************************************
C     *                                                                *
C     *       SHIFT DATA AREA IN BANK POINTED BY IDATA                 *
C     *                                                                *
C     ******************************************************************
C
      DIMENSION IZ(1)
C
C     ------------------------------------------------------------------
C
      JZ     = IZ(1)
      ND=IZ(IDATA)
      IF (NPUSH.EQ.0)                            GO TO 10
      CALL UCOPY2(IZ(IDATA),IZ(IDATA+NPUSH),ND-1)
      IF(IZ(JZ-1).NE.0)CALL ZUPLOC(IZ,IDATA,ILAST,NPUSH)
      IDATA=IDATA+NPUSH
C
  10  IF (NDP.EQ.0)                              GO TO 99
      IZ(IDATA)=IZ(IDATA)+NDP
      IF (IZ(JZ + 17).NE.0)                      GO TO 99
      IF (NDP.LE.0)                              GO TO 99
#if !defined(CERNLIB_BESM6)
      CALL VZERO(IZ(IDATA+ND-1),NDP)
#endif
#if defined(CERNLIB_BESM6)
      CALL UZERO(IZ(IDATA+ND-1),1,NDP)
#endif
C
  99  RETURN
      END
