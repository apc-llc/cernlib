*
* $Id: iomarn32.F,v 1.1.1.1 1996/03/08 15:21:58 mclareni Exp $
*
* $Log: iomarn32.F,v $
* Revision 1.1.1.1  1996/03/08 15:21:58  mclareni
* Epio
*
*
#include "epio/pilot.h"
#if defined(CERNLIB_ND50)||defined(CERNLIB_ND500)
      SUBROUTINE IOMARK(LUNIT,IRET)
C     *****************************
C   WRITE EOF
C   LUNIT  = LOGICAL DEVICE NUMBER
C   IRET   = RETURN STATUS
      INTEGER*4 LUNIT,IRET,ZERO
      INTEGER*4 MFUNC,MAGTPZ,DUM,MSTAT,IORCNV
      DATA ZERO/0/
      MFUNC=12B
      IRET=MAGTPZ(MFUNC,DUM,LUNIT,DUM,DUM,MSTAT)
      IF(IRET.EQ.ZERO) GOTO 77777
      IRET=IORCNV(IRET)
77777 RETURN
      END
#endif
