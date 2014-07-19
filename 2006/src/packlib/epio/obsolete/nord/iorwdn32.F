*
* $Id: iorwdn32.F,v 1.1.1.1 1996/03/08 15:21:58 mclareni Exp $
*
* $Log: iorwdn32.F,v $
* Revision 1.1.1.1  1996/03/08 15:21:58  mclareni
* Epio
*
*
#include "epio/pilot.h"
#if defined(CERNLIB_ND50)||defined(CERNLIB_ND500)
      SUBROUTINE IORWND(LUNIT,IRET)
C     *****************************
C   REWIND
C   LUNIT  = LOGICAL DEVICE NUMBER
C   IRET   = RETURN STATUS
      INTEGER*4 LUNIT,IRET,ZERO
      INTEGER*4 MFUNC,MAGTPZ,DUM,MSTAT,IORCNV
      DATA ZERO/0/
      MFUNC=13B
      IRET=MAGTPZ(MFUNC,DUM,LUNIT,DUM,DUM,MSTAT)
      IF(IRET.NE.ZERO) IRET=1
77777 RETURN
      END
#endif
