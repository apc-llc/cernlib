*
* $Id: hbigbi.F,v 1.1.1.1 1996/01/16 17:07:32 mclareni Exp $
*
* $Log: hbigbi.F,v $
* Revision 1.1.1.1  1996/01/16 17:07:32  mclareni
* First import
*
*
#include "hbook/pilot.h"
*CMZ :  4.10/05 21/02/89  16.14.45  by  Rene Brun
*-- Author :
      SUBROUTINE HBIGBI(ID2,NSTEP)
*.==========>
*.           one channel will be printed over NSTEP columns
*..=========> ( R.Brun )
#include "hbook/hcbits.inc"
#include "hbook/hcbook.inc"
      COMMON/HCGARB/IS,NSTEP0,IRET,NARG,GA(13)
*.___________________________________________
      NARG=2
      CALL NOARG(NARG)
      NSTEP0=0
      IF(NARG.EQ.2)NSTEP0=INTARG(NSTEP)
      IF(NSTEP0.LT.1)NSTEP0=1
      IRET=3
*
  10  CALL HLOOP(ID2,'HBIGBI',IRET)
      IF(IRET.EQ.0)GO TO 99
*
      CALL HDCOFL
      IF(I123.NE.0)THEN
         CALL HSIFLA(17,1)
         CALL SBYT(NSTEP0,IQ(LCID),1,4)
      ENDIF
      IRET=2
      GO TO 10
*
  99  RETURN
      END
