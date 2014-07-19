*
* $Id: dzdnfz.F,v 1.1.1.1 1996/03/04 16:13:03 mclareni Exp $
*
* $Log: dzdnfz.F,v $
* Revision 1.1.1.1  1996/03/04 16:13:03  mclareni
* Dzdoc/Zebpack
*
*
#include "dzdoc/pilot.h"
      SUBROUTINE DZDNFZ(CDIR,CLUN,ICONT,IRET)
#include "zebra/mqsys.inc"
#include "zebra/eqlqf.inc"
      CHARACTER*(*)  CDIR,CLUN
      INTEGER ICONT,IRET, LFZ
      SAVE LFZ
 
*     get next top directory (FZfile)
 
      IF(ICONT.EQ.0)THEN
         IF(LQFS.EQ.0)THEN
            ICONT=0
            IRET=-2
            GOTO 999
         ELSE
            LFZ=LQFS
         ENDIF
      ELSE
         LFZ=LQ(KQSP+LFZ)
      ENDIF
 
      IF(LFZ.EQ.0)THEN
         ICONT=0
         IRET=-1
      ELSE
         ICONT=ICONT+1
         IRET=0
         WRITE(CDIR,'(I2.2)')IQ(KQSP+LFZ-5)
         WRITE(CLUN,'(I2)')IQ(KQSP+LFZ-5)
      ENDIF
999   END
***********************************************************************
