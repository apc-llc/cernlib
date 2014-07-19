*
* $Id: csdpro.F,v 1.1.1.1 1996/02/26 17:16:16 mclareni Exp $
*
* $Log: csdpro.F,v $
* Revision 1.1.1.1  1996/02/26 17:16:16  mclareni
* Comis
*
*
#include "comis/pilot.h"
*CMZ :  1.18/14 09/11/94  11.34.06  by  Vladimir Berezhnoi
*-- Author : V.Berezhnoi
       SUBROUTINE CSDPRO(IT)
***------------------------
*        IT is 'address' of comis routine
*        removes routine from comis-memory
***------------------------
#include "comis/cstabps.inc"
      COMMON/MDPOOL/IQ(99)
      IF(IT.LE.0)RETURN
*+SELF,IF=PAW.
      CALL CSLISTD(IT)
*+SELF.
      CALL CSRMCL(IT)
      IP=IQ(IT+KSADGP)
      IF(IQ(IT+KSIFCS).GT.0)THEN
        IF(IQ(IP-1).LT.0)RETURN
        IB=IP-4
        IDES=IQ(IP-2)
  1     IF(IDES.NE.0)THEN
          I=IQ(IDES)
          CALL MHFREE(IDES)
          IDES=I
          GO TO 1
        ENDIF
        CALL CSLFRE(IQ(IB))
        CALL MHFREE(IB)
        IQ(IT+KSADGP)=0
        IQ(IT+KSIFCS)=0
      ELSEIF(IQ(IT+KSIFCS).EQ.-2)THEN
*                    shl-routine
        IQ(IT+KSADGP)=0
        IQ(IT+KSIFCS)=0
      ENDIF
      END
