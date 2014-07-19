*
* $Id: rzcget.F,v 1.1.1.1 1996/03/06 10:47:04 mclareni Exp $
*
* $Log: rzcget.F,v $
* Revision 1.1.1.1  1996/03/06 10:47:04  mclareni
* Zebra
*
*
#if defined(CERNLIB_INTER)
#include "test_include/pilot.h"
      SUBROUTINE RZCGET(CHP,NWKEY,CHFORM,CHTAG,KEY)
*
********************************************************************************
*                                                                              *
*        Routine called by interactive test program to prompt for a key        *
*        according to the format CHFORM and key name CHTAG                     *
*        CHP is the beginning of the prompt                                    *
*        NWKEY the number of keys                                              *
*        Output is returned in array KEY.                                      *
*    Called by RZT                                                             *
*                                                                              *
*  Author  : R.Brun                                                            *
*  Written : 25.05.86                                                          *
*  Last mod: 25.05.86                                                          *
*                                                                              *
********************************************************************************
*
      CHARACTER*(*) CHP,CHFORM
      CHARACTER*8 CHTAG(NWKEY)
      CHARACTER*64 CHPROM
      CHARACTER*64 CHCOPY
      DIMENSION IHPROM(16),KEY(*)
*
*     ---------------------------------------------------------------------
*
      I=0
      NCHP=LEN(CHP)
      NCHPL=NCHP
      DO 5 J=NCHPL,1,-1
         IF(CHP(J:J).NE.' ')GO TO 10
         NCHP=J
   5  CONTINUE
  10  I=I+1
      IF(I.GT.NWKEY)GO TO 99
      CHPROM=CHP(1:NCHP)//' '//CHTAG(I)//' $ '
      CALL UCTOH(CHPROM,IHPROM,4,64)
      IF(CHFORM(I:I).EQ.'I')THEN
         CALL ZCGETI(IHPROM,KEY(I))
      ELSEIF(CHFORM(I:I).EQ.'B')THEN
         CALL ZCGETI(IHPROM,KEY(I))
      ELSEIF(CHFORM(I:I).EQ.'H')THEN
         NCH=-4
         CALL ZCGETA(IHPROM,KEY(I),NCH)
      ELSE
         K=1
         L=NCHP+9
         CHPROM=CHP(1:NCHP)//' '//CHTAG(I)//' $ '
         DO 20 J=I+1,NWKEY
            IF(CHFORM(J:J).NE.'A')GO TO 30
            K=K+1
            CHCOPY=CHPROM(1:L)//CHTAG(J)//' $ '
            CHPROM=CHCOPY
            L=L+8
  20     CONTINUE
  30     NCH=-4*K
         CALL UCTOH(CHPROM,IHPROM,4,64)
         CALL VBLANK(KEY(I),K)
         CALL ZCGETA(IHPROM,KEY(I),NCH)
         I=I+K-1
      ENDIF
      GO TO 10
*
  99  RETURN
      END
#endif
