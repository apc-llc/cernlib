*
* $Id: icharn.F,v 1.1.1.1 1996/02/15 17:47:50 mclareni Exp $
*
* $Log: icharn.F,v $
* Revision 1.1.1.1  1996/02/15 17:47:50  mclareni
* Kernlib
*
*
#include "kernbit/pilot.h"
      FUNCTION ICHARN(CHN)
*
* CERN PROGLIB# M443    CHARN            .VERSION KERNBIT  1.07  910711
* Author: Miguel Marquina 85/02/26
*
* Build INTEGER ICHARN from CHARACTER string CHN
*
* Mods       Date   Comments
* MARQUINA 87/05/xx Recode in F77
*
      CHARACTER*(*) CHN
      CHARACTER*1 NULL,MINUS,IBLANK,IDOT
      EQUIVALENCE(B,NB)
      DATA NULL/'0'/,MINUS/'-'/,IDOT/'.'/,IBLANK/' '/

      LN=LEN(CHN)
      NB=0
      NP=1
      IR=0
      IF(LN.LT.1)           GOTO 100
      K1=LN+1
      K2=K1
      IS= 1
      IP=LN
      DO 50 K=1,LN
      K1=K1-1
      IF(CHN(K1:K1).EQ.IBLANK) GOTO 50
      K2=K2-1
      IF(CHN(K1:K1).NE.MINUS)  GOTO 10
      IS=-1
      GOTO 50
10    CONTINUE
      IF(CHN(K1:K1).NE.IDOT)   GOTO 20
      IP= K2
      IR=  1
      GOTO 50
20    CONTINUE
      NDIGIT=ICHAR(CHN(K1:K1))-ICHAR(NULL)
      NB=NB+NP*NDIGIT
      NP=NP*10
50    CONTINUE
      NB=NB*IS
      IF(IR.EQ.0) GOTO 100
      EXP=10**(LN-IP)
      B=NB/EXP
100   CONTINUE
      ICHARN=NB
      RETURN
      END
