*
* $Id: dzdstk.F,v 1.1.1.1 1996/03/04 16:13:17 mclareni Exp $
*
* $Log: dzdstk.F,v $
* Revision 1.1.1.1  1996/03/04 16:13:17  mclareni
* Dzdoc/Zebpack
*
*
#include "dzdoc/pilot.h"
      SUBROUTINE DZDSTK(IXSTOR,NK1,NK2,LRET)
      INTEGER LRET(9)
#include "zebra/rzcl.inc"
#include "zebra/rzk.inc"
      IF(NK1.GT.NK2)GOTO 999
      IF(LCDIR.EQ.0)GO TO 999
      LCOLD=0
      LK   = IQ(KQS+LCDIR+KLK)
      NK   = IQ(KQS+LCDIR+KNKEYS)
      NWKEY= IQ(KQS+LCDIR+KNWKEY)
      IF(NK1.LT.1)NK1=1
      IF(NK2.GT.NK)NK2=NK
*     remove previously booked banks
      NL = IQ(KQS+LRET(1)-1)
#include "zebra/qstore.inc"
*     book a bank for each key
*     format:
*     FLAG (=2), TSTAMP, KEYSEQ, CYCLE, NDATA, NWKEYS,
*     KEYDEF((NWKEYS+3/4)), KEYS(NWKEYS)
      IF(NK.LE.0)GOTO 999
      ND = NWKEY+6+(NWKEY+3)/4
 
*     taken from  RZKEYS
      NC=0
      DO 20 I=NK,1,-1
          IF(I.LT.NK1)GOTO 999
          K=LK+(NWKEY+1)*(I-1)
          LCYC=IQ(KQS+LCDIR+K)
  30      NC=NC+1
          IF(I.GT.NK2 .OR. I.LT. NK1)GOTO 26
          IF(LQ(KQS+LRET(1)-NC).NE.0)GOTO 26
          LCOLD = JBYT(IQ(KQS+LCDIR+LCYC ), 1,16)
          ITSTAM=IQ(KQS+LCDIR+LCYC+1)
          IC    = JBYT(IQ(KQS+LCDIR+LCYC+3),21,12)
          NW    = JBYT(IQ(KQS+LCDIR+LCYC+3), 1,20)
#include "zebra/qstore.inc"
          CALL MZBOOK(IXSTOR,LL,LRET(1),-NC,'RKEY',0,0,ND,0,0)
          LL=LQ(KQS+LRET(1)-NC)
          IQ(KQS+LL+1) = 2
          IQ(KQS+LL+2) = ITSTAM
          IQ(KQS+LL+3) = I
          IQ(KQS+LL+4) = IC
          IQ(KQS+LL+5) = NW
          IQ(KQS+LL+6) = NWKEY
          NWF=(NWKEY+3)/4
          CALL UCOPY(IQ(KQS+LRET(1)+7),IQ(KQS+LL+7),NWF)
          NFKEY = 5+(NWKEY+3)/4+1
*          CALL UCOPY(IQ(KQS+LCDIR+LKC+1),IQ(KQS+LL+NFKEY),NWKEY)
          DO 25 J=1,NWKEY
            IKDES=(J-1)/10
            IKBIT1=3*J-30*IKDES-2
            IF(JBYT(IQ(KQS+LCDIR+KKDES+IKDES),IKBIT1,3).LT.3)THEN
                IQ(KQS+LL+NFKEY+J)=IQ(KQS+LCDIR+K+J)
             ELSE
                CALL ZITOH(IQ(KQS+LCDIR+K+J),IQ(KQS+LL+NFKEY+J),1)
             ENDIF
  25      CONTINUE
  26      IF(LCOLD.NE.0)THEN
             LCYC=LCOLD
             GO TO 30
          ENDIF
   20 CONTINUE
 999  RETURN
      END
 
