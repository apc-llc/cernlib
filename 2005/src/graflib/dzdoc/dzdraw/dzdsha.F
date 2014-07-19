*
* $Id: dzdsha.F,v 1.1.1.1 1996/03/04 16:13:01 mclareni Exp $
*
* $Log: dzdsha.F,v $
* Revision 1.1.1.1  1996/03/04 16:13:01  mclareni
* Dzdoc/Zebpack
*
*
#include "dzdoc/pilot.h"
      SUBROUTINE DZDSHA
#include "dzdzbrinc.inc"
#include "zebra/zebq.inc"
#include "zebra/mzca.inc"
#include "zebra/mzcb.inc"
#include "zebra/mzcc.inc"
#include "zebra/mzcn.inc"
#include "zebra/mzbits.inc"
      INTEGER IHID, IUPHID, NID, IDIV, ISTORE, LGO, LUP, NPAR
      CHARACTER*4 CHID, CUPHID
      CHARACTER*80 CMD
*--
      CALL DZDIBR
      CALL KUGETC(CHID,NCH)
      IF(NCH.EQ.0)THEN
         WRITE(*,*)'No bank Id given'
         GOTO 999
      ENDIF
      CALL KUPATL(CMD,NPAR)
      CALL UCTOH(CHID,IHID,4,4)
      CALL KUGETI(NID)
      CALL KUGETI(ISTORE)
      IF(ISTORE.LT.0 .OR. ISTORE.GT.16)ISTORE=0
      CALL KUGETI(IDIV)
      IF(IDIV.LE.0 .OR. IDIV.GT.20)IDIV=2
      CUPHID=' '
      CALL KUGETC(CUPHID,NCH)
      IF(CUPHID.NE.' ')CALL UCTOH(CUPHID,IUPHID,4,4)
 
      IXDIV=IDIV
      CALL SBYT(ISTORE,IXDIV,27,6)
      CALL MZSDIV(IXDIV,1)
*     init link area for this store
      IF(LZEFLG(ISTORE).EQ.0)THEN
         CALL MZLINK(IXDIV,'DZDZEB',
     &   LZEBLK(1,ISTORE),LZEBLK(3,ISTORE),LZEBLK(3,ISTORE))
         LZEFLG(ISTORE)=1
      ENDIF
 
*     find the bank
 
      LGO=0
10    CONTINUE
      IF(NID.EQ.0)THEN
         LZEBLK(3,ISTORE)=LZFIDH(IXDIV,IHID,LGO)
      ELSE
         LZEBLK(3,ISTORE)=LZFID(IXDIV,IHID,NID,LGO)
      ENDIF
      IF(LZEBLK(3,ISTORE).EQ.0)THEN
         WRITE(*,'(A,A,I3,A,I3)')
     &   CHID,' not found in Store',ISTORE,' Division',IDIV
         GOTO 999
      ENDIF
      IF(CUPHID.NE.' ')THEN
         LGO=LZEBLK(3,ISTORE)
         LUP= LQ(LZEBLK(3,ISTORE)+1+KQS)
         IF(CUPHID.EQ.'NONE' .AND. LUP .NE. 0)GOTO 10
         IF(LUP.EQ.0)THEN
            WRITE(*,'(A,A,I3,A,I3)')
     &         CHID,' not found in Store',ISTORE,' Division',IDIV
               GOTO 999
         ENDIF
         IF(IQ(LUP-4+KQS).NE.IUPHID)GOTO 10
      ENDIF
      CALL DZDSH1(IXDIV,LZEBLK(3,ISTORE),'_'//CMD)
999   END
*********************************************************************
