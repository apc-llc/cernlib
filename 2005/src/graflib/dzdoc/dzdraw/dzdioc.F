*
* $Id: dzdioc.F,v 1.1.1.1 1996/03/04 16:13:07 mclareni Exp $
*
* $Log: dzdioc.F,v $
* Revision 1.1.1.1  1996/03/04 16:13:07  mclareni
* Dzdoc/Zebpack
*
*
#include "dzdoc/pilot.h"
      SUBROUTINE DZDIOC(IOW,CQLINE,NCH)
      CHARACTER*(*) CQLINE
      INTEGER IOW(9)
      COMMON/QUEST/IQUEST(100)
#include "zebra/mzioc.inc"
*  BANK FORMAT PARAMETERS  (IO CHARACERISTIC)
#include "zebra/bkfoparq.inc"
      CHARACTER   KFOTYP(0:11)*1
      DATA KFOTYP /'U','B','I','F','D','H','*','S','*','N','*','L'/
      CALL MZIOCR(IOW)
      MAXNCH=LEN(CQLINE)
      CQLINE=' '
      IPOS = 1
      DO 100 JFOCUR = 1,JFOEND,2
          IF (JFOCUR.EQ.JFOREP+1) THEN
              WRITE(CQLINE(IPOS:IPOS+1),'(''/ '')')
              IPOS = IPOS + 2
              IF(IPOS.GE.MAXNCH)GOTO 888
          ENDIF
          ITYPE  = MFO(JFOCUR)
          IF (ITYPE.EQ.IFOSEQ) THEN
              WRITE(CQLINE(IPOS:IPOS+1),'(''*'',A1)') KFOTYP(ITYPE)
              IPOS = IPOS + 3
              IF(IPOS.GE.MAXNCH)GOTO 888
                                                           GO TO  100
          ENDIF
          NWSEC  = MFO(JFOCUR+1)
          ITYPE  = MIN(ITYPE,8)
          IF (NWSEC.LT.0) THEN
              WRITE(CQLINE(IPOS:IPOS+1),'(''-'',A1)') KFOTYP(ITYPE)
              IPOS = IPOS + 3
              IF(IPOS.GE.MAXNCH)GOTO 888
          ELSEIF (NWSEC.EQ.0) THEN
              WRITE(CQLINE(IPOS:IPOS+1),'(''*'',A1)') KFOTYP(ITYPE)
              IPOS = IPOS + 3
              IF(IPOS.GE.MAXNCH)GOTO 888
          ELSE
              DO 10 I=1,100
                  IF(NWSEC.EQ.0)                           GO TO 20
                      IQUEST(I)=MOD(NWSEC,10)
                      NWSEC    = NWSEC/10
   10         CONTINUE
   20         DO 30 J=1,I-1
                  WRITE(CQLINE(IPOS:IPOS),'(I1)') IQUEST(I-J)
                  IPOS = IPOS + 1
                  IF(IPOS.GE.MAXNCH)GOTO 888
   30         CONTINUE
              WRITE(CQLINE(IPOS:IPOS),'(A1)') KFOTYP(ITYPE)
              IPOS = IPOS + 2
              IF(IPOS.GE.MAXNCH)GOTO 888
          ENDIF
  100 CONTINUE
      NCH=IPOS-1
      GOTO 999
  888 CONTINUE
      CQLINE=' too complicated (long)'
      NCH=22
  999 RETURN
      END
********************************************************************
