*
* $Id: dzdglk.F,v 1.1.1.1 1996/03/04 16:13:09 mclareni Exp $
*
* $Log: dzdglk.F,v $
* Revision 1.1.1.1  1996/03/04 16:13:09  mclareni
* Dzdoc/Zebpack
*
*
#include "dzdoc/pilot.h"
      INTEGER FUNCTION DZDGLK(LB,IB,L,JB)
#include "dzdprm.inc"
      INTEGER LB(*),IB(*),L,JB,LL,I,MAXP,IC
      SAVE MAXP
      CHARACTER*4 CHID
      DATA MAXP/3/
      LL=LB(L-JB)
*     check only ref links
      IF(JB.LE.IB(L-2))GOTO 900
      IF(IDREFL.EQ.0)GOTO 889
      IF(LL.NE.0)THEN
         IF( IB(LL-1).LT.0 .OR. IB(LL-1).GT.65000
     +   .OR.IB(LL-2).LT.0 .OR. IB(LL-2).GT.65000
     +   .OR.IB(LL-3).LT.0 .OR. IB(LL-3).GT.65000)THEN
            GOTO 888
         ENDIF
         CALL UHTOC(IB(LL-4),4,CHID,4)
         DO 10 I=1,4
            IC=ICHAR(CHID(I:I))
            IF(IC.GE.ICHAR('A').AND.IC.LE.ICHAR('Z'))GOTO 10
            IF(IC.GE.ICHAR('a').AND.IC.LE.ICHAR('z'))GOTO 10
            IF(IC.GE.ICHAR('0').AND.IC.LE.ICHAR('9'))GOTO 10
            GOTO 888
 10      CONTINUE
      ENDIF
      GOTO 900
 888  CONTINUE
      IF(MAXP.GE.0)THEN
         WRITE(*,'(A,I6,A,I6)')
     +   ' Link ',LL, ' seems not to point to a bank, Jbias: ',JB
         IF(MAXP.EQ.0)WRITE(*,'(A)')' Will stop printing this message'
         MAXP=MAXP-1
      ENDIF
 889  LL=0
 900  DZDGLK=LL
      RETURN
      END
******************************************************************
