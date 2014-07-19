*
* $Id: hrzkey.F,v 1.1.1.1 1996/01/16 17:08:08 mclareni Exp $
*
* $Log: hrzkey.F,v $
* Revision 1.1.1.1  1996/01/16 17:08:08  mclareni
* First import
*
*
#include "hbook/pilot.h"
*CMZ :  4.21/09 19/02/94  18.20.45  by  Rene Brun
*-- Author :    Rene Brun   24/11/93
      SUBROUTINE HRZKEY(IDN)
*.==========>
*.           Fill data structure with key serial numbers corresponding
*.           to ntuple extensions.
*..=========> ( R.Brun )
#include "hbook/hcbook.inc"
#include "hbook/hcdire.inc"
      COMMON/QUEST/IQUEST(100)
      EQUIVALENCE (LK1,LHDUM(8))
      EQUIVALENCE (LK2,LHDUM(9))
*.___________________________________________
*
      CALL HRZIN(IHDIV,0,0,1,9999,'SC')
      NKEYS=IQUEST(7)
      NEXT=IQ(LCID+6)
*-*  do not built the structure if too few extensions or
*-*      if the ratio extensions/keys is too small
      IF(NEXT.LT.20)GO TO 99
      IF(10*NEXT.LT.NKEYS)GO TO 99
*-*  create (or extend) structure to store key serial numbers
      LK1=LQ(LCID-1)
      LK2=LQ(LK1)
      IF(LK2.LE.0)THEN
         CALL MZBOOK(IHDIV,LK2,LK1,0,'HKEY',0,0,NEXT,2,0)
      ELSE
         IF(IQ(LK2-1).LT.NEXT)THEN
            CALL MZPUSH(IHDIV,LK2,0,NEXT-IQ(LK2-1),' ')
         ENDIF
      ENDIF
*
*-*  Now scan current RZ directory
*
      DO 10 KEYRZ=1,NKEYS
         CALL HRZIN(IHDIV,0,0,KEYRZ,9999,'SC')
         IF(ICHTYP(ICDIR).EQ.1)THEN
            IEXT1=MOD(IQUEST(21),10000)
            IF(IEXT1.NE.IDN)GO TO 10
            IEXT=(IQUEST(21)-IEXT1)/10000
         ELSE
            IF(IQUEST(21).NE.IDN)GO TO 10
            IEXT=IQUEST(22)
         ENDIF
         IF(IEXT.LE.0.OR.IEXT.GT.NEXT)GO TO 10
         IQ(LK2+IEXT)=KEYRZ
  10  CONTINUE
*
  99  END
