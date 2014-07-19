*
* $Id: zslism1.F,v 1.1.1.1 1996/03/08 15:44:22 mclareni Exp $
*
* $Log: zslism1.F,v $
* Revision 1.1.1.1  1996/03/08 15:44:22  mclareni
* Cspack
*
*
#include "cspack/pilot.h"
#if defined(CERNLIB_MMAP)
      SUBROUTINE ZSLISM1(IB,LB,ID1,JTAB,KOF,ISTAT)
*.==========>
*.           Auxiliary for ZSLISM
*..=========> ( R.Brun)
#include "cspack/hcbook.inc"
#include "cspack/hcntpar.inc"
      DIMENSION IB(1),LB(1)
      CHARACTER*128 LOUT
      CHARACTER*1 HTYPE
*.___________________________________________
      NH=IB(JTAB-1)
      DO 10 I=1,NH
         IF(IB(JTAB+I).EQ.ID1)GO TO 20
  10  CONTINUE
      GO TO 99
*
  20  CONTINUE
C     ID=ID1
      JCID=LB(JTAB-I)-KOF
      NTOT=IB(JCID+KNTOT)
      I123=JBYT(IB(JCID+KBITS),1,3)
      I230=JBYT(IB(JCID+KBITS),2,2)
      I1  =JBIT(I123,1)
      I4  =JBIT(IB(JCID+KBITS),4)
*
      IF(I1.NE.0)THEN
         NWTITL=IB(JCID-1)-KTIT1+1
         ITIT1=JCID+KTIT1
         HTYPE='1'
      ELSEIF(I230.NE.0)THEN
         NWTITL=IB(JCID-1)-KTIT2+1
         ITIT1=JCID+KTIT2
         HTYPE='2'
      ELSEIF(I4.NE.0)THEN
         HTYPE='N'
         IF (IB(JCID-2) .EQ. 2) THEN
            ITIT1=JCID+IB(JCID+9)
            NWTITL=IB(JCID+8)
         ELSE
            ITIT1=JCID+IB(JCID+ZITIT1)
            NWTITL=IB(JCID+ZNWTIT)
         ENDIF
      ELSE
         GO TO 99
      ENDIF
      LOUT=' '
      WRITE (LOUT,1001) ID1,HTYPE,(IB(KK),KK=ITIT1,ITIT1+NWTITL-1)
      CALL CZPUTA(LOUT,ISTAT)
1001  FORMAT('2',I10,1X,'(',A,')',3X,15A4)
*
  90  CONTINUE
*
  99  RETURN
      END
#endif
