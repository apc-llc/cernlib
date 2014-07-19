*
* $Id: hfontr.F,v 1.1.1.1 1996/01/16 17:08:11 mclareni Exp $
*
* $Log: hfontr.F,v $
* Revision 1.1.1.1  1996/01/16 17:08:11  mclareni
* First import
*
*
#include "sys/CERNLIB_machine.h"
#include "_hbook/pilot.h"
*CMZ :  4.21/08 17/01/94  12.37.34  by  Rene Brun
*-- Author :
      SUBROUTINE HFONTR(ID,N,X,Y,Z,W,CHOPT)
*.==========>
*.           To fill /HCBONT/ if CHOPT=' '
*.           To copy /HCBONT/ in X,Y,Z,W if CHOPT='R'
*..=========> ( R. Brun )
*
#include "hcbook.inc"
#include "hcbont.inc"
      DIMENSION X(1),Y(1),Z(1),W(1)
      CHARACTER*(*) CHOPT
*___________________________________________________________________
*
      IDPOS=LOCATI(IQ(LTAB+1),IQ(LCDIR+KNRH),ID)
      IF(IDPOS.LE.0)THEN
         CALL HBUG('Unknown histogram','HFONTR',ID)
         RETURN
      ENDIF
      LC=LQ(LTAB-IDPOS)
      NAR=JBYT(IQ(LC+KBITS),5,3)
      IF(INDEX(CHOPT,'R').NE.0)GO TO 15
*
      IF(NAR.LE.0)RETURN
      NONT=MIN(N,MAXNP)
      DO 10 I=1,NONT
         IF(NAR.GE.1)XONT(I)=X(I)
         IF(NAR.GE.2)YONT(I)=Y(I)
         IF(NAR.GE.3)ZONT(I)=Z(I)
         IF(NAR.GE.4)WONT(I)=W(I)
  10  CONTINUE
      RETURN
*
  15  N=NONT
      IF(NAR.LE.0)N=0
      DO 20 I=1,NONT
         IF(NAR.GE.1)X(I)=XONT(I)
         IF(NAR.GE.2)Y(I)=YONT(I)
         IF(NAR.GE.3)Z(I)=ZONT(I)
         IF(NAR.GE.4)W(I)=WONT(I)
  20  CONTINUE
      END
