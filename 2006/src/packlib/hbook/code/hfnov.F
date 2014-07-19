*
* $Id: hfnov.F,v 1.1.1.1 1996/01/16 17:07:37 mclareni Exp $
*
* $Log: hfnov.F,v $
* Revision 1.1.1.1  1996/01/16 17:07:37  mclareni
* First import
*
*
#include "hbook/pilot.h"
*CMZ :  4.17/00 10/02/92  15.07.20  by  Fons Rademakers
*-- Author :    Rene Brun   20/02/90
      SUBROUTINE HFNOV(ID1,X)
*.==========>
*.           To fill a memory-resident Ntuple
*.           When the memory buffer gets full,
*.           the first events are overwritten
*.
*..=========> ( R.Brun )
#include "hbook/hcflag.inc"
#include "hbook/hcbook.inc"
#include "hbook/hcbits.inc"
      DIMENSION X(*)
*.___________________________________________
*
      IF(ID1.NE.IDLAST)THEN
         ID=ID1
         IDPOS=LOCATI(IQ(LTAB+1),IQ(LCDIR+KNRH),ID)
         IF(IDPOS.LE.0)RETURN
         IDLAST=ID1
         LCID=LQ(LTAB-IDPOS)
         LCONT=LQ(LCID-1)
         I4=JBIT(IQ(LCID+KBITS),4)
         IF(I4.EQ.0)RETURN
         IF (IQ(LCID-2) .NE. 2) THEN
            CALL HBUG('New N-tuple, can not use HFNOV',
     +                'HFNOV',ID)
            RETURN
         ENDIF
      ENDIF
*
      NDIM=IQ(LCID+2)
      IFIRST=IQ(LCID+7)
      NMAX=IQ(LCID+4)/NDIM
      NCHRZ=IQ(LCID+11)
      IF(NCHRZ.NE.0)THEN
         CALL HBUG('HFNOV used with a disk-resident Ntuple','HFNOV',ID1)
         GO TO 99
      ENDIF
*
      LLIMS=LQ(LCID-2)
      LR2=LQ(LCID-3)
      DO 10 I=1,NDIM
         IF(X(I).LT.Q(LLIMS+2*I-1))Q(LLIMS+2*I-1)=X(I)
         IF(X(I).GT.Q(LLIMS+2*I  ))Q(LLIMS+2*I  )=X(I)
         Q(LR2+IFIRST+I-1)=X(I)
  10  CONTINUE
      IF(IQ(LCID+3).LT.NMAX) IQ(LCID+3)=IQ(LCID+3)+1
      IQ(LCID+7)=IFIRST+NDIM
      IF(IQ(LCID+7).GT.IQ(LCID+4))IQ(LCID+7)=1
*
  99  RETURN
      END
