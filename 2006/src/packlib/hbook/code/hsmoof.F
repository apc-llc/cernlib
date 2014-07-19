*
* $Id: hsmoof.F,v 1.1.1.1 1996/01/16 17:07:48 mclareni Exp $
*
* $Log: hsmoof.F,v $
* Revision 1.1.1.1  1996/01/16 17:07:48  mclareni
* First import
*
*
#include "hbook/pilot.h"
*CMZ :  4.22/11 23/08/94  14.17.45  by  Rene Brun
*-- Author :
      SUBROUTINE HSMOOF(IDD,ICASS,SUMQR)
*.==========>
*.           THIS ROUTINE SMOOTHS AN HISTOGRAM IDD FOLLOWING
*.           THE METHOD DESCRIBED BY J.FRIEDMAN IN THE PROCEEDINGS
*.           OF THE C.E.R.N SCHOOL OF COMPUTING AT BERGEN
*.           IN AUGUST 1974.THE ALGORITHM USED IS THE 353QH,TWICE
*.
*.           ICASE=1  THE CONTENTS OF IDD ARE REPLACED BY THE
*.                    SMOOTHED VALUES
*.           ICASE=2  THE SMOOTHED VALUES ARE SUPERIMPOSED TO IDD
*..=========> ( R.Brun )
#include "hbook/hcbook.inc"
#include "hbook/hcflag.inc"
#include "hbook/hcbits.inc"
#include "hbook/hcprin.inc"
      DIMENSION X(6)
*.___________________________________________
      ICASE=1
      NARG=3
      CALL NOARG(NARG)
      IF(NARG.NE.1)ICASE=ICASS
      NOENT=1
      CALL HFIND(IDD,'HSMOOF')
      IF(LCID.EQ.0)GO TO 99
      CALL SBIT0(IQ(LCID),5)
      CALL HDCOFL
      IF(I1.EQ.0.AND.LFIX.EQ.0)THEN
         CALL HBUG('Not a 1-DIM histogram','HSMOOF',IDD)
         GO TO 99
      ENDIF
*
      NCHAN=IQ(LPRX)
      NV=1
      IF(ICASE.EQ.2)CALL HFUNC(IDD,SQRT)
      NV=2
*
      CALL HWORK(3*NCHAN,NOLD,'HSMOOF')
*
      IF(NOLD.EQ.0)GO TO 99
      CALL HUNPAK(ID,Q(NOLD),'HIST',1)
*
    5 CONTINUE
*
      DO 30 K=1,3
         CALL UCOPY2(Q(NOLD),Q(NOLD+NCHAN),NCHAN)
         IK=0
         IF(K.EQ.2)IK=1
         N1=IK+2
         N2=NCHAN-IK-1
*
         DO 10 I=N1,N2
            CALL UCOPY2(Q(NOLD+NCHAN+I-IK-2),X,3+2*IK)
            Q(NOLD+I-1)=HMEDIA(X,3+2*IK)
   10    CONTINUE
         GO TO (15,20,30),K
*
   15    X(3)=Q(NOLD+1)
         X(2)=Q(NOLD)
         X(1)=3.*X(3)-2.*Q(NOLD+2)
         Q(NOLD)=HMEDIA(X,3)
*
         X(1)=Q(NOLD+NCHAN-2)
         X(2)=Q(NOLD+NCHAN-1)
         X(3)=3.*X(1)-2.*Q(NOLD+NCHAN-3)
         Q(NOLD+N2)=HMEDIA(X,3)
         GO TO 30
*
   20    CONTINUE
*
         CALL UCOPY2(Q(NOLD+NCHAN),X,3)
         Q(NOLD+1)=HMEDIA(X,3)
         CALL UCOPY2(Q(NOLD+NCHAN+N2-1),X,3)
         Q(NOLD+N2)=HMEDIA(X,3)
   30 CONTINUE
*
*             QUADRATIC INTERPOLATION IF NECESSARY
*
*                    1,  SEARCH FOR A FLAT
*
      N2=N2-2
      DO 40 I=N1,N2
         IF(Q(NOLD+I-1).NE.Q(NOLD+I))GO TO 40
         IF(Q(NOLD+I).NE.Q(NOLD+I+1))GO TO 40
         X(1)=Q(NOLD+I-2)-Q(NOLD+I)
         X(2)=Q(NOLD+I+2)-Q(NOLD+I)
         IF(X(1)*X(2).LE.0.)GO TO 40
         J1=1
         IF(ABS(X(2)).GT.ABS(X(1))) J1=-1
         Q(NOLD+I)=-.5*Q(NOLD+I-2*J1)+Q(NOLD+I-J1)/.75+Q(NOLD+I+2*J1)/6.
         Q(NOLD+I+J1)=.5*(Q(NOLD+I+2*J1)-Q(NOLD+I-2*J1))+Q(NOLD+I-J1)
   40 CONTINUE
*
*             2 ,  RUNNING MEANS
*
      N1=2
      N2=NCHAN-1
      CALL UCOPY2(Q(NOLD),Q(NOLD+NCHAN),NCHAN)
      DO 50 I=N1,N2
         J=NOLD+NCHAN+I
   50 Q(NOLD+I-1)=.25*Q(J-2)+.5*Q(J-1)+.25*Q(J)
*
      IF(NOENT.EQ.2)GO TO 70
      NOENT=2
      CALL UCOPY2(Q(NOLD),Q(NOLD+2*NCHAN),NCHAN)
*
      DO 60 I=1,NCHAN
   60 Q(NOLD+I-1)=HCX(I,1)-Q(NOLD+I-1)
      GO TO 5
   70 CONTINUE
*
      SUMSQR=0.
      DO 80 I=1,NCHAN
         Q(NOLD+I-1)=Q(NOLD+2*NCHAN+I-1)+Q(NOLD+I-1)
         ER=HCX(I,1+I9)
         IF(I9.NE.0)ER=ER*ER
         IF(ER.EQ.0.)ER=Q(NOLD+I-1)
         IF(ER.EQ.0.)GO TO 80
         SUMSQR=SUMSQR+((Q(NOLD+I-1)-HCX(I,1))**2)/ER
   80 CONTINUE
*
      IF(NARG.EQ.3)SUMQR=SUMSQR
      IF(ICASE.EQ.2)GO TO 90
      NOENT=IQ(LCONT+KNOENT)
      CALL HPAK(ID,Q(NOLD))
      IQ(LCONT+KNOENT)=NOENT
      GO TO 99
*
   90 LFUNC=LQ(LCONT-1)
      CALL UCOPY2(Q(NOLD),Q(LFUNC+3),IQ(LFUNC-1)-2)
*
   99 RETURN
      END
