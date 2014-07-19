*
* $Id: ifa3.F,v 1.1.1.1 1996/02/14 13:10:42 mclareni Exp $
*
* $Log: ifa3.F,v $
* Revision 1.1.1.1  1996/02/14 13:10:42  mclareni
* Higz
*
*
#include "higz/pilot.h"
*CMZ :  1.23/06 21/11/95  16.22.46  by  O.Couet
*-- Author :    O.Couet   06/01/92
      SUBROUTINE IFA3(N,X,Y,Z)
#include "higz/hiatt.inc"
#include "higz/higraf.inc"
#include "higz/hi3d.inc"
      DIMENSION X(*),Y(*),Z(*)
      IF (IDIM.EQ.3) THEN
#if defined(CERNLIB_PHIGS)
         CALL PFA3(N,X,Y,Z)
#endif
      ELSE
         NGRAF = N
         IF(N.GT.NPMAX)THEN
           CALL IGERR('Too many points','IFA3')
           NGRAF = NPMAX
         ENDIF
         DO 10 I=1,NGRAF
            XGRAF(I) = AWNX*X(I)+BWNX*Y(I)+CWNX*Z(I)+DWNX
            YGRAF(I) = AWNY*X(I)+BWNY*Y(I)+CWNY*Z(I)+DWNY
            ZGRAF(I) = AWNZ*X(I)+BWNZ*Y(I)+CWNZ*Z(I)+DWNZ
   10    CONTINUE
         IF (IZBUF.NE.0) THEN
#if defined(CERNLIB_ZBUF)
            CALL IHZFIL(IFACI,NGRAF,XGRAF,YGRAF,ZGRAF)
#endif
         ELSE
            CALL IFA(NGRAF,XGRAF,YGRAF)
         ENDIF
      ENDIF
*
      END
