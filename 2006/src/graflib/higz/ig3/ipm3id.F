*
* $Id: ipm3id.F,v 1.1.1.1 1996/02/14 13:10:43 mclareni Exp $
*
* $Log: ipm3id.F,v $
* Revision 1.1.1.1  1996/02/14 13:10:43  mclareni
* Higz
*
*
#include "higz/pilot.h"
*CMZ :  1.23/06 14/11/95  10.26.40  by  O.Couet
*-- Author :    O.Couet   06/01/92
      SUBROUTINE IPM3ID(N,X,Y,Z,LEVEL,ID)
#include "higz/hiatt.inc"
#include "higz/higraf.inc"
#include "higz/hihist.inc"
#include "higz/hi3d.inc"
#include "higz/hiflag.inc"
      DIMENSION X(*),Y(*),Z(*),ID(*)
      DIMENSION IDD(NPMAX)
      EQUIVALENCE (XWORK(1),IDD(1))
*
      IF (IDIM.EQ.3) THEN
#if defined(CERNLIB_PHIGS)
         CALL PPM3(N,X,Y,Z)
#endif
      ELSE
         NGRAF = 0
         DO 10 I=1,N
            NGRAF        = NGRAF+1
            XGRAF(NGRAF) = AWNX*X(I)+BWNX*Y(I)+CWNX*Z(I)+DWNX
            YGRAF(NGRAF) = AWNY*X(I)+BWNY*Y(I)+CWNY*Z(I)+DWNY
            ZGRAF(NGRAF) = AWNZ*X(I)+BWNZ*Y(I)+CWNZ*Z(I)+DWNZ
            IDD(NGRAF)   = ID(I)
            IF(NGRAF.EQ.NPMAX.OR.I.EQ.N)THEN
               CALL IPM(NGRAF,XGRAF,YGRAF)
               IF(ZFLAG)CALL IGADID(LEVEL,NGRAF,IDD)
               NGRAF = 0
            ENDIF
   10    CONTINUE
      ENDIF
*
      END
