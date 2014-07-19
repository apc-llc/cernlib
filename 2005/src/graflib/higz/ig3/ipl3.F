*
* $Id: ipl3.F,v 1.1.1.1 1996/02/14 13:10:43 mclareni Exp $
*
* $Log: ipl3.F,v $
* Revision 1.1.1.1  1996/02/14 13:10:43  mclareni
* Higz
*
*
#include "higz/pilot.h"
*CMZ :  1.23/06 21/11/95  15.35.07  by  O.Couet
*-- Author :    O.Couet   06/01/92
      SUBROUTINE IPL3(N,X,Y,Z)
#include "higz/hiatt.inc"
#include "higz/higraf.inc"
#include "higz/hi3d.inc"
      DIMENSION X(*),Y(*),Z(*)
      IF (IDIM.EQ.3) THEN
#if defined(CERNLIB_PHIGS)
         CALL PPL3(N,X,Y,Z)
#endif
      ELSE
         NGRAF = 0
         DO 20 I=1,N
            NGRAF        = NGRAF+1
            XGRAF(NGRAF) = AWNX*X(I)+BWNX*Y(I)+CWNX*Z(I)+DWNX
            YGRAF(NGRAF) = AWNY*X(I)+BWNY*Y(I)+CWNY*Z(I)+DWNY
            ZGRAF(NGRAF) = AWNZ*X(I)+BWNZ*Y(I)+CWNZ*Z(I)+DWNZ
            IF(NGRAF.EQ.NPMAX.OR.I.EQ.N)THEN
               IF (IZBUF.NE.0) THEN
#if defined(CERNLIB_ZBUF)
                  DO 10 J=2,NGRAF
                     CALL IHZLIN(IPLCI,XGRAF(J-1),YGRAF(J-1),ZGRAF(J-1)
     +,                                XGRAF(J)  ,YGRAF(J)  ,ZGRAF(J) )
   10             CONTINUE
#endif
               ELSE
                  CALL IPL(NGRAF,XGRAF,YGRAF)
               ENDIF
               XGRAF(1) = XGRAF(NGRAF)
               YGRAF(1) = YGRAF(NGRAF)
               ZGRAF(1) = ZGRAF(NGRAF)
               NGRAF    = 1
            ENDIF
   20    CONTINUE
      ENDIF
*
      END
