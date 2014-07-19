*
* $Id: ipm4id.F,v 1.1.1.1 1996/02/14 13:10:41 mclareni Exp $
*
* $Log: ipm4id.F,v $
* Revision 1.1.1.1  1996/02/14 13:10:41  mclareni
* Higz
*
*
#include "higz/pilot.h"
*CMZ :  1.23/06 14/11/95  10.26.40  by  O.Couet
*-- Author :    O.Couet   28/09/93
      SUBROUTINE IPM4ID(N,X,Y,Z,C,CMIN,CMAX,LEVEL,ID)
#include "higz/hiatt.inc"
#include "higz/higraf.inc"
#include "higz/hihist.inc"
#include "higz/hi3d.inc"
#include "higz/hiflag.inc"
      DIMENSION X(*),Y(*),Z(*),C(*),ID(*)
      DIMENSION IDD(NPMAX)
      EQUIVALENCE (XWORK(1),IDD(1))
*
      IF(INBCOL.GT.8)THEN
         ICOFF = 8
      ELSE
         ICOFF = 1
      ENDIF
      DC = (CMAX-CMIN)/(INBCOL-ICOFF)
*
      IF (IDIM.EQ.3) THEN
#if defined(CERNLIB_PHIGS)
         CALL PPM3(N,X,Y,Z)
#endif
      ELSE
         DO 20 IC=ICOFF,INBCOL-1
            NGRAF = 0
            CALL ISPMCI(IC)
            DO 10 I=1,N
               INCOL = INT((C(I)-CMIN)/DC)+ICOFF
               IF(INCOL.EQ.IC)THEN
                  NGRAF        = NGRAF+1
                  XGRAF(NGRAF) = AWNX*X(I)+BWNX*Y(I)+CWNX*Z(I)+DWNX
                  YGRAF(NGRAF) = AWNY*X(I)+BWNY*Y(I)+CWNY*Z(I)+DWNY
                  ZGRAF(NGRAF) = AWNZ*X(I)+BWNZ*Y(I)+CWNZ*Z(I)+DWNZ
                  IDD(NGRAF)   = ID(I)
               ENDIF
               IF(NGRAF.EQ.NPMAX.OR.I.EQ.N)THEN
                  CALL IPM(NGRAF,XGRAF,YGRAF)
                  IF(ZFLAG)CALL IGADID(LEVEL,NGRAF,IDD)
                  NGRAF = 0
               ENDIF
   10       CONTINUE
   20    CONTINUE
      ENDIF
*
      END
