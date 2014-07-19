*
* $Id: ikpl.F,v 1.3 1998/01/28 14:34:43 couet Exp $
*
* $Log: ikpl.F,v $
* Revision 1.3  1998/01/28 14:34:43  couet
* - GGDM driver removed
*
* Revision 1.2  1996/09/25 14:58:46  couet
* - GPR driver removed
*
* Revision 1.1.1.1  1996/02/14 13:10:55  mclareni
* Higz
*
*
#include "higz/pilot.h"
#if !defined(CERNLIB_BATCH)
*CMZ :  1.19/17 22/10/93  16.54.23  by  O.Couet
*-- Author : O.Couet
      SUBROUTINE IKPL(N,X,Y)
#if defined(CERNLIB_KERNEL)
#include "higz/hikern.inc"
#endif
#include "higz/hiatt.inc"
      DIMENSION X(*),Y(*)
*
#if defined(CERNLIB_KERNEL)
#include "higz/hiwcdc.inc"
#endif
#if (defined(CERNLIB_KERNEL))&&(!defined(CERNLIB_X11))
      IF(N.GT.IBUFSI)THEN
         CALL IGERR('Invalid number of points','IPL')
         RETURN
      ENDIF
#endif
#if defined(CERNLIB_PHIGS)
      CALL PPL(N,X,Y)
#endif
#if defined(CERNLIB_KERNEL)||defined(CERNLIB_FALCO)
      DO 60 IWKNB=1,INOPWK
         IF(NODRFL(IWKNB))GOTO 60
         CALL IGSG(IWKNB)
#endif
#if defined(CERNLIB_FALCO)
         IF(IWTYL(IWKNB).EQ.7878.OR.IWTYL(IWKNB).EQ.7879)THEN
            CALL IFPL(N,X,Y)
            GOTO 60
         ENDIF
#endif
#if defined(CERNLIB_GL)
         IF(ICURCI(IWKNB).NE.IPLCI)THEN
            CALL COLOR(IPLCI+IOFCOL)
            ICURCI(IWKNB)=IPLCI
         ENDIF
         IXX=IXWCDC(X(1))
         IYY=IYWCDC(Y(1))
         CALL MOVE2I(IXX,IYY)
         DO 20 I=2,N
            IXX=IXWCDC(X(I))
            IYY=IYWCDC(Y(I))
            CALL DRAW2I(IXX,IYY)
   20    CONTINUE
#endif
#if defined(CERNLIB_X11)
         IPOS(1,1) = IXWCDC(X(1))
         IPOS(2,1) = IYWCDC(Y(1))
         NP        = 1
         DO 30 I=2,N
            IXX = IXWCDC(X(I))
            IYY = IYWCDC(Y(I))
            IF(IXX.NE.IPOS(1,NP).OR.IYY.NE.IPOS(2,NP))THEN
               NP         = NP+1
               IPOS(1,NP) = IXX
               IPOS(2,NP) = IYY
               IF(NP.EQ.IBUFSI)THEN
                  CALL IXLINE(NP,IPOS)
                  NP        = 1
                  IPOS(1,1) = IXX
                  IPOS(2,1) = IYY
               ENDIF
            ENDIF
   30    CONTINUE
         IF(NP.GT.1)CALL IXLINE(NP,IPOS)
#endif
#if defined(CERNLIB_MACMPW)
         IPOS(1,1)=IXWCDC(X(1))
         IPOS(2,1)=IYWCDC(Y(1))
         NP=1
         DO 40 I=2,N
            IXX=IXWCDC(X(I))
            IYY=IYWCDC(Y(I))
            IF(IXX.NE.IPOS(1,NP).OR.IYY.NE.IPOS(2,NP))THEN
               NP=NP+1
               IPOS(1,NP)=IXX
               IPOS(2,NP)=IYY
            ENDIF
   40    CONTINUE
         CALL IMLINE(NP,IPOS)
#endif
#if defined(CERNLIB_KERNEL)||defined(CERNLIB_FALCO)
   60 CONTINUE
#endif
      END
#endif
