*
* $Id: igxmes.F,v 1.1.1.1 1996/02/14 13:10:41 mclareni Exp $
*
* $Log: igxmes.F,v $
* Revision 1.1.1.1  1996/02/14 13:10:41  mclareni
* Higz
*
*
#include "higz/pilot.h"
*CMZ :  1.20/08 19/01/94  13.57.39  by  O.Couet
*-- Author : O.Couet
      SUBROUTINE IGXMES(IX,IY,NC,N,CHMESS,CHTIT,CHOPT)
#if defined(CERNLIB_X11)
#include "higz/hikern.inc"
#endif
      CHARACTER*(*) CHMESS(N),CHTIT,CHOPT
#if defined(CERNLIB_X11)
      DIMENSION IOPT(4)
      EQUIVALENCE (IOPT(1) ,IOPTP),(IOPT(2) ,IOPTC)
      EQUIVALENCE (IOPT(3) ,IOPTD),(IOPT(4) ,IOPTL)
      CHARACTER*32 FX11M
      SAVE IOPEN,IDMES,IW,IH,FX11M,ISIZE,NLINE
      DATA IOPEN /0/
      DATA ISIZE /12/
      DATA IH /0/
#endif
*.______________________________________
*
#if defined(CERNLIB_X11)
      CALL UOPTC (CHOPT,'PCDL',IOPT)
*
*              Print the array CHMESS and open the message
*              window if necessary.
*
      IF(IOPTP.NE.0)THEN
         CALL IXSAVWI
*
* Open the window if necessary
*
         IF(IOPEN.EQ.0)THEN
            NLINE=N
   10       FX11M='-*-courier-bold-r-normal--'
            LONG=LENOCC(FX11M)
            CALL IZITOC(ISIZE,FX11M(LONG+1:))
            LONG=LENOCC(FX11M)+1
            FX11M(LONG:LONG)='*'
            IF(IXSETTF(1,LENOCC(FX11M),FX11M).NE.0)THEN
               IF(ISIZE.EQ.20)THEN
                  CALL IGERR('X11 fonts not available','IGMESS')
                  ISIZE=0
                  RETURN
               ENDIF
               ISIZE=ISIZE+1
               GOTO 10
            ENDIF
#endif
#if (defined(CERNLIB_X11))&&(defined(CERNLIB_IBM))
            CALL CLTOU(CHMESS(1))
#endif
#if defined(CERNLIB_X11)
            CALL IXTXTL(IW,IH,NC,CHMESS(1))
#endif
#if (defined(CERNLIB_X11))&&(defined(CERNLIB_IBM))
            CALL CUTOL(CHMESS(1))
#endif
#if defined(CERNLIB_X11)
            WINSIZ(1) = IX
            WINSIZ(2) = IY
            WINSIZ(3) = IW+10
            WINSIZ(4) = NLINE*(IH+5)+5
            IF(IOPTL.NE.0)WINSIZ(1) = WINSIZ(1)-WINSIZ(3)
            IFLAG=1
            IDMES=IXOPNWI(WINSIZ(1),WINSIZ(2),WINSIZ(3),WINSIZ(4),
     +                    LENOCC(CHTIT),CHTIT,IFLAG)
            IF(IDMES.LT.0)THEN
               CALL IGERR('Can''t open WINDOW','IGMESS')
               RETURN
            ENDIF
            CALL IXS2BUF(IDMES,1)
            CALL IXUPDWI(1)
            IOPEN=1
         ENDIF
*
* Rescale the window if necessary
*
         IF(N.GT.NLINE)THEN
            NLINE = N
            IWW   = IW+10
            IHH   = NLINE*(IH+5)+5
            CALL IXRSCWI(IDMES,IWW,IHH)
         ENDIF
*
* Perform the output
*
         CALL IZSAVA
         CALL IXSELWI(IDMES)
         CALL IXNOCLI(IDMES)
         CALL IXCLRWI
         CALL ISTXAL(0,0)
         CALL ISTXCI(1)
         IF(ISIZE.NE.0)THEN
            IFPX11=IXSETTF(1,LENOCC(FX11M),FX11M)
         ELSE
            RETURN
         ENDIF
         IYT=5+IH
         DO 20 I=1,N
            CALL IXTEXT(0,5,IYT,0.,1.,NC,CHMESS(I))
            IYT=IYT+5+IH
   20    CONTINUE
         CALL IXUPDWI(1)
         CALL IXSETWI
         CALL IZSETA
      ENDIF
*
*              Close the message window.
*
      IF(IOPTC.NE.0.AND.IOPEN.NE.0)THEN
         CALL IXSAVWI
         CALL IXSELWI(IDMES)
         CALL IXCLSWI
         CALL IXUPDWI(1)
         IOPEN=0
         CALL IXSETWI
      ENDIF
*
*              Clear the message window.
*
      IF(IOPTD.NE.0)THEN
         CALL IXSAVWI
         CALL IXSELWI(IDMES)
         CALL IXCLRWI
         CALL IXUPDWI(1)
         CALL IXSETWI
      ENDIF
#endif
*
      END
