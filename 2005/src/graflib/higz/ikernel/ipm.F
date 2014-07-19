*
* $Id: ipm.F,v 1.3 1998/01/28 14:34:46 couet Exp $
*
* $Log: ipm.F,v $
* Revision 1.3  1998/01/28 14:34:46  couet
* - GGDM driver removed
*
* Revision 1.2  1996/09/25 14:58:49  couet
* - GPR driver removed
*
* Revision 1.1.1.1  1996/02/14 13:10:56  mclareni
* Higz
*
*
#include "higz/pilot.h"
*CMZ :  1.20/10 26/01/94  12.06.22  by  O.Couet
*-- Author : O.Couet
      SUBROUTINE IPM(N,X,Y)
      ENTRY      GPM(N,X,Y)
#if defined(CERNLIB_KERNEL)
#include "higz/hikern.inc"
#endif
#if defined(CERNLIB_MSDOS)
#include "higz/himeta.inc"
#endif
#include "higz/hiatt.inc"
#include "higz/hiflag.inc"
      DIMENSION X(*),Y(*)
*
#if defined(CERNLIB_KERNEL)
#include "higz/hiwcdc.inc"
#endif
#if (defined(CERNLIB_KERNEL))&&(!defined(CERNLIB_X11))
      IF(N.LT.1.OR.N.GT.IBUFSI)THEN
         CALL IGERR('Invalid number of points','IPM')
         RETURN
      ENDIF
#endif
      IF(IMK.GE.20)THEN
         CALL IGPM(N,X,Y,IMK)
      ELSE
         IF(GFLAG)THEN
#if defined(CERNLIB_PHIGS)
            CALL PPM(N,X,Y)
#endif
#if defined(CERNLIB_KERNEL)||defined(CERNLIB_FALCO)
            DO 70 IWKNB=1,INOPWK
               IF(NODRFL(IWKNB))GOTO 70
               CALL IGSG(IWKNB)
#endif
#if defined(CERNLIB_FALCO)
               IF(IWTYL(IWKNB).EQ.7878.OR.IWTYL(IWKNB).EQ.7879)THEN
                  IF(IMK.EQ.3)THEN
                     CALL IGPM(N,X,Y,31)
                  ELSEIF(IMK.EQ.4)THEN
                     CALL IGPM(N,X,Y,24)
                  ELSE
                     DO 10 I=1,N
                        XFALCO=(RVXMIN+XRATIO*(X(I)-RWXMIN))
                        YFALCO=(RVYMIN+YRATIO*(Y(I)-RWYMIN))
                        CALL IFMOVE(XFALCO,YFALCO)
                        CALL IFDRAW(XFALCO,YFALCO)
   10                CONTINUE
                     CALL IFPUT(0)
                  ENDIF
                  GOTO 70
               ENDIF
#endif
#if (defined(CERNLIB_GL))&&(!defined(CERNLIB_MSDOS))
               IF(ICURCI(IWKNB).NE.IPMCI)THEN
                  CALL COLOR(IPMCI+IOFCOL)
                  ICURCI(IWKNB)=IPMCI
               ENDIF
               DO 30 I=1,N
                  IXX=IXWCDC(X(I))
                  IYY=IYWCDC(Y(I))
                  CALL MOVE2I(IXX,IYY)
                  CALL DRAW2I(IXX,IYY)
   30          CONTINUE
#endif
#if defined(CERNLIB_MSDOS)
               CALL MARKER(N,X,Y)
#endif
#if defined(CERNLIB_X11)
               LM=ABS(IMK)
               IM=NINT(RMKSC*4)
               IF(IM.LE.0) LM=1
               IF(LM.EQ.2) THEN
*--- + shaped marker
                  IPOS(1,1)=-IM
                  IPOS(2,1)=0
                  IPOS(1,2)=IM
                  IPOS(2,2)=0
                  IPOS(1,3)=0
                  IPOS(2,3)=-IM
                  IPOS(1,4)=0
                  IPOS(2,4)=IM
                  CALL IXSETMS(4,4,IPOS)
               ELSEIF(LM.EQ.3) THEN
*--- * shaped marker
                  IPOS(1,1)=-IM
                  IPOS(2,1)=0
                  IPOS(1,2)=IM
                  IPOS(2,2)=0
                  IPOS(1,3)=0
                  IPOS(2,3)=-IM
                  IPOS(1,4)=0
                  IPOS(2,4)=IM
                  IM=NINT(IM*0.707)
                  IPOS(1,5)=-IM
                  IPOS(2,5)=-IM
                  IPOS(1,6)=IM
                  IPOS(2,6)=IM
                  IPOS(1,7)=-IM
                  IPOS(2,7)=IM
                  IPOS(1,8)=IM
                  IPOS(2,8)=-IM
                  CALL IXSETMS(4,8,IPOS)
               ELSEIF(LM.EQ.4) THEN
*--- O shaped marker
                  CALL IXSETMS(0,IM*2,IPOS)
               ELSEIF(LM.EQ.5) THEN
*--- X shaped marker
                  IM=NINT(IM*0.707)
                  IPOS(1,1)=-IM
                  IPOS(2,1)=-IM
                  IPOS(1,2)=IM
                  IPOS(2,2)=IM
                  IPOS(1,3)=-IM
                  IPOS(2,3)=IM
                  IPOS(1,4)=IM
                  IPOS(2,4)=-IM
                  CALL IXSETMS(4,4,IPOS)
               ELSEIF(LM.EQ.6) THEN
*--- + shaped marker (with 1 pixel)
                  IPOS(1,1)=-1
                  IPOS(2,1)=0
                  IPOS(1,2)=1
                  IPOS(2,2)=0
                  IPOS(1,3)=0
                  IPOS(2,3)=-1
                  IPOS(1,4)=0
                  IPOS(2,4)=1
                  CALL IXSETMS(4,4,IPOS)
               ELSEIF(LM.EQ.7) THEN
*--- . shaped marker (with 9 pixel)
                  IPOS(1,1)=-1
                  IPOS(2,1)=1
                  IPOS(1,2)=1
                  IPOS(2,2)=1
                  IPOS(1,3)=-1
                  IPOS(2,3)=0
                  IPOS(1,4)=1
                  IPOS(2,4)=0
                  IPOS(1,5)=-1
                  IPOS(2,5)=-1
                  IPOS(1,6)=1
                  IPOS(2,6)=-1
                  CALL IXSETMS(4,6,IPOS)
               ELSEIF(LM.EQ.8) THEN
*--- O shaped marker (filled)
                  CALL IXSETMS(1,IM*2,IPOS)
               ELSE
*--- single dot
                  CALL IXSETMS(0,0,IPOS)
               ENDIF
               NPT = 0
               DO 40 I=1,N
                  NPT = NPT+1
                  IPOS(1,NPT) = IXWCDC(X(I))
                  IPOS(2,NPT) = IYWCDC(Y(I))
                  IF(NPT.EQ.IBUFSI.OR.I.EQ.N)THEN
                     CALL IXMARKE(NPT,IPOS)
                     NPT = 0
                  ENDIF
   40          CONTINUE
#endif
#if defined(CERNLIB_MACMPW)
               DO 50 I=1,N
                  IPOS(1,I)=IXWCDC(X(I))
                  IPOS(2,I)=IYWCDC(Y(I))
   50          CONTINUE
               CALL IMMARKE(N,IPOS)
#endif
#if defined(CERNLIB_KERNEL)||defined(CERNLIB_FALCO)
   70       CONTINUE
#endif
         ENDIF
      ENDIF
#if defined(CERNLIB_ZEBRA)||defined(CERNLIB_PSCRIPT)||defined(CERNLIB_MAIL)
      IF(GLFLAG)CALL IZPM(N,X,Y)
#endif
*
      END
