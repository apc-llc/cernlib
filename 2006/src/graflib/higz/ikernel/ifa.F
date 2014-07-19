*
* $Id: ifa.F,v 1.3 1998/01/28 14:34:41 couet Exp $
*
* $Log: ifa.F,v $
* Revision 1.3  1998/01/28 14:34:41  couet
* - GGDM driver removed
*
* Revision 1.2  1996/09/25 14:58:43  couet
* - GPR driver removed
*
* Revision 1.1.1.1  1996/02/14 13:10:55  mclareni
* Higz
*
*
#include "higz/pilot.h"
*CMZ :  1.23/01 08/05/95  11.43.59  by  O.Couet
*-- Author : O.Couet
      SUBROUTINE IFA(N,X,Y)
      ENTRY      GFA(N,X,Y)
#if defined(CERNLIB_KERNEL)
#include "higz/hikern.inc"
#endif
#include "higz/hiatt.inc"
#include "higz/hiflag.inc"
#if defined(CERNLIB_FALCO)||defined(CERNLIB_MACMPW)
      DIMENSION XCLOSE(2),YCLOSE(2)
#endif
      DIMENSION X(*),Y(*)
*
#if defined(CERNLIB_KERNEL)
#include "higz/hiwcdc.inc"
#endif
#if defined(CERNLIB_KERNEL)
      IF(N.LT.3.OR.N.GT.IBUFSI-1)THEN
         CALL IGERR('Invalid number of points','IFA')
         RETURN
      ENDIF
#endif
      IF(IFAIS.EQ.3.AND.IFASI.GE.100)THEN
         CALL IGFA(N,X,Y)
      ELSE
         IF(GFLAG)THEN
#if defined(CERNLIB_PHIGS)
            CALL PFA(N,X,Y)
#endif
#if defined(CERNLIB_KERNEL)||defined(CERNLIB_FALCO)
            DO 70 IWKNB=1,INOPWK
               IF(NODRFL(IWKNB))GOTO 70
               CALL IGSG(IWKNB)
#endif
#if defined(CERNLIB_FALCO)
               IF(IWTYL(IWKNB).EQ.7878.OR.IWTYL(IWKNB).EQ.7879)THEN
                  IF(IFAIS.EQ.1)THEN
                     CALL IGHATC(0.001,0.,N,X,Y)
                  ELSE
                     CALL IFPL(N,X,Y)
                     XCLOSE(1)=X(1)
                     YCLOSE(1)=Y(1)
                     XCLOSE(2)=X(N)
                     YCLOSE(2)=Y(N)
                     CALL IFPL(2,XCLOSE,YCLOSE)
                  ENDIF
                  GOTO 70
               ENDIF
#endif
#if (defined(CERNLIB_GL))&&(!defined(CERNLIB_MSDOS))
               DO 20 I=1,N
                  IPOS(1,I)=IXWCDC(X(I))
                  IPOS(2,I)=IYWCDC(Y(I))
   20          CONTINUE
               IF(ICURCI(IWKNB).NE.IFACI)THEN
                  CALL COLOR(IFACI+IOFCOL)
                  ICURCI(IWKNB)=IFACI
               ENDIF
               IF(IFAIS.NE.0)THEN
                  CALL POLF2I(N,IPOS)
               ELSE
                  CALL POLY2I(N,IPOS)
               ENDIF
#endif
#if defined(CERNLIB_MSDOS)
               IF(IFAIS.EQ.1.AND.ILOPWK(IWKNB).EQ.IDMETA)THEN
                  CALL IGHATC(0.001,0.0,N,X,Y)
               ELSE
                  DO 30 I=1,N
#endif
#if (defined(CERNLIB_MSDOS))&&(defined(CERNLIB_NDP))
                     IPOS(I,1)=IXWCDC(X(I))
                     IPOS(I,2)=IYWCDC(Y(I))
#endif
#if (defined(CERNLIB_MSDOS))&&(!defined(CERNLIB_NDP))
                     IPOS(1,I)=IXWCDC(X(I))
                     IPOS(2,I)=IYWCDC(Y(I))
#endif
#if defined(CERNLIB_MSDOS)
   30             CONTINUE
                  IF(ICURCI(IWKNB).NE.IFACI)THEN
                     CALL COLOR(IFACI+IOFCOL)
                     ICURCI(IWKNB)=IFACI
                  ENDIF
                  IF(IFAIS.NE.0)THEN
                     CALL POLF2I(N,IPOS)
                  ELSE
                     CALL POLY2I(N,IPOS)
                  ENDIF
               ENDIF
#endif
#if defined(CERNLIB_X11)
               NP=N
               DO 40 I=1,NP
                  IPOS(1,I)=IXWCDC(X(I))
                  IPOS(2,I)=IYWCDC(Y(I))
   40          CONTINUE
#endif
#if (defined(CERNLIB_X11))&&(!defined(CERNLIB_WINNT))
               IF(IFAIS.EQ.0)THEN
                  IPOS(1,NP+1)=IPOS(1,1)
                  IPOS(2,NP+1)=IPOS(2,1)
                  NP=NP+1
               ENDIF
#endif
#if defined(CERNLIB_X11)
               CALL IXFLARE(NP,IPOS)
#endif
#if defined(CERNLIB_MACMPW)
               NP=N
               DO 60 I=1,NP
                  IPOS(1,I)=IXWCDC(X(I))
                  IPOS(2,I)=IYWCDC(Y(I))
   60          CONTINUE
               IF(IFAIS.EQ.0)THEN
                  IPOS(1,NP+1)=IPOS(1,1)
                  IPOS(2,NP+1)=IPOS(2,1)
                  NP=NP+1
               ENDIF
               CALL IMFLARE(NP,IPOS)
#endif
#if defined(CERNLIB_KERNEL)||defined(CERNLIB_FALCO)
   70       CONTINUE
#endif
         ENDIF
      ENDIF
#if defined(CERNLIB_ZEBRA)||defined(CERNLIB_PSCRIPT)||defined(CERNLIB_MAIL)
      IF(GLFLAG)CALL IZFA(N,X,Y)
#endif
*
      END
