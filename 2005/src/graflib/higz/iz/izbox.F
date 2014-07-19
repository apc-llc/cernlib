*
* $Id: izbox.F,v 1.2 1999/07/21 15:48:28 couet Exp $
*
* $Log: izbox.F,v $
* Revision 1.2  1999/07/21 15:48:28  couet
* - Improvements in PS Patterns drawing
*
* Revision 1.1.1.1  1996/02/14 13:11:08  mclareni
* Higz
*
*
#include "higz/pilot.h"
#if defined(CERNLIB_ZEBRA)||defined(CERNLIB_PSCRIPT)||defined(CERNLIB_MAIL)
*CMZ :  1.21/09 26/09/94  16.21.39  by  O.Couet
*-- Author :
      SUBROUTINE IZBOX(X1,X2,Y1,Y2)
*.===========>
*.
*.   This routine store in the current picture the necessary data to
*. draw a line box .
*.
*. _Input parameters:
*.
*.  REAL X1,Y1 : Left down corner .
*.  REAL X2,Y2 : Right up corner .
*.
*..==========> (O.Couet)
#if defined(CERNLIB_ZEBRA)||defined(CERNLIB_MAIL)
#include "higz/hicode.inc"
#endif
#include "higz/hiflag.inc"
#if defined(CERNLIB_ZEBRA)
#include "higz/hipaw.inc"
#endif
#if defined(CERNLIB_PSCRIPT)
#include "higz/hiatt.inc"
#include "higz/hipost.inc"
#endif
#if defined(CERNLIB_MAIL)
#include "higz/himail.inc"
#endif
#if defined(CERNLIB_PSCRIPT)
      LOGICAL GSAV
      DIMENSION X(4),Y(4)
#endif
*.______________________________________
*
#if defined(CERNLIB_PSCRIPT)
#include "higz/hiwcps.inc"
#endif
#if defined(CERNLIB_PSCRIPT)
      IF(PFLAG)THEN
         CALL IPZONE
         IF(ICLIP.NE.0)THEN
            IX1=IXWCPS(MIN(MAX(X1,X1W),X2W))
            IX2=IXWCPS(MIN(MAX(X2,X1W),X2W))
            IY1=IYWCPS(MIN(MAX(Y1,Y1W),Y2W))
            IY2=IYWCPS(MIN(MAX(Y2,Y1W),Y2W))
         ELSE
            IX1=IXWCPS(X1)
            IX2=IXWCPS(X2)
            IY1=IYWCPS(Y1)
            IY2=IYWCPS(Y2)
         ENDIF
*
         IF(IFAIS.EQ.3.OR.IFAIS.EQ.2)THEN
            IF(IFASI.GE.100)THEN
               GSAV=GFLAG
               GFLAG=.FALSE.
               X(1)=X1
               X(2)=X2
               X(3)=X2
               X(4)=X1
               Y(1)=Y1
               Y(2)=Y1
               Y(3)=Y2
               Y(4)=Y2
               CALL IGFA(4,X,Y)
               GFLAG=GSAV
            ENDIF
            IF(IFASI.GE.1.AND.IFASI.LE.25..AND..NOT.TFLAG)THEN
               CALL IPDPAT(IFASI,IFACI)
               X(1)=X1
               X(2)=X2
               X(3)=X2
               X(4)=X1
               Y(1)=Y1
               Y(2)=Y1
               Y(3)=Y2
               Y(4)=Y2
               CALL IPDRAW(-4,X,Y)
               CALL IPPSTF(3,' FA')
            ENDIF
            IF(IFASI.EQ.-3.AND..NOT.TFLAG)THEN
               CALL IPSCOL(5)
               CALL IPIOUT(IX2-IX1)
               CALL IPIOUT(IY2-IY1)
               CALL IPIOUT(IX1)
               CALL IPIOUT(IY1)
               CALL IPPSTF(3,' bf')
            ENDIF
         ENDIF
*
         IF(.NOT.TFLAG.AND.IFAIS.EQ.1)THEN
            CALL IPLWID(1)
            CALL IPLTYP(1)
            CALL IPSCOL(IFACI)
            CALL IPIOUT(IX2-IX1)
            CALL IPIOUT(IY2-IY1)
            CALL IPIOUT(IX1)
            CALL IPIOUT(IY1)
            CALL IPPSTF(3,' bf')
         ENDIF
*
         IF(TFLAG.AND.IFAIS.EQ.1)THEN
            CALL IPTLIN('m',IX1,IY1)
            CALL IPTLIN('f',IX2-IX1,IY2-IY1)
         ENDIF
*
         IF(IFAIS.EQ.0.OR.IBORD.NE.0)THEN
            IF(TFLAG) THEN
               CALL IPPSTR(BSLASH//'put')
               CALL IPJOUT(IX1,IY1)
               CALL IPPSTR('{'//BSLASH//'framebox')
               CALL IPJOUT(IX2-IX1,IY2-IY1)
               CALL IPPSTR('{}}')
            ELSE
               CALL IPLWID(INT(RLWSC))
               CALL IPLTYP(1)
               CALL IPSCOL(IPLCI)
               CALL IPIOUT(IX2-IX1)
               CALL IPIOUT(IY2-IY1)
               CALL IPIOUT(IX1)
               CALL IPIOUT(IY1)
               CALL IPPSTF(3,' bl')
            ENDIF
         ENDIF
      ENDIF
#endif
#if defined(CERNLIB_MAIL)
*
      IF(MFLAG)THEN
         WRITE (CHMAIL,'(I3,4E16.7)') IBXCO,X1,X2,Y1,Y2
         CALL IMWRIT(4)
      ENDIF
#endif
#if defined(CERNLIB_ZEBRA)
*
      IF(ZFLAG)THEN
         IF(LPICT.LT.0)RETURN
         IF(IZPUSH(0,4,0,'IGBOX').NE.0)RETURN
         CALL IZCFA(IFAICO,1)
         CALL IZCFA(IFASCO,1)
         CALL IZCFA(IFACCO,1)
         CALL IZCFA(IBORCO,1)
         CALL IZCFA(ILWSCO,1)
         CALL IZCFA(IPLCCO,1)
         CALL IZSTCC(IBXCO,IFLPTR)
         Q(LHF+IFLPTR)=X1
         Q(LHF+IFLPTR+1)=X2
         Q(LHF+IFLPTR+2)=Y1
         Q(LHF+IFLPTR+3)=Y2
         CALL IZINCF(4)
      ENDIF
#endif
*
      END
#endif
