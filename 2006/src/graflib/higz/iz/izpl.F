*
* $Id: izpl.F,v 1.2 2005/10/28 12:38:18 couet Exp $
*
* $Log: izpl.F,v $
* Revision 1.2  2005/10/28 12:38:18  couet
* - SVG driver, workstation type -779
*
* Revision 1.1.1.1  1996/02/14 13:11:12  mclareni
* Higz
*
*
#include "higz/pilot.h"
#if defined(CERNLIB_ZEBRA)||defined(CERNLIB_PSCRIPT)||defined(CERNLIB_MAIL)
*CMZ :  1.21/09 26/09/94  16.21.39  by  O.Couet
*-- Author :
      SUBROUTINE IZPL(N,X,Y)
*.===========>
*.
*.   This routine stores in the current picture the necessary data to
*. to draw a polyline .
*.
*. _Input parameters:
*.
*.  INTEGER N : Number of points .
*.  REAL X(N) : X coordinates .
*.  REAL Y(N) : Y coordinates .
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
#endif
#if defined(CERNLIB_MAIL)
#include "higz/himail.inc"
#endif
      DIMENSION X(*),Y(*)
*.______________________________________
*
#if defined(CERNLIB_PSCRIPT)
      IF(PFLAG)THEN
         IF(ILN.GE.12.AND.ILN.LE.15)GOTO 10
         CALL IPZONE
         IF(TFLAG)THEN
            CALL IPLTYP(ILN)
            CALL IPDRAW(N,X,Y)
         ELSEIF(VFLAG)THEN
            CALL IPPSTF(29,'<polyline fill="none" stroke=')
            CALL IPSCOL(IPLCI)
            CALL IPLWID(INT(RLWSC))
            CALL IPLTYP(ILN)
            CALL IPPSTF(9,' points="')
            CALL IPDRAW(N,X,Y)
            CALL IPPSTF(3,'"/>')
         ELSE
            CALL IPLTYP(ILN)
            CALL IPLWID(INT(RLWSC))
            CALL IPSCOL(IPLCI)
            CALL IPDRAW(N,X,Y)
            IF(X(1).EQ.X(N).AND.Y(1).EQ.Y(N))THEN
               CALL IPPSTF(3,' cl')
            ENDIF
            CALL IPPSTF(2,' s')
         ENDIF
      ENDIF
   10 CONTINUE
#endif
#if defined(CERNLIB_MAIL)
*
      IF(MFLAG)THEN
         IF(N.EQ.2)THEN
            WRITE (CHMAIL,'(I3,4E16.7)') IPL2CO,X(1),X(2),Y(1),Y(2)
            CALL IMWRIT(4)
         ELSE
            WRITE (CHMAIL,'(I3,I5)') IPLCO,N
            CALL IMWRIT(1)
            CALL IMFOUT(N,X)
            CALL IMFOUT(N,Y)
            CALL IMWRIT(5)
         ENDIF
      ENDIF
#endif
#if defined(CERNLIB_ZEBRA)
*
      IF(ZFLAG)THEN
         IF(LPICT.LT.0)RETURN
         IF(IZPUSH(2,2*N,0,'IPL').NE.0)RETURN
         CALL IZCFA(ILNCO,1)
         CALL IZCFA(ILWSCO,1)
         CALL IZCFA(IPLCCO,1)
         CALL IZCFA(IBSLCO,1)
         IF(N.NE.2)THEN
            CALL IZSTCC(IPLCO,INTPTR)
            IQ(LHI+INTPTR)   = IFLPTR
            IQ(LHI+INTPTR+1) = N
            CALL IZINCI(2)
            IADR  = LHF+IFLPTR-1
            IADRN = IADR+N
            DO 20 I=1,N
               Q(IADR+I)  = X(I)
               Q(IADRN+I) = Y(I)
   20       CONTINUE
            CALL IZINCF(2*N)
         ELSE
            CALL IZSTCC(IPL2CO,IFLPTR)
            Q(LHF+IFLPTR)   = X(1)
            Q(LHF+IFLPTR+1) = X(2)
            Q(LHF+IFLPTR+2) = Y(1)
            Q(LHF+IFLPTR+3) = Y(2)
            CALL IZINCF(4)
         ENDIF
      ENDIF
#endif
*
      END
#endif
