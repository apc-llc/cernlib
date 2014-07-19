*
* $Id: izfa.F,v 1.3 1999/07/21 15:48:29 couet Exp $
*
* $Log: izfa.F,v $
* Revision 1.3  1999/07/21 15:48:29  couet
* - Improvements in PS Patterns drawing
*
* Revision 1.2  1998/05/26 09:19:28  couet
* - For filling patterns 1 to 25 the PS ouput was wrong in some cases. For those
* the pattern we now close the polygon (move to X1 Y1).
*
* Revision 1.1.1.1  1996/02/14 13:11:09  mclareni
* Higz
*
*
#include "higz/pilot.h"
#if defined(CERNLIB_ZEBRA)||defined(CERNLIB_PSCRIPT)||defined(CERNLIB_MAIL)
*CMZ :  1.21/09 26/09/94  16.21.39  by  O.Couet
*-- Author :
      SUBROUTINE IZFA(N,X,Y)
*.===========>
*.
*.   This routine stores in the current picture the necessary data to
*. draw a fill area .
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
#include "higz/hipost.inc"
#endif
#if defined(CERNLIB_MAIL)
#include "higz/himail.inc"
#endif
      DIMENSION X(*),Y(*)
#if defined(CERNLIB_PSCRIPT)
      LOGICAL GSAV
#endif
*.______________________________________
*
#if defined(CERNLIB_PSCRIPT)
#include "higz/hiwcps.inc"
      IF(PFLAG)THEN
         IF(IFAIS.EQ.3.AND.IFASI.GE.100)GOTO 10
         CALL IPZONE
         GSAV=GFLAG
         GFLAG=.FALSE.
         IF(.NOT.TFLAG) THEN
            CALL IPSCOL(IFACI)
            CALL IPLWID(1)
         ENDIF
         CALL IPLTYP(1)
         IF(IFAIS.EQ.0.AND..NOT.TFLAG)THEN
            CALL IPDRAW(-N,X,Y)
            CALL IPPSTF(5,' cl s')
         ELSEIF(IFAIS.EQ.3.OR.IFAIS.EQ.2)THEN
            IF(IFASI.EQ.-101)CALL IGHATC(0.0075,180.,N,X,Y)
            IF(IFASI.EQ.-102)CALL IGHATC(0.0075,90.,N,X,Y)
            IF(IFASI.EQ.-103)CALL IGHATC(0.0075,135.,N,X,Y)
            IF(IFASI.EQ.-104)CALL IGHATC(0.0075,45.,N,X,Y)
            IF(IFASI.EQ.-105)CALL IGHATC(0.0075,150.,N,X,Y)
            IF(IFASI.EQ.-106)CALL IGHATC(0.0075,30.,N,X,Y)
            IF(IFASI.EQ.-107)CALL IGHATC(0.0075,120.,N,X,Y)
            IF(IFASI.EQ.-108)CALL IGHATC(0.0075,60.,N,X,Y)
            IF(IFASI.EQ.-109)CALL IGHATC(0.01,180.,N,X,Y)
            IF(IFASI.EQ.-110)CALL IGHATC(0.01,90.,N,X,Y)
            IF(IFASI.EQ.-111)CALL IGHATC(0.01,135.,N,X,Y)
            IF(IFASI.EQ.-112)CALL IGHATC(0.01,45.,N,X,Y)
            IF(IFASI.EQ.-113)CALL IGHATC(0.01,150.,N,X,Y)
            IF(IFASI.EQ.-114)CALL IGHATC(0.01,30.,N,X,Y)
            IF(IFASI.EQ.-115)CALL IGHATC(0.01,120.,N,X,Y)
            IF(IFASI.EQ.-116)CALL IGHATC(0.01,60.,N,X,Y)
            IF(IFASI.EQ.-117)CALL IGHATC(0.015,180.,N,X,Y)
            IF(IFASI.EQ.-118)CALL IGHATC(0.015,90.,N,X,Y)
            IF(IFASI.EQ.-119)CALL IGHATC(0.015,135.,N,X,Y)
            IF(IFASI.EQ.-120)CALL IGHATC(0.015,45.,N,X,Y)
            IF(IFASI.EQ.-121)CALL IGHATC(0.015,150.,N,X,Y)
            IF(IFASI.EQ.-122)CALL IGHATC(0.015,30.,N,X,Y)
            IF(IFASI.EQ.-123)CALL IGHATC(0.015,120.,N,X,Y)
            IF(IFASI.EQ.-124)CALL IGHATC(0.015,60.,N,X,Y)
            IF(IFASI.GE.1.AND.IFASI.LE.25..AND..NOT.TFLAG)THEN
               CALL IPDPAT(IFASI,IFACI)
               CALL IPDRAW(-N,X,Y)
               CALL IPPSTF(3,' FA')
            ENDIF
            IF(IFASI.EQ.-3.AND..NOT.TFLAG)THEN
               CALL IPSCOL(5)
               CALL IPDRAW(-N,X,Y)
               CALL IPPSTF(2,' f')
            ENDIF
         ELSEIF(.NOT.TFLAG) THEN
            CALL IPDRAW(-N,X,Y)
            CALL IPPSTF(2,' f')
         ENDIF
         GFLAG=GSAV
      ENDIF
   10 CONTINUE
#endif
#if defined(CERNLIB_MAIL)
*
      IF(MFLAG)THEN
         WRITE (CHMAIL,'(I3,I5)') IFACO,N
         CALL IMWRIT(1)
         CALL IMFOUT(N,X)
         CALL IMFOUT(N,Y)
         CALL IMWRIT(5)
      ENDIF
#endif
#if defined(CERNLIB_ZEBRA)
*
      IF(ZFLAG)THEN
         IF(LPICT.LT.0)RETURN
         IF(IZPUSH(2,2*N,0,'IFA').NE.0)RETURN
         CALL IZCFA(IFAICO,1)
         CALL IZCFA(IFASCO,1)
         CALL IZCFA(IFACCO,1)
         CALL IZSTCC(IFACO,INTPTR)
         IQ(LHI+INTPTR)   = IFLPTR
         IQ(LHI+INTPTR+1) = N
         CALL IZINCI(2)
         IADR  = LHF+IFLPTR-1
         IADRN = IADR+N
         DO 20 I=1,N
            Q(IADR+I)  = X(I)
            Q(IADRN+I) = Y(I)
   20    CONTINUE
         CALL IZINCF(2*N)
      ENDIF
#endif
*
      END
#endif
