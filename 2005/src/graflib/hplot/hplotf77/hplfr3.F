*
* $Id: hplfr3.F,v 1.4 2000/06/06 13:35:28 couet Exp $
*
* $Log: hplfr3.F,v $
* Revision 1.4  2000/06/06 13:35:28  couet
* - New option 'A' to draw the axis only
*
* Revision 1.3  2000/04/10 13:48:28  couet
* - The axis title in were not placed properlly after a call to HPLFR3
*
* Revision 1.2  1996/03/19 11:18:23  couet
* - The log computation had disapear during the cvs conversion
*
* Revision 1.1.1.1  1996/01/19 10:50:08  mclareni
* Hplot
*
*
#include "hplot/pilot.h"
      SUBROUTINE HPLFR3(X1I,X2I,Y1I,Y2I,Z1I,Z2I,THETA,PHI,CHOPT)
*.==========>
*.
*.   Define a 3D frame.
*.
*. _Input parameters:
*.
*.    REAL X1I : Low range in X
*.    REAL X2I : High range in X
*.    REAL Y1I : Low range in Y
*.    REAL Y2I : High range in Y
*.    REAL Z1I : Low range in Z
*.    REAL Z2I : High range in Z
*.    REAL THETA, PHI : Viewing angles
*.    CHARACTER CHOPT : Options
*.                      'B' Draw the back box
*.                      'F' Draw the front box
*.                      'A' Draw the axis
*.                      'W' Start a new window
*.                      'G' forget log scale
*.
*..=========>
#include "hplot/hpl1.inc"
#include "hplot/hpl2.inc"
#include "hplot/hpl3.inc"
#include "hplot/hpl11.inc"
#include "hplot/quest.inc"
      CHARACTER*(*) CHOPT
      EXTERNAL IHDFL1,IHDFL2
      DIMENSION IOPT(5)
      EQUIVALENCE (IOPTB,IOPT(1)),(IOPTF,IOPT(2))
      EQUIVALENCE (IOPTW,IOPT(3)),(IOPTG,IOPT(4))
      EQUIVALENCE (IOPTA,IOPT(5))
*
      DIMENSION RMIN(3),RMAX(3),PAR(17)
      EQUIVALENCE (XLINE(1),RMIN(1))
      EQUIVALENCE (XLINE(4),RMAX(1))
      EQUIVALENCE (YLINE(1),PAR(1))
*.______________________________________
*
      CALL UOPTC(CHOPT,'BFWGA',IOPT)
*
      RMIN(1) = X1I
      RMIN(2) = Y1I
      RMIN(3) = Z1I
      RMAX(1) = X2I
      RMAX(2) = Y2I
      RMAX(3) = Z2I
*
      CALL IGQ('LWID',RLWID)
*
*              Take the LOG if required
*
      IF (IOPTG.EQ.0) THEN
         IF (LOGX.NE.0) THEN
            IF(RMAX(1).LE.0)THEN
               CALL HBUG('X2 must be >0 in LOG scale','HPLFR3',0)
               GOTO 10
            ENDIF
            IF (RMIN(1).LE.0.) RMIN(1) = MIN(RMAX(1)/1000.,1.)
            RMIN(1) = LOG10(RMIN(1))
            RMAX(1) = LOG10(RMAX(1))
         ENDIF
*
         IF (LOGY.NE.0) THEN
            IF(RMAX(2).LE.0)THEN
               CALL HBUG('Y2 must be >0 in LOG scale','HPLFR3',0)
               GOTO 10
            ENDIF
            IF (RMIN(2).LE.0.) RMIN(2) = MIN(RMAX(2)/1000.,1.)
            RMIN(2) = LOG10(RMIN(2))
            RMAX(2) = LOG10(RMAX(2))
         ENDIF
*
         IF (LOGZ.NE.0) THEN
            IF(RMAX(3).LE.0)THEN
               CALL HBUG('Z2 must be >0 in LOG scale','HPLFR3',0)
               GOTO 10
            ENDIF
            IF (RMIN(3).LE.0.) RMIN(3) = MIN(RMAX(3)/1000.,1.)
            RMIN(3) = LOG10(RMIN(3))
            RMAX(3) = LOG10(RMAX(3))
         ENDIF
      ENDIF
*
*              Errors control flag is reset
*
      IQUEST(1) = 0
*
*              Define the 2D space according to the ZONEs
*
      IF(IOPTW.NE.0)CALL HPLFRA(-1.,1.,-1.,1.,'AB')
*
*              Define the 3D space
*
      CALL ISWN3(NTWIN,
     +           RMIN(1),RMAX(1),RMIN(2),RMAX(2),RMIN(3),RMAX(3),
     +           -90.-PHI,90-THETA,0.,' ')
      CALL ISVP3(NTWIN,-1.,1.,-1.,1.)
      CALL ISELNT(NTWIN)
      IF ( IQUEST(1) .NE. 0 ) GOTO 10
      LO3DPL = .TRUE.
*
      CALL IGSET('LWID',1.)
      CALL HPLAX2(PAR)
      IF ( IQUEST(1) .NE. 0 ) THEN
         CALL IGERR('Try option W',' ')
         GOTO 10
      ENDIF
*
      IF (IOPTG.EQ.0) THEN
         PAR(15) = FLOAT(LOGX)
         PAR(16) = FLOAT(LOGY)
         PAR(17) = FLOAT(LOGZ)
      ELSE
         PAR(15) = 0.
         PAR(16) = 0.
         PAR(17) = 0.
      ENDIF
*
      IF(IOPTB.NE.0)THEN
         CALL IGTAB4(RMIN,RMAX,PAR(6))
         CALL IGLEV(INT(PAR(3)),RMIN(3),RMAX(3),0,'A')
         CALL ISCLIP(0)
         CALL IHSINI(-1.1,1.1)
         CALL IHBBOX(RMIN,RMAX,90.,IHDFL1)
      ENDIF
*
      IF(IOPTF.NE.0)THEN
         CALL IHSINI(-1.1,1.1)
         CALL IHFBOX(RMIN,RMAX,90.,IHDFL2)
      ENDIF
*
      IF(IOPTA.NE.0)THEN
         CALL IHSINI(-1.1,1.1)
         CALL IZSAV
         CALL IGTAB5(RMIN,RMAX,90.,PAR)
         CALL IZSET
      ENDIF
*
   10 CALL IGSET('LWID',RLWID)
      END
