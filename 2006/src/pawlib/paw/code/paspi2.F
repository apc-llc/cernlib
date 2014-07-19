*
* $Id: paspi2.F,v 1.3 1996/08/21 13:58:20 lecointe Exp $
*
* $Log: paspi2.F,v $
* Revision 1.3  1996/08/21 13:58:20  lecointe
* Corrected small type incoherences
*
* Revision 1.2  1996/08/21 12:53:27  lecointe
* PASPI routine now being call from C, with slightly different arguments
*
* Revision 1.1.1.1  1996/03/01 11:38:42  mclareni
* Paw
*
*
#include "paw/pilot.h"
*CMZ :  2.07/00 04/05/95  14.08.27  by  O.Couet
*-- Author :    Piotr Wolski   18/07/94
*
*
      SUBROUTINE PASPI (ICHEVT, NVARS, CNAMES, RZONE, IVART, IZONE, 
     +                   CURRENT, LOW, HIGH, AVG, SPIDER_TYPE)
*
*-----------------------------------------------------------------------
*
*                             DRAWS A SPIDER
*
*-----------------------------------------------------------------------
*
* IN
* ++
*
*  ICHEVT            current event number
*  NVARS             Total number of variables
*  CNAMES            variable names
*  RZONE             number of zones -> max(horizontally,vertically)
*  IZONE             which zone is now drawing
*  IVART             Type code for each variable
*                      0 : integer
*                      1 : real
*                      2 : other (char, logical)
*  CURRENT           current value of each variable
*  LOW               min values of each variable
*  HIGH              max values of each variable
*  AVG               average values of each variable
*  SPIDER_TYPE       1=contour 2=wedges
*
*
* CALL
* ++++
*
*  by qp_exec_evt (in "qp_execute.c")
*
*  no subcall      
*
*-----------------------------------------------------------------------
      PARAMETER (MXPOIN=500,PI=3.1415926,O2PI=180./PI)
      COMMON/PCBUFF/PAWBUF(2004)
*
      CHARACTER*80 CHTMP
      CHARACTER*(*) CNAMES
      CHARACTER*80  CURNAME
      CHARACTER*64 CLOW
      CHARACTER*64 CHIGH
      CHARACTER*64 CVAR
*
      INTEGER ICHEVT
      INTEGER NVARS
      INTEGER IVART(NVARS)
      INTEGER IZONE,ICVARS
      INTEGER SPIDER_TYPE
*
      REAL RZONE
*
      REAL RXT(2)
      REAL RYT(2)
      REAL RXE(MXPOIN), RXA(MXPOIN)
      REAL RYE(MXPOIN), RYA(MXPOIN)
      EQUIVALENCE   (PAWBUF(201),RXE(1)),(PAWBUF(400),RYE(1))
      EQUIVALENCE   (PAWBUF(600),RXA(1)),(PAWBUF(800),RYA(1))
*
      REAL LOW(NVARS)
      REAL HIGH(NVARS)
      REAL CURRENT(NVARS)
      REAL AVG(NVARS)
*
      REAL COSANG,SINANG,BARW
*
      LOGICAL AVRG
*
      IF (ICHEVT.LT.0) THEN
       AVRG = .TRUE.
       ICHEVT = -ICHEVT
      ELSE
         AVRG = .FALSE.
      ENDIF

*
*-- set axes (do not show), leave space for the names of variables
*
      CALL HPLFRA (-1.0 - 0.4*RZONE,
     +              1.0 + 0.4*RZONE,
     +             -1.0 - 0.4*RZONE,
     +              1.0 + 0.4*RZONE, 'AB')
*
      CALL HPLSET('?GFON',CFON)
      ICFON  = INT(CFON/10.)
      IF(ICFON.NE.0)THEN
         TFON = SIGN(1,ICFON)*(FLOAT(ABS(ICFON)*10)+1.)
         CALL IGSET('TXFP',TFON)
      ELSE
         CALL IGSET('TXFP',2.)
      ENDIF
      CALL IGSET ('CHHE', 0.03*RZONE)
*
*-- show event number (in the middle/bottom of the screen)
*
      CALL IGSET ('TXAL', 20.0)
      CHTMP = 'Event number '
      CALL IZITOC (ICHEVT, CHTMP(14:))
      CALL ITX (0.0, -1.0-0.35*RZONE, CHTMP)
*
* Get wedge angle for animated spider
*
      IF (SPIDER_TYPE.EQ.2) THEN
         CALL HPLSET('?BARW',BARW)
         BARW = MIN(1.,MAX(BARW,0.))
      ENDIF
*
*-- main part of the subroutine --*
*
      RANGLE  = 0.0
      RDANGLE = 2.*PI / NVARS
*
*-- store coords of the middle of spider
*
      RXT(1) = 0.0
      RYT(1) = 0.0
*
      CALL IGSET ('LTYP', 1.0)
      CALL IGSET ('LWID', 1.0)
      CALL IGSET ('PLCI', 1.0)
*
*-- draw axes for every variable and compute their values (main loop)
*
      ICVARS=0
      istart = 1
      DO 30 I = 1, NVARS
         IF (IVART(I).GT.1) GOTO 30
*
* Check we do not exceed the point array limit
*
         IF(ICVARS.GE.MXPOIN) GOTO 30
         ICVARS=ICVARS+1
*
         COSANG = COS(RANGLE)
         SINANG = SIN(RANGLE)
*
* Position of end of spider radial line from centre
*
         RXT(2) = COSANG
         RYT(2) = SINANG
         CALL IPL (2, RXT, RYT)
*
*-- text -> varible names and values
*
         IF (RANGLE.GT.0.5*PI .AND. RANGLE.LE.1.5*PI) THEN
            RTXAL = 30.0
         ELSE
            RTXAL = 0.0
         ENDIF
*
         IF (IZONE.EQ.1) THEN
            IF (IVART(I).EQ.1) THEN
               CALL IZRTOC (LOW(I), CLOW)
               CALL IZRTOC (HIGH(I), CHIGH)
            ELSE
               CALL IZITOC (INT(LOW(I)), CLOW)
               CALL IZITOC (INT(HIGH(I)), CHIGH)
            ENDIF
         ENDIF
*
*-- remove blank characters from CLOW and CHIGH
*
         NL = LENOCC (CLOW)
         DO 10 IC = 1, NL
            IF (CLOW(1:1).EQ.' ') CLOW = CLOW(2:)
   10    CONTINUE
*
         NL = LENOCC (CHIGH)
         DO 20 IC = 1, NL
            IF (CHIGH(1:1).EQ.' ') CHIGH = CHIGH(2:)
   20    CONTINUE
*
  700    CONTINUE
         W = CURRENT(I)
         IF (IVART(I).EQ.1) THEN
            CALL IZRTOC (W, CVAR)
         ELSEIF (IVART(I).EQ.0) THEN
            CALL IZITOC (INT (W), CVAR)
         ELSE
            IF (JBIT (INT (LOW(I)), 32) .EQ. 1) THEN
               CALL PHXTOC (INT (W), CVAR)
            ELSE
               CALL IZITOC (INT (W), CVAR)
            ENDIF
         ENDIF
*
         IF (RANGLE.GE.0.0 .AND. RANGLE.LT.PI) THEN
            CALL IGSET ('TXAL', RTXAL)
*
               CURNAME = CNAMES(ISTART:
     +           ISTART+INDEX(CNAMES(ISTART:),' ')-1)
               IF (IZONE.EQ.1) THEN
               CHTMP = CURNAME
               ILEN = LENOCC (CHTMP)
               CHTMP(ILEN+1:) = '='
               CHTMP(ILEN+2:) = CVAR
               CALL ITX (RXT(2)*1.03,
     +                   RYT(2)*1.03 + SIGN(0.06*RZONE,RYT(2)),
     +                   CHTMP)
               CHTMP = CLOW
               ILEN = LENOCC (CHTMP)
               CHTMP(ILEN+1:) = '<'
               CHTMP(ILEN+2:) = CURNAME
               ILEN = LENOCC (CHTMP)
               CHTMP(ILEN+1:) = '<'
               CHTMP(ILEN+2:) = CHIGH
               CALL IGTCCH(CHTMP)
               CALL ITX (RXT(2)*1.03, RYT(2)*1.03, CHTMP)
            ELSE
               CHTMP = CURNAME
               ILEN = LENOCC (CHTMP)
               CHTMP(ILEN+1:) = '='
               CHTMP(ILEN+2:) = CVAR
               CALL ITX (RXT(2)*1.03, RYT(2)*1.03, CHTMP)
            ENDIF
            ISTART = ISTART+INDEX(CNAMES(ISTART:),' ')
*
         ELSE
            CALL IGSET ('TXAL', RTXAL+1.0)
            CURNAME = CNAMES(ISTART:ISTART+INDEX(CNAMES(ISTART:),' ')-1)
            CHTMP = CURNAME
            ILEN = LENOCC (CHTMP)
            CHTMP(ILEN+1:) = '='
            CHTMP(ILEN+2:) = CVAR
            CALL ITX (RXT(2)*1.03, RYT(2)*1.03, CHTMP)
            IF (IZONE.EQ.1) THEN
               CHTMP = CLOW
               ILEN = LENOCC (CHTMP)
               CHTMP(ILEN+1:) = '<'
               CHTMP(ILEN+2:) = CURNAME
               ILEN = LENOCC (CHTMP)
               CHTMP(ILEN+1:) = '<'
               CHTMP(ILEN+2:) = CHIGH
               CALL IGTCCH(CHTMP)
               CALL ITX (RXT(2)*1.03,
     +                   RYT(2)*1.03 + SIGN(0.06*RZONE,RYT(2)),
     +                   CHTMP)
            ENDIF
            ISTART = ISTART+INDEX(CNAMES(ISTART:),' ')+1
*
         ENDIF
*
         IF(SPIDER_TYPE.EQ.1) THEN
            IF (HIGH(I).ne.LOW(I)) THEN
               AW = (W - LOW(I))/(HIGH(I) - LOW(I))
            ELSE
               AW = 0.5
            ENDIF
            RXE(ICVARS) = COSANG * AW
            RYE(ICVARS) = SINANG * AW
         ENDIF
*
         IF (AVRG) THEN
            W = AVG(I)
            IF (HIGH(I).ne.LOW(I)) THEN
               AW = (W - LOW(I))/(HIGH(I) - LOW(I))
            ELSE
               AW = 0.5
            ENDIF
            RXA(ICVARS) = COSANG * AW
            RYA(ICVARS) = SINANG * AW
         ENDIF
*
*-- next variable
*
         RANGLE = RANGLE + RDANGLE
*
   30 CONTINUE
*
*-- for contour drawing
*
      IF(ICVARS.GE.MXPOIN) THEN
         WRITE(6,*) 'PASPI2 -- Too many Ntuple variables to Spider-plot'
         RETURN
      ENDIF
      ICVARS = ICVARS+1
      RXE(ICVARS) = RXE(1)
      RYE(ICVARS) = RYE(1)
*
*-- average?
*
      IF (AVRG) THEN
         RXA(ICVARS) = RXA(1)
         RYA(ICVARS) = RYA(1)
      ENDIF
*
*-- CREATE WEDGES FOR ANIMATED SPIDER, OR FILL INTERIOR OF SPIDER
*
      CALL HPLATT(1)
      IF(SPIDER_TYPE.EQ.2) THEN
         SANGLE = O2PI*(2.*PI - 0.5*BARW*RDANGLE)
         EANGLE = O2PI*(0.5*BARW*RDANGLE)
         DO 40 I=1,NVARS
            IF (IVART(I).GT.2) GO TO 40
            W = CURRENT(I)
            IF (HIGH(I).ne.LOW(I)) THEN
               AW = (W - LOW(I))/(HIGH(I) - LOW(I))
            ELSE
               AW = 0.5
            ENDIF
            CALL IGARC(0.,0.,0.,AW,SANGLE,EANGLE)
            SANGLE = SANGLE + O2PI*RDANGLE
            EANGLE = EANGLE + O2PI*RDANGLE
   40    CONTINUE
         CALL IGSET ('PLCI', 1.0)
         CALL IGSET ('LWID', 1.0)
      ELSE
         CALL IFA (ICVARS-1, RXE, RYE)
*
*-- contour of the event: (when one 'arm' of the spider is too thin
*                          IFA does not draw its -> we have to do it)
*
         CALL IGSET ('PLCI', 1.0)
         CALL IGSET ('LWID', 1.0)
         CALL IPL (ICVARS, RXE, RYE)
      ENDIF
*
*-- average?
*
      IF (AVRG) THEN
         CALL IGSET ('LTYP', 2.0)
         CALL IPL (ICVARS, RXA, RYA)
      ENDIF
*
*-- secondary arcs
*
      CALL IGSET ('LTYP', 3.0)
*
      IF (RZONE.LT.4.0) THEN
       DO 60 R=0.1, 1.0, 0.1
          CALL IGARC (0.0, 0.0, R, R, 0.0, 0.0)
   60    CONTINUE
      ELSE
       DO 70 R=0.25, 1.0, 0.25
          CALL IGARC (0.0, 0.0, R, R, 0.0, 0.0)
   70    CONTINUE
      ENDIF
*
*-- primary arcs
*
      CALL IGSET ('LTYP', 1.0)
      CALL IGSET ('LWID', 6.0)
      CALL IGARC (0.0, 0.0, 1.0, 1.0, 0.0, 0.0)
      CALL IGSET ('LWID', 3.0)
      CALL IGARC (0.0, 0.0, 0.5, 0.5, 0.0, 0.0)
*
      END
