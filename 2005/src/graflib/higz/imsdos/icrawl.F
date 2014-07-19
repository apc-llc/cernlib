*
* $Id: icrawl.F,v 1.1.1.1 1996/02/14 13:11:03 mclareni Exp $
*
* $Log: icrawl.F,v $
* Revision 1.1.1.1  1996/02/14 13:11:03  mclareni
* Higz
*
*
#if defined(CERNLIB_MSDOS)
#include "higz/pilot.h"
#if defined(CERNLIB_NDP)
*CMZ :  1.14/11 29/06/92  13.51.08  by  Unknown
*-- Author : V.Fine
      FUNCTION ICRAWL(X,Y)
*.
*...  CHANGE POSITION OF THE CURSOR
*.
*..   INPUT:
*.      -----
*.      X, Y - INITIAL POSITION
*.
*..     OUTPUT:
*.      ------
*.      X, Y - NEW POSITION
*.      ICRAWL  =  10*XFLAG + YFLAG
*.                 XFLAG = 1 - X POSITION HAVE BEEN CHANGED
*.                 YFLAG = 1 - Y POSITION HAVE BEEN CHANGED
*.              = -1 KEY "ESC" HAVE BEEN PRESSED OR LEFT BUTTON
*.              =  0 KEY "ENTER" HAVE BEEN PRESSED OR RIGHT BUTTON
#include "higz/hikern.inc"
      INTEGER XMIN,YMIN,XMAX,YMAX,X,Y, CTRL_C, ESC, ENTER
      PARAMETER (CTRL_C=3, ESC=27, RETKEY=13)
      INTEGER K
*
      LOGICAL SAMPLE
      LOGICAL MIDPR,LEFTPR,RIGHTPR
      SAVE MIDPR,LEFTPR,RIGHTPR
      DATA MIDPR,LEFTPR,RIGHTPR /3*.true./
*
      IER = GET_CLIP_LIMITS(XMIN,YMIN,XMAX,YMAX)

      ICRAWL = 0
   10 IF (MOUSE) THEN
         IBUT = 0
         CALL GET_MOUSE(IBUT,IX,IY)
         IF (IX .NE. X) THEN
            IF(IX.GT.XMAX) IX = XMIN
            if(IX.lt.xmin) IX = xmax
            icrawl = icrawl + 10
         END IF

         IF (IY .NE. Y) THEN
            IF(IY.GT.YMAX) IY = YMIN
            IF(IY.LT.YMIN) IY = YMAX
            icrawl = icrawl + 1
         END IF

         IF (ICRAWL .EQ. 0 .AND. IBUT .EQ. 0) then
            MIDPR = .TRUE.
            LEFTPR = .TRUE.
            RIGHTPR = .TRUE.
            GO TO 70
         END IF
         IF (IBUT .GT. 0) THEN
            GO TO (30,20,40,50),IBUT
   20       CONTINUE
*--
*--     LEFT BUTTON ARE PRESSING
*
            IF (.NOT.LEFTPR) GO TO 70
            MIDPR = .TRUE.
            RIGHTPR = .TRUE.
            LEFTPR = .NOT. LEFTPR
            ICRAWL = IBUT - 2
            GO TO 60
   30       CONTINUE
*--
*--     RIGHT BUTTON ARE PRESSING
*
            IF (.NOT.RIGHTPR) GO TO 70
            MIDPR = .TRUE.
            LEFTPR = .TRUE.
            RIGHTPR = .NOT. RIGHTPR
            ICRAWL = IBUT - 2
            GO TO 60
   40       CONTINUE
*--
*--     MIDDLE BUTTON ARE PRESSING
*
   50       CONTINUE
            IF (.NOT.MIDPR) GO TO 70
            RIGHTPR = .TRUE.
            LEFTPR = .TRUE.
            MIDPR = .NOT. MIDPR
            ICRAWL = IBUT - 2
         ELSE
            MIDPR = .TRUE.
            LEFTPR = .TRUE.
            RIGHTPR = .TRUE.
            Y = IY
            X = IX
            CALL SET_MOUSE(X,Y)
         END IF
   60    CONTINUE
         GO TO 80
      END IF
   70 CONTINUE
      K = INKEY$()
      IF (K .EQ. 0) GO TO 10
      IF (K .EQ. CTRL_C) THEN
         CALL IGEND
         CALL ABEND
      END IF
      IF(K.LT.0) THEN
         K = -K
         IF(K.EQ.83) THEN
*--
*--     [DEL] KEY have been pressed
*--     to decrease the speed of cursor
*
            ISTEP = ISTEP-1
            ISTEP = MAX0(ISTEP,1)
            IF (MOUSE) THEN
               IRAT = 64/ISTEP + 1
               CALL SET_MP_RATIO(IRAT,IRAT)
            END IF
            GO TO 10
         END IF
*
         IF(K.EQ.82) THEN
*--
*--     [INS] KEY have been pressed
*--     to increase the speed of cursor
*
            ISTEP = ISTEP+1
            ISTEP = MIN0(ISTEP,100)
            IF (MOUSE) THEN
               IRAT = 64/ISTEP + 1
               CALL SET_MP_RATIO(IRAT,IRAT)
            END IF
            GO TO 10
         END IF
*
         IF(K.LT.71.OR.K.GT.81) GO TO 10
*--
*--        Arrow keys have been pressed
*
*
*              Y MOTION
*
         IF(K.EQ.71.OR.K.EQ.72.OR.K.EQ.73) THEN
            Y = Y - ISTEP
            ICRAWL = ICRAWL + 1
         ELSE IF(K.EQ.79.OR.K.EQ.80.OR.K.EQ.81) THEN
            Y = Y + ISTEP
            ICRAWL = ICRAWL + 1
         END IF
*
*        X MOTION
*
         IF(K.EQ.71.OR.K.EQ.75.OR.K.EQ.79) THEN
            X = X - ISTEP
            ICRAWL = ICRAWL + 10
         ELSE IF(K.EQ.73.OR.K.EQ.77.OR.K.EQ.81) THEN
            X = X + ISTEP
            ICRAWL = ICRAWL + 10
         END IF
         IF(X.LT.XMIN) X = XMAX
         IF(Y.LT.YMIN) Y = YMAX
         IF(X.GT.XMAX) X = XMIN
         IF(Y.GT.YMAX) Y = YMIN
         IF (MOUSE) CALL SET_MOUSE(X,Y)
      ELSE IF (K .EQ. ESC) THEN
         ICRAWL = -1
      ELSE IF (K .EQ. RETKEY) THEN
         ICRAWL =  0
      ELSE
         GO TO 10
      END IF
   80 RETURN
      END
#endif
#endif
