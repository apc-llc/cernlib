*
* $Id: ihpvie.F,v 1.1.1.1 1996/02/14 13:10:53 mclareni Exp $
*
* $Log: ihpvie.F,v $
* Revision 1.1.1.1  1996/02/14 13:10:53  mclareni
* Higz
*
*
#include "higz/pilot.h"
*CMZ :  1.20/07 13/01/94  14.40.02  by  O.Couet
*-- Author :
      SUBROUTINE IHPVIE(RMIN,RMAX,P,PSI,IREP)
************************************************************************
*                                                                      *
*     IHPVIE                                     Date:    13.08.90     *
*     Author: E. Chernyaev (IHEP/Protvino)       Revised:              *
*                                                                      *
*     Function: Set view direction using eye point                     *
*                                                                      *
*     References: IHWV01, IHWV02                                       *
*                                                                      *
*     Input: RMIN(3) - MIN -                                           *
*            RMAX(3) -       MAX scope                                 *
*                                                                      *
*            P(3)    - eye point                                       *
*            PSI     - rotation in screen plane                        *
*                                                                      *
*     Output: IREP   - reply :-1 - error in min-max                    *
*                             -2 - error in eye point                  *
*                                                                      *
*     Errors: error in min-max scope                                   *
*                                                                      *
************************************************************************
#include "higz/hctnor.inc"
#include "higz/hcscrn.inc"
      REAL              RMIN(3),RMAX(3),P(3),CENTRE(3),SCALE(3)
      DOUBLE PRECISION  DRAD,C1,S1,C2,S2,C3,S3
      DOUBLE PRECISION  DDXX,DDYY,DDZZ,S,SS
*-
      DRAD    = DATAN(1.D0)*4./180.
*
**          F I N D   C E N T E R   O F   S C O P E   A N D
**          S C A L E   F A C T O R S
*
      CALL IHWV01(RMIN,RMAX,SCALE,CENTRE,IREP)
      IF (IREP .LT. 0)                  GOTO 997
*
**          S E T   T R A N S F O R M A T I O N   M A T R I X E S
*
      DDXX   = P(1) - CENTRE(1)
      DDYY   = P(2) - CENTRE(2)
      DDZZ   = P(3) - CENTRE(3)
      S      = DSQRT(DDXX*DDXX + DDYY*DDYY)
      SS     = DSQRT(DDXX*DDXX + DDYY*DDYY+ DDZZ*DDZZ)
      IF (SS .EQ. 0.)                   GOTO 998
      C1     = 1.
      S1     = 0.
      C2     = 1.
      S2     = 0.
      IF (S .EQ. 0.)                    GOTO 200
      C1     = DDXX / S
      S1     = DDYY / S
      C2     = DDZZ / SS
      S2     = S / SS
  200 C3     = DCOS(PSI*DRAD)
      S3     = DSIN(PSI*DRAD)
      CALL IHWV02(SCALE,CENTRE,C1,S1,C2,S2,C3,S3,TNORM,TBACK)
      C3     = 1.
      S3     = 0.
      CALL IHWV02(SCALE,CENTRE,C1,S1,C2,S2,C3,S3,TN,TB)
      RETURN
*
  997 WRITE(*,*) 'IHPVIE: error in min-max scope'
      IREP   =-1
      RETURN
  998 WRITE(*,*) 'IHPVIE: error in eye point'
      IREP   =-2
      END
