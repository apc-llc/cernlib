*
* $Id: ihsvie.F,v 1.1.1.1 1996/02/14 13:10:54 mclareni Exp $
*
* $Log: ihsvie.F,v $
* Revision 1.1.1.1  1996/02/14 13:10:54  mclareni
* Higz
*
*
#include "higz/pilot.h"
*CMZ :  1.20/07 13/01/94  14.39.17  by  O.Couet
*-- Author :
      SUBROUTINE IHSVIE(RMIN,RMAX,PHI,THETA,PSI,IREP)
************************************************************************
*                                                                      *
*     IHSVIE                                     Date:    13.08.90     *
*     Author: E. Chernyaev (IHEP/Protvino)       Revised:              *
*                                                                      *
*     Function: Set view direction (in spherical coordinates)          *
*                                                                      *
*     References: IHWV01, IHWV02                                       *
*                                                                      *
*     Input: RMIN(3) - MIN -                                           *
*            RMAX(3) -       MAX scope                                 *
*                                                                      *
*            PHI     - longitude                                       *
*            THETA   - latitude (angle between +Z and view direction)  *
*            PSI     - rotation in screen plane                        *
*                                                                      *
*     Output: IREP   - reply (-1 if error in min-max)                  *
*                                                                      *
*     Errors: error in min-max scope                                   *
*                                                                      *
************************************************************************
#include "higz/hctnor.inc"
#include "higz/hcscrn.inc"
      REAL              RMIN(3),RMAX(3),CENTRE(3),SCALE(3)
      DOUBLE PRECISION  DRAD,C1,S1,C2,S2,C3,S3
*-
      DRAD    = DATAN(1.D0)*4./180.
*
**          F I N D   C E N T E R   O F   S C O P E   A N D
**          S C A L E   F A C T O R S
*
      CALL IHWV01(RMIN,RMAX,SCALE,CENTRE,IREP)
      IF (IREP .LT. 0)                  GOTO 998
*
**          S E T   T R A N S F O R M A T I O N   M A T R I X E S
*
      C1     = DCOS(PHI*DRAD)
      S1     = DSIN(PHI*DRAD)
      C2     = DCOS(THETA*DRAD)
      S2     = DSIN(THETA*DRAD)
      C3     = DCOS(PSI*DRAD)
      S3     = DSIN(PSI*DRAD)
      CALL IHWV02(SCALE,CENTRE,C1,S1,C2,S2,C3,S3,TNORM,TBACK)
      C3     = 1.
      S3     = 0.
      CALL IHWV02(SCALE,CENTRE,C1,S1,C2,S2,C3,S3,TN,TB)
      RETURN
*
  998 WRITE(*,*) 'IHSVIE: error in min-max scope'
      END
