*
* $Id: is1trk.F,v 1.10 2001/10/08 14:03:16 mclareni Exp $
*
* $Log: is1trk.F,v $
* Revision 1.10  2001/10/08 14:03:16  mclareni
* Version 7.58 from author
*
*
#include "sys/CERNLIB_machine.h"
#include "pilot.h"
      SUBROUTINE IS1TRK(ID,XYZ,PXYZ)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods :
C-      User supplied subroutine for one track ISAJET event generation
C-      This example returns a randomly distributed 50 GeV electron
C-      -1.3> eta>1.3
C-
C-   Outputs :
C-     ID      = particle ID (use ISAJET id's)
C-     XYZ(3)  = x,y,z starting point
C-     PXYZ(3) = px,py,pz of particle
C-
C-
C-   Created   6-MAR-1989   Serban D. Protopopescu
C-
C----------------------------------------------------------------------
#if defined(CERNLIB_IMPNONE)
      IMPLICIT NONE
#endif
      REAL    XYZ(3),PXYZ(3)
      REAL    PHI,SNTH,CSTH,RANF,ETA,THETA
      INTEGER ID
C----------------------------------------------------------------------
C
      ID=12
      ETA=-1.3+2.6*RANF()
      THETA=2.*ATAN(EXP(-ETA))
      CSTH=COS(THETA)
      SNTH=SQRT(1.-CSTH**2)
      PHI=6.283185*RANF()
      PXYZ(1)=50.*SNTH*COS(PHI)
      PXYZ(2)=50.*SNTH*SIN(PHI)
      PXYZ(3)=50.*CSTH
      CALL INTVTX(XYZ)            ! get random XYZ, standard D0 sigma
  999 RETURN
      END
