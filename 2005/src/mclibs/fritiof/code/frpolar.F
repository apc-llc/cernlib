*
* $Id: frpolar.F,v 1.1.1.1 1996/01/11 14:05:21 mclareni Exp $
*
* $Log: frpolar.F,v $
* Revision 1.1.1.1  1996/01/11 14:05:21  mclareni
* Fritiof
*
*
C********************************* END FRTOCMS ************************
 
C********************************* FRPOLAR ****************************
 
      SUBROUTINE FRPOLAR(DTHE,DPHI,DP)
 
C....FOR A GIVEN PAIR OF MEMENTA DP(2,4), WHICH ARE IN LIGHT-CONE FORM
C....AND IN THEIR CMS FRAME, THIS ROUTINE IS TO FIND THE POLAR ANGEL
C....(THETA,PHI) WITH WHICH A FOLLOW UP CALL TO FRROTAR (ROTATION)
C....WILL MAKE THE PAIRWISE MOMENTA LIE ON THE NEW Z-AXES.
C.......................................................................
 
      IMPLICIT DOUBLE PRECISION (D)
      DIMENSION DP(2,5)
C............................ANGLES OF THE MOMENTUM VECTOR ...............
      DTHE = 0D0
      DPHI = 0D0
      DBP3=.5D0*(DP(1,4)-DP(1,3))
      DXY=(DP(1,1)**2+DP(1,2)**2)
      DXYZ = DXY + DBP3**2
      IF(DXYZ.EQ.0.D0) RETURN
 
      DCTH=DBP3/DFRSQR(DXYZ, 'dcthiowe')
      DCTH=DMAX1(-1.D0,DMIN1(DCTH,1.D0))
      DTHE=DACOS(DCTH)
      IF(DTHE.EQ.0.D0.OR.DXY.EQ.0D0) RETURN
        DCPH = DP(1,1)/DFRSQR(DXY, 'dxyeuw')
      DPHI = DACOS(DCPH)
        IF(DP(1,2).LT.0.D0) DPHI = - DPHI
 
      RETURN
      END
