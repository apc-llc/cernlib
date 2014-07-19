*
* $Id: cube.F,v 1.1.1.1 1996/03/01 11:39:05 mclareni Exp $
*
* $Log: cube.F,v $
* Revision 1.1.1.1  1996/03/01 11:39:05  mclareni
* Paw
*
*
#include "paw/pilot.h"
*CMZ :  2.04/00 25/10/93  12.58.37  by  Rene Brun
*-- Author :    O.Couet   03/09/92
      SUBROUTINE GETANG(PWTHT,PWPHI)
      PARAMETER (WIN=120.)
      DIMENSION SCOL1(4),SCOL2(4)
      SAVE SCOL1,SCOL2
      SAVE IWINID
      SAVE PHISCA,THTSCA,PHIDEF,THTDEF
      SAVE RMIN,RMAX
      SAVE IFIRST
      REAL PHISCA,THTSCA,PHIDEF,THTDEF
      REAL RMIN(3),RMAX(3)
      DATA RMIN/-1.1,-1.1,-1.1/
      DATA RMAX/+1.1,+1.1,+1.1/
      DATA IFIRST /0/
*
      IF(IFIRST.EQ.0)THEN
         SCOL1(1) = 0.
         SCOL2(1) = 1.
         CALL IGQ('RGB ',SCOL1)
         CALL IGQ('RGB ',SCOL2)
         CALL ISCR(1,0,0.25,0.25,0.25)
         CALL ISCR(1,1,1.,1.,1.)
         PHISCA = -360.
         THTSCA = 180.
         PHIDEF = -120.
         THTDEF = 60.
         CALL IXSAVWI
         IWINID = IXOPNWI(400,400,INT(WIN),INT(WIN),1,' ',1)
         CALL IXSELWI(IWINID)
         CALL IXSETLC(1)
         CALL IXS2BUF(IWINID,1)
         CALL IXCLRWI
         IX    = NINT(PWPHI)
         IY    = NINT(PWTHT)
         U     = 2.*(FLOAT(IX)/WIN)-1.
         V     = 2.*(FLOAT(IY)/WIN)-1.
         PHI   = PHISCA*U + PHIDEF
         THETA = THTSCA*V + THTDEF
         CALL IHSVIE(RMIN,RMAX,PHI,THETA,0.,IREP)
         CALL DRAWCU
         CALL IXSETCO(1,0.,0.,0.)
         CALL IXSETCO(0,1.,1.,1.)
         CALL IXSETLC(1)
         CALL IXSETWI
         IFIRST = 1
         RETURN
      ENDIF
      CALL IXSAVWI
      CALL IXSELWI(IWINID)
      CALL IXSETCO(0,0.25,0.25,0.25)
      CALL IXSETCO(1,1.,1.,1.)
      CALL IXSETLC(1)
*
** Top of the LOOP erase/draw loop after first time.
*
      IX    = NINT(PWPHI)
      IY    = NINT(PWTHT)
      U     = 2.*(FLOAT(IX)/WIN)-1.
      V     = 2.*(FLOAT(IY)/WIN)-1.
      PHI   = PHISCA*U + PHIDEF
      THETA = THTSCA*V + THTDEF
      CALL IHSVIE(RMIN,RMAX,PHI,THETA,0.,IREP)
      CALL IXCLRWI
      CALL DRAWCU
*
      PWPHI = -PHI-90.0
      PWTHT = 90.0-THETA
*
      CALL IXSETWI
      CALL IXSETCO(1,0.,0.,0.)
      CALL IXSETCO(0,1.,1.,1.)
      CALL ISCR(1,0,SCOL1(2),SCOL1(3),SCOL1(4))
      CALL ISCR(1,1,SCOL2(2),SCOL2(3),SCOL2(4))
      CALL IXSETLC(1)
      END

**********************************************************************
***         Routine to draw the cube
**    Stored as 8 vertices, X,Y,Z, 12 winged edges (1st three = XYZ
**    axes), with EV1, EV2 = indices of vertex edges, ELFC, ERFC =
**    indices of the Left and Right faces of directed winged edge.
**    FCNORM = the normals of the faces.
**
      SUBROUTINE DRAWCU
      DIMENSION X(8),Y(8),Z(8)
* Default X,Y,Z Axis parameters
      INTEGER DASHED(2)
      REAL VX(2),VY(2),VZ(2)
      INTEGER EV1(12),EV2(12), ELFC(12), ERFC(12)
      REAL FCNORM(3,6)
* Parameters of cube as 12 winged edges, 6 shared face normals
      DATA X/-1.,1.,1.,-1.,-1.,1.,1.,-1./
      DATA Y/-1.,-1.,1.,1.,-1.,-1.,1.,1./
      DATA Z/-1.,-1.,-1.,-1.,1.,1.,1.,1./
      DATA EV1/1,1,1,2,5,6,7,8,3,2,3,4/
      DATA EV2/2,4,5,3,6,7,8,5,4,6,7,8/
      DATA ELFC/2,1,5,3,6,6,6,6,4,2,3,4/
      DATA ERFC/1,5,2,1,2,3,4,5,1,3,4,5/
      DATA FCNORM/0.,0.,-1., 0.,-1.,0., 1.,0.,0., 0.,1.,0., -1.,0.,0.,
     +0.,0.,1./
      DATA DASHED/2,2/

* Axes = first 3 edges. Dotted if invisible.
      DO 10 I=1,3
         IF (ISEDGE(ELFC(I),ERFC(I),FCNORM).EQ.0) THEN
            CALL IXSETLS(2,DASHED)
         ELSE
            CALL IXSETLS(0,0)
         ENDIF
         CALL IXSETLN(I+1)
         I1 = EV1(I)
         I2 = EV2(I)
         VX(1) = X(I1)
         VY(1) = Y(I1)
         VZ(1) = Z(I1)
         VX(2) = X(I2)
         VY(2) = Y(I2)
         VZ(2) = Z(I2)
         CALL IXPL3(2,VX,VY,VZ)
   10 CONTINUE
      CALL IXSETLN(1)
      CALL IXSETLS(0,0)
* Other edges
      DO 20 I=4,12
         IF (ISEDGE(ELFC(I),ERFC(I),FCNORM).NE.0) THEN
            I1 = EV1(I)
            I2 = EV2(I)
            VX(1) = X(I1)
            VY(1) = Y(I1)
            VZ(1) = Z(I1)
            VX(2) = X(I2)
            VY(2) = Y(I2)
            VZ(2) = Z(I2)
            CALL IXPL3(2,VX,VY,VZ)
         ENDIF
   20 CONTINUE
      CALL IXUPDWI(1)
*
      END

**********************************************************************
** Compute visibility of a winged edge from camera on plus Z axis.
** A winged edge Vij = x(j) - x(i) lies between two faces LEFT and RIGHT
**   whose normals are defined by the cross product of convex edges.
**
**   Need the number of the left face, and the number of the right face,
**   plus the array of face normal vectors.
**   If EITHER rotated face normal has dot product > 0 with the
**    viewing direction, edge is visible
**
      FUNCTION ISEDGE(NFACLF,NFACRT,FCNORM)
      DIMENSION FCNORM(3,6),FNLF(3),FNRT(3)
      ISEDGE = 0
      CALL IHWTON(FCNORM(1,NFACLF),FNLF)
      CALL IHWTON(FCNORM(1,NFACRT),FNRT)
      IF ((FNLF(3).GT.0.0).OR.(FNRT(3).GT.0.0)) ISEDGE = 1
      END

      SUBROUTINE IXPL3(N,XI,YI,ZI)
      PARAMETER (WIN=120.)
      DIMENSION XI(2),YI(2),ZI(2)
      INTEGER*2 IPOS(2,2)
      DIMENSION RW(3),RN(3)
*
      DO 10 I=1,N
         RW(1)=XI(I)
         RW(2)=YI(I)
         RW(3)=ZI(I)
         CALL IHWTON(RW,RN)
         IPOS(1,I)=INT(WIN*((RN(1)+1)/2.))
         IPOS(2,I)=INT(WIN-WIN*((RN(2)+1)/2.))
   10 CONTINUE
      CALL IXLINE(N,IPOS)
*
      END
