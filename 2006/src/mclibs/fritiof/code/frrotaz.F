*
* $Id: frrotaz.F,v 1.1.1.1 1996/01/11 14:05:22 mclareni Exp $
*
* $Log: frrotaz.F,v $
* Revision 1.1.1.1  1996/01/11 14:05:22  mclareni
* Fritiof
*
*
C********************************* END FRROTAY ************************
 
C********************************* FRROTAZ ****************************
 
      SUBROUTINE FRROTAZ(DPHI, DPV)
 
C:  ROTATE COORDINATES AROUND Z-AXES BY AN ANGLE DPHI
C:  DPV(3) GIVES THE SPACE COMPONENTS OF A VECTOR.
 
      IMPLICIT DOUBLE PRECISION (D)
      DIMENSION DPV(4)
      IF(DPHI**2.LT.1D-20) RETURN
 
      DPVX=DPV(1)*DCOS(DPHI)+DPV(2)*DSIN(DPHI)
      DPVZ = DPV(3)
      DPVY=-DPV(1)*DSIN(DPHI)+DPV(2)*DCOS(DPHI)
 
      DPV(1) = DPVX
      DPV(2) = DPVY
      DPV(3) = DPVZ
 
      RETURN
      END
