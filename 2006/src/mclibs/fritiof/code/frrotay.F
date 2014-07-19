*
* $Id: frrotay.F,v 1.1.1.1 1996/01/11 14:05:22 mclareni Exp $
*
* $Log: frrotay.F,v $
* Revision 1.1.1.1  1996/01/11 14:05:22  mclareni
* Fritiof
*
*
C********************************* END FRROTAR ************************
 
C********************************* FRROTAY ****************************
 
      SUBROUTINE FRROTAY(DTHE, DPV)
 
C:  ROTATE COORDINATES AROUND Y-AXIS BY AN ANGLE DTHE
C:  DPV(3) GIVES THE SPACE COMPONENTS OF A VECTOR.
 
      IMPLICIT DOUBLE PRECISION (D)
      DIMENSION DPV(4)
      IF(DTHE**2.LT.1D-20) RETURN
 
      DPVX=DPV(1)*DCOS(DTHE)-DPV(3)*DSIN(DTHE)
      DPVY = DPV(2)
      DPVZ=DPV(1)*DSIN(DTHE)+DPV(3)*DCOS(DTHE)
 
      DPV(1) = DPVX
      DPV(2) = DPVY
      DPV(3) = DPVZ
 
      RETURN
      END
