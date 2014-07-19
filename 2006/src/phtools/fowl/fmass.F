*
* $Id: fmass.F,v 1.1.1.1 1996/03/22 16:42:46 mclareni Exp $
*
* $Log: fmass.F,v $
* Revision 1.1.1.1  1996/03/22 16:42:46  mclareni
* Phtools
*
*
      FUNCTION FMASS (I1,I2,I3,I4,I5,I6)
C=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
C--  CALLED FROM - USER (OPTIONAL)
C--  PURPOSE - CALCULATES THE EFFECTIVE MASS OF UP TO 6 PARTICLES
      EMSQ = FMASSQ (I1,I2,I3,I4,I5,I6)
      FMASS = SQRT(EMSQ)
C*UL  400 RETURN
      RETURN
      END
