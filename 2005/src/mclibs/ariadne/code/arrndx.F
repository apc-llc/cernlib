C***********************************************************************
C $Id: arrndx.F,v 1.2 1996/04/10 12:33:36 mclareni Exp $

      REAL FUNCTION ARNDX1()

C...Ariadne function RNDom Xt2 version 1

C...Generate an x_t^2 according to a Sudakov suppressed distribution.
C...Suitable for running alpha_QCD

#include "arimpl.f"
#include "arint1.f"


      ARNDX1=0.0
      ARG=RLU(IDUM)
      IF (LOG(ARG)*CN.LT.LOG(LOG(XT2C/XLAM2)/LOG(XT2M/XLAM2))) RETURN
      ARNDX1=XLAM2*(XT2M/XLAM2)**(ARG**CN)

      RETURN

C**** END OF ARNDX1 ****************************************************
      END
C***********************************************************************

      REAL FUNCTION ARNDX2()

C...Ariadne function RNDom Xt2 version 2

C...Generate an x_t^2 according to a Sudakov suppressed distribution.
C...Suitable for constant alpha_QCD and QED emission

#include "arimpl.f"
#include "arint1.f"


      ARNDX2=0.0
      ARG=RLU(IDUM)
      IF (CN*LOG(ARG).LT.(LOG(XT2M))**2-(LOG(XT2C))**2) RETURN
      ARNDX2=EXP(-SQRT((LOG(XT2M))**2-LOG(ARG)*CN))

      RETURN

C**** END OF ARNDX2 ****************************************************
      END
C***********************************************************************

      REAL FUNCTION ARNDX3()

C...Ariadne function RNDom Xt2 version 3

C...Generate an x_t^2 according to a Sudakov suppressed distribution.
C...Suitable for constant alpha_QCD q-qbar emission

#include "arimpl.f"
#include "arint1.f"


      ARNDX3=0.0
      ARG=RLU(IDUM)
      IF (LOG(ARG)*CN.LT.LOG(XT2C/XT2M)) RETURN
      ARNDX3=XT2M*(ARG**CN)

      RETURN

C**** END OF ARNDX3 ****************************************************
      END
C***********************************************************************

      REAL FUNCTION ARNDX4()

C...Ariadne function RNDom Xt2 version 4

C...Generate an x_t^2 according to a Sudakov suppressed distribution.
C...Suitable for O(alpha_S) ME boson-gluon processes

#include "arimpl.f"
#include "arint1.f"


      ARNDX4=0.0
      ARG=RLU(IDUM)
      IF (LOG(ARG)*CN.LT.1.0/XT2M-1.0/XT2C) RETURN
      ARNDX4=1.0/(1.0/XT2M-CN*LOG(ARG))

      RETURN

C**** END OF ARNDX4 ****************************************************
      END
C***********************************************************************
