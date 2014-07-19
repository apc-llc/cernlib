C***********************************************************************
C $Id: arveto.F,v 1.2 1996/04/10 12:33:45 mclareni Exp $

      REAL FUNCTION ARVET1()

C...ARiadne function VETo factor version 1

C...Determine the acceptance factor for chosen x_t^2 and y
C...Suitable for photon emission with constant alpha_EM

#include "arimpl.f"
#include "ardat1.f"
#include "arint1.f"


      ARVET1=0.0
      IF (B2.LE.0) RETURN
      ARVET1=-((FQ1*(1.0-B1)/B2-FQ3*(1.0-B3)/B2)**2)*
     $         (B1**NXP1+B3**NXP3)*(YMAX-YMIN)*0.5/LOG(XT2)

      IF (MSTA(19).EQ.0) RETURN

      ARVET1=ARVET1*ARVETH()

      RETURN

C**** END OF ARVET1 ****************************************************
      END
C***********************************************************************

      REAL FUNCTION ARVET2()

C...ARiadne function VETo factor version 2

C...Determine the acceptance factor for chosen x_t^2 and y
C...Suitable for photon emission with running alpha_EM

#include "arimpl.f"
#include "arint1.f"


      ARVET2=ARVET1()*ULALEM(XT2*S)/ULALEM(0.25*S)

      RETURN

C**** END OF ARVET2 ****************************************************
      END
C***********************************************************************

      REAL FUNCTION ARVET3()

C...ARiadne function VETo factor version 3

C...Determine the acceptance factor for chosen x_t^2 and y
C...Suitable for gluon emission with constant alpha_QCD

#include "arimpl.f"
#include "ardat1.f"
#include "arint1.f"


      ARVET3=-(B1**NXP1+B3**NXP3)*(YMAX-YMIN)*0.5/LOG(XT2)

      IF (MSTA(19).EQ.0) RETURN

      ARVET3=ARVET3*ARVETH()


      RETURN

C**** END OF ARVET3 ****************************************************
      END
C***********************************************************************

      REAL FUNCTION ARVET4()

C...ARiadne function VETo factor version 4

C...Determine the acceptance factor for chosen x_t^2 and y
C...Suitable for gluon emission with running alpha_QCD

#include "arimpl.f"
#include "ardat1.f"
#include "arint1.f"


      ARVET4=(B1**NXP1+B3**NXP3)*(YMAX-YMIN)*0.5/YINT

      IF (MSTA(19).EQ.0) RETURN

      ARVET4=ARVET4*ARVETH()


      RETURN

C**** END OF ARVET4 ****************************************************
      END
C***********************************************************************

      REAL FUNCTION ARVET5()

C...ARiadne function VETo factor version 5

C...Determine the acceptance factor for chosen x_t^2 and y
C...Suitable for q-qbar emission

#include "arimpl.f"
#include "ardat1.f"
#include "arint1.f"


      ARVET5=((1.0D0-B3+Y3)**2+(1.0D0-B2+Y2)**2)*XT*
     $         (EXP(-YMIN)-EXP(-YMAX))/YINT

      IF (MSTA(23).EQ.1) ARVET5=ARVET5*(BC1-B1)/(1.0D0-B1+Y1)

      RETURN

C**** END OF ARVET5 ****************************************************
      END
C***********************************************************************

      REAL FUNCTION ARVET6()

C...ARiadne function VETo factor version 6

C...Determine the acceptance factor for chosen x_t^2 and y
C...Suitable for gluon emission with running alpha_QCD according to
C...O(alpha_S) gluon emission for lepto production

#include "arimpl.f"
#include "ardat1.f"
#include "arint1.f"
#include "arint4.f"
#include "arhide.f"


      BCNST0=0.0
      Y3P=ZSQEV*Y3
      Y2P=ZSQEV*Y2
      Y1P=ZSQEV*Y1
      B3P=1.0-ZSQEV*(1.0-B3)
      B1P=B1-(Y1P-Y3P)*(1.0-ZSQEV)/ZSQEV
      B2P=2.0-B1P-B3P
      BASSX1=B1P
      BASSX3=B3P

      B13=MAX(1.0-B3P+Y3P,BCNST0)
      B12=MAX(1.0-B2P+Y2P,BCNST0)
      B11=MAX(1.0-B1P+Y1P,BCNST0)
      B13Q=B13+SQ2
      CG1=((B12/B3P)**2+(SQ2/B13Q)**2)*SQ2/B13Q
      CG2=2.0*(B12*SQ2/(B13Q*B3P)+1.0)*B13*B11*SQ2/(B3P*B13Q**2)
      CG3=4.0*YFAC*B11*B12*B13*(SQ2**2)/((B3P**2)*B13Q**3)
      CG0=(3.0+4.0*YFAC/27.0)
      IF (MHAR(116).LE.0) THEN
        CG1=(B12/B3P)**2+(SQ2/B13Q)**2
        CG2=2.0*(B12*SQ2/(B13Q*B3P)+1.0)*B13*B11/(B3P*B13Q)
        CG3=4.0*YFAC*B11*B12*B13*SQ2/((B3P**2)*B13Q**2)
        CG0=(6.0+0.25*YFAC)
        IF (MHAR(116).LT.0) CG0=CG0*SQRT(B13Q/SQ2)
      ENDIF

      ARVET6=(YMAX-YMIN)*(CG1+CG2+CG3)/(YINT*CG0)

      IF (MSTA(19).EQ.0) RETURN

      IF (MSTA(19).EQ.2) ARVET6=ARVET6*
     $     MIN(1.0,LOG(XT2/XLAM2)/LOG(PARA(21)*SQ2/XLAM2))

      ARVET6=ARVET6*ARVETH()


      RETURN

C**** END OF ARVET6 ****************************************************
      END
C***********************************************************************

      REAL FUNCTION ARVET7()

C...ARiadne function VETo factor version 7

C...Determine the acceptance factor for chosen x_t^2 and y
C...Suitable for gluon emission with constant alpha_QCD according to
C...O(alpha_S) gluon emission for lepto production


#include "arimpl.f"
#include "ardat1.f"
#include "arint1.f"
#include "arint4.f"
#include "arhide.f"

      BCNST0=0.0
      Y3P=ZSQEV*Y3
      Y2P=ZSQEV*Y2
      Y1P=ZSQEV*Y1
      B3P=1.0-ZSQEV*(1.0-B3)
      B1P=B1-(Y1P-Y3P)*(1.0-ZSQEV)/ZSQEV
      B2P=2.0-B1P-B3P
      BASSX1=B1P
      BASSX3=B3P

      B13=MAX(1.0-B3P+Y3P,BCNST0)
      B12=MAX(1.0-B2P+Y2P,BCNST0)
      B11=MAX(1.0-B1P+Y1P,BCNST0)
      B13Q=B13+SQ2
      CG1=((B12/B3P)**2+(SQ2/B13Q)**2)*SQ2/B13Q
      CG2=2.0*(B12*SQ2/(B13Q*B3P)+1.0)*B13*B11*SQ2/(B3P*B13Q**2)
      CG3=4.0*YFAC*B11*B12*B13*(SQ2**2)/((B3P**2)*B13Q**3)
      CG0=(3.0+4.0*YFAC/27.0)
      IF (MHAR(116).LE.0) THEN
        CG1=(B12/B3P)**2+(SQ2/B13Q)**2
        CG2=2.0*(B12*SQ2/(B13Q*B3P)+1.0)*B13*B11/(B3P*B13Q)
        CG3=4.0*YFAC*B11*B12*B13*SQ2/((B3P**2)*B13Q**2)
        CG0=(6.0+0.25*YFAC)
        IF (MHAR(116).LT.0) CG0=CG0*SQRT(B13Q/SQ2)
      ENDIF

      ARVET7=-(YMAX-YMIN)*(CG1+CG2+CG3)/(LOG(XT2)*CG0)

      IF (MSTA(19).EQ.0) RETURN

      ARVET7=ARVET7*ARVETH()


      RETURN

C**** END OF ARVET7 ****************************************************
      END
C***********************************************************************

      REAL FUNCTION ARVETH()

C...ARiadne function Heavy VETo factor

C...Extra acceptance factor for heavy dipoles

#include "arimpl.f"
#include "arint1.f"


      ARVETH=0.0
      BX1=1.0-B1+Y1-Y3
      BX3=1.0-B3+Y3-Y1
      IF (B2.GE.1.0.OR.BX1.LE.0.OR.BX3.LE.0) RETURN
      BXM=BX1/BX3
      ARVETH=1.0-(Y1*BXM+Y3/BXM)/(1.0-B2)

      RETURN

C**** END OF ARVETH ****************************************************
      END
C***********************************************************************

      REAL FUNCTION ARVET8()

C...ARiadne function VETo factor version 8

C...Determine the acceptance factor for chosen x_t^2 and y
C...Suitable for q-qbar emission

#include "arimpl.f"
#include "arint1.f"


      ARVET8=((1.0D0-B3+Y3)**2+(1.0D0-B2+Y2)**2)*(BC3-B3)*
     $         (YMAX-YMIN)/YINT

      RETURN

C**** END OF ARVET8 ****************************************************
      END
