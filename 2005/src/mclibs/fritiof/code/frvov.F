*
* $Id: frvov.F,v 1.1.1.1 1996/01/11 14:05:20 mclareni Exp $
*
* $Log: frvov.F,v $
* Revision 1.1.1.1  1996/01/11 14:05:20  mclareni
* Fritiof
*
*
C********************************* END FROVLAP **************************
 
C************************  FUNCTION FRVOV ********************************
 
      FUNCTION FRVOV(R2)
 
C...... Gives the value of overlap function at b^2=R2.
C...... KFR(5)=IQ=0, eikonal, IQ=1,Gaussian, IQ=2, grey disk.
 
      PARAMETER (KSZ1=20)
      COMMON/FRPARA1/KFR(KSZ1),VFR(KSZ1)
      COMMON/FRINTN0/PLI0(2,4),AOP(KSZ1),IOP(KSZ1),NFR(KSZ1)
      SAVE /FRPARA1/,/FRINTN0/
 
      IF(KFR(5).EQ.0) THEN
      OMEGA0 = AOP(3)
      BETA = AOP(4)
      FRVOV = 1.-FRREX( -2.*OMEGA0*FRREX(-BETA*R2))
            ELSE IF (KFR(5).EQ.1) THEN
      GAUA = AOP(5)
      GAUB = AOP(6)
            FRVOV =1.-(1.-GAUA*FRREX(-GAUB*R2))**2
            ELSE IF (KFR(5).EQ.2) THEN
      REC02 = AOP(7)**2
      ALFA = AOP(8)
         IF (R2.LT.REC02) THEN
         FRVOV=ALFA
         ELSE
         FRVOV=0.
         ENDIF
      ENDIF
 
      RETURN
      END
