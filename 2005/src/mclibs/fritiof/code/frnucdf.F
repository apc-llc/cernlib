*
* $Id: frnucdf.F,v 1.1.1.1 1996/01/11 14:05:20 mclareni Exp $
*
* $Log: frnucdf.F,v $
* Revision 1.1.1.1  1996/01/11 14:05:20  mclareni
* Fritiof
*
*
C********************************* END FRNUCOD ***************************
 
C********************************* FRNUCDF ******************************
 
      SUBROUTINE FRNUCDF(NA,A0,A2,A4,RMAX3)
 
C --- this subroutine determines parameters of nucleon density
C       distribution of deformed nuclei
 
      PARAMETER (KSZ1=20)
      COMMON/FRPARA1/KFR(KSZ1),VFR(KSZ1)
      SAVE /FRPARA1/
 
      BETA2S=VFR(4)*VFR(4)
      BETA4S=VFR(5)*VFR(5)
      R0=1.16*NA**.3333333-1.35*NA**(-.3333333)
      A0=R0*(1.-.0795774*(BETA2S+BETA4S)-.446031*VFR(4)+.44881*VFR(5))
      A2=R0*(1.338093*VFR(4)-4.4881*VFR(5))
      A4=R0*(5.236117*VFR(5))
      RMAX3=(A0+A2+A4)**3
      RETURN
      END
