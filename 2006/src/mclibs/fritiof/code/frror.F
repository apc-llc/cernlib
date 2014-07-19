*
* $Id: frror.F,v 1.1.1.1 1996/01/11 14:05:20 mclareni Exp $
*
* $Log: frror.F,v $
* Revision 1.1.1.1  1996/01/11 14:05:20  mclareni
* Fritiof
*
*
C********************************* END FRSEARC ***************************
 
       REAL FUNCTION FRROR(L,R)
 
C......Gives value of nuclear density.  L=1,2 for projectile and target
C......the dinsity FRROR is given by
C...... for A<,=16, harmonic oscilator potential shell model density;
C...... for A> 16, (Wood-Saxon) Fermi distribution.
 
      PARAMETER (KSZ1=20)
      COMMON/FRPARA1/KFR(KSZ1),VFR(KSZ1)
      CHARACTER*4 PACD
      COMMON/FRCODES/IPT(2),PACD(27),NNUC(27),NPROT(27),KCD(27)
     >           ,RO1(27,2),EXMA(9,2)
      SAVE /FRPARA1/,/FRCODES/
 
      A = NNUC(IPT(L))
 
      IF(A.LE.16) THEN
      RCH = RO1(IPT(L),1)
      D2 = (2.5-4./A)**(-1) * (RCH**2 - 0.81**2)
C.......(Proton radius 0.81 was subtracted off from the charge radius.)
      FRROR = (1.+((A-4.)/6.)* R**2/D2) * FRREX(-R**2/D2)
 
      ELSE
C                               (Wood-Saxon) distribution
 
       R0 = RO1(IPT(L),1)
       C =  RO1(IPT(L),2)
        AP=A**(1./3.)
        IF(R0.LE.0.) r0=1.16*(1.-1.16/ap**2)
        IF(C.LE.0.) C =0.5
       ARG = (R - R0*AP)/C
       FRROR=( 1.+FRREX(ARG) )**(-1)
 
       ENDIF
 
       FRROR = r**2 * FRROR
 
       return
       end
