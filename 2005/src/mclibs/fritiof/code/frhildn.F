*
* $Id: frhildn.F,v 1.1.1.1 1996/01/11 14:05:19 mclareni Exp $
*
* $Log: frhildn.F,v $
* Revision 1.1.1.1  1996/01/11 14:05:19  mclareni
* Fritiof
*
*
C******************************** END FRINITA ***************************
 
 
C******************************** FRHILDN *******************************
 
      SUBROUTINE FRHILDN
 
C...This routine sets particle codes and masses. Fills common block
C...FRINTN3-IDN,FMN; randomly order the neutrons and protons.
 
      PARAMETER (KSZ1=20,KSZ2=300)
      COMMON/FRINTN0/PLI0(2,4),AOP(KSZ1),IOP(KSZ1),NFR(KSZ1)
      COMMON/FRPARA1/KFR(KSZ1),VFR(KSZ1)
      COMMON/FRINTN3/IDN(2,KSZ2),FMN(2,KSZ2),NUC(2,3000)
      SAVE /FRINTN0/,/FRPARA1/,/FRINTN3/
 
      DO 100 L=1,2
 
      IF(IOP(3+2*(L-1)).LE.1) THEN
      IDN(L,1)=IOP(6+L)
      FMN(L,1)=ULMASS(IDN(L,1))
 
      ELSE
 
      IPR=0
      INU=0
      IZ0=IOP(4+2*(L-1))
      IA0=IOP(3+2*(L-1))
      DO 30 I=1, IA0
         S=RLU(0)
         Q=FLOAT(IZ0-IPR)/(IA0-IPR-INU)
         IF (IPR.LT.IZ0.AND.(S.LT.Q.OR.INU.EQ.IA0-IZ0)) THEN
            IDN(L,I)=2212
            IPR= IPR+1
         ELSE
            IDN(L,I)=2112
            INU= INU+1
         ENDIF
         FMN(L,I)=ULMASS(IDN(L,I))
   30 CONTINUE
         IF(IPR.NE.IZ0.OR.INU.NE.IA0-IZ0) CALL FRMGOUT(0,0,
     >   ' Proton or Neutron numbers incorrect!',float(IA0),float(IZ0),
     >     float(IPR),float(INU),0.)
      ENDIF
 
  100 CONTINUE
 
      RETURN
      END
