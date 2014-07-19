*
* $Id: frsetpy.F,v 1.1.1.1 1996/01/11 14:05:21 mclareni Exp $
*
* $Log: frsetpy.F,v $
* Revision 1.1.1.1  1996/01/11 14:05:21  mclareni
* Fritiof
*
*
C********************************* END FRHARDP ***************************
 
 
C********************************* FRSETPY *******************************
 
      SUBROUTINE FRSETPY(IQ)
 
C.......TO SET PT_MIN AND CERTAIN SWITCHES FOR PYTHIA................
C....... IQ = -1, SET ON ONLY QCD PROCESSES;
C........   = +1, SET ON QCD + LOW_PT PROCESSES
C:
 
      PARAMETER (KSZ1=20)
      COMMON/FRINTN0/PLI0(2,4),AOP(KSZ1),IOP(KSZ1),NFR(KSZ1)
      COMMON/FRPARA1/KFR(KSZ1),VFR(KSZ1)
      COMMON/PYSUBS/MSEL,MSUB(200),KFIN(2,-40:40),CKIN(200)
      COMMON/PYPARS/MSTP(200),PARP(200),MSTI(200),PARI(200)
      SAVE /FRINTN0/,/FRPARA1/,/PYSUBS/,/PYPARS/
 
      MSTP(61) = 0
      MSTP(63) = 0
      MSTP(65) = 0
      MSTP(71) = 0
      MSTP(91) = 0
      MSTP(111) = 0
      MSTP(122) = 0
      MSTP(31) =5
 
C....since sea quarks are treated as gluons in current model, heavy
C....quarks are not included...........
      MSUB(81) = 0
      MSUB(82) = 0
      MSUB(83) = 0
 
      IF(IQ.EQ.-1) THEN
      MSEL = 0
      MSUB(11) = 1
      MSUB(12) = 1
      MSUB(13) = 1
      MSUB(28) = 1
      MSUB(53) = 1
      MSUB(68) = 1
      MSUB(92) = 0
      MSUB(95) = 0
      CKIN(3) = VFR(12)
      CKIN(5) = VFR(12)
      CKIN(6) = VFR(12)
      ELSEIF(IQ.EQ.1) THEN
Cc      MSUB(92) = 1
Cc      MSUB(95) = 1
      MSEL = 1
 
      ENDIF
 
 
C.........multiple interactions....................
      PARP(81) = VFR(12)
 
      RETURN
      END
