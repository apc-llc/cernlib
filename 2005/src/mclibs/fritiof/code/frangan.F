*
* $Id: frangan.F,v 1.1.1.1 1996/01/11 14:05:19 mclareni Exp $
*
* $Log: frangan.F,v $
* Revision 1.1.1.1  1996/01/11 14:05:19  mclareni
* Fritiof
*
*
C******************************** END FRINGEB  ***************************
 
 
C*************************************************************************
C*************************************************************************
C                                                                        *
C      This is the routine package for nuclear geometry                  *
C      Input FRINTN0, output FRINTN3_NUC                                  *
C                                                                        *
C*************************************************************************
 
 
C********************************* FRANGAN ****************************
 
      SUBROUTINE FRANGAN
 
      PARAMETER (KSZ1=20,KSZ2=300)
      COMMON/FRGEOMC/NFLG,NUMROP,NUMROT,NUMREP
      COMMON/FRPARA1/KFR(KSZ1),VFR(KSZ1)
      COMMON/FRINTN0/PLI0(2,4),AOP(KSZ1),IOP(KSZ1),NFR(KSZ1)
      COMMON/FRINTN3/IDN(2,KSZ2),FMN(2,KSZ2),NUC(2,3000)
      SAVE /FRGEOMC/,/FRPARA1/,/FRINTN0/,/FRINTN3/
 
      IF (IOP(3).LE.1.and.IOP(5).LE.1) THEN
       CALL FRPPCOL
 
      ELSE
 
      CALL FROVLAP
            IF(IOP(3).LE.1.or.IOP(5).LE.1) then
      CALL FRPACOL
            ELSE
      CALL FRAACOL
            ENDIF
 
      ENDIF
 
      RETURN
      END
