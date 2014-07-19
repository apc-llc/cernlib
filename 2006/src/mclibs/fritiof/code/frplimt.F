*
* $Id: frplimt.F,v 1.1.1.1 1996/01/11 14:05:20 mclareni Exp $
*
* $Log: frplimt.F,v $
* Revision 1.1.1.1  1996/01/11 14:05:20  mclareni
* Fritiof
*
*
C****************************** END FUNCTION FRDPQ ***********************
 
 
C********************************* FRPLIMT ******************************
 
C....TO GIVE THE UPPER AND LOWER LIMITS FOR FINAL MOMENTA
C....  DP(2,4)  -  initial momenta
C...   DA=MINIMUM OF M_3T**2; DB=MINIMUM OF M_4T**2.
 
      SUBROUTINE FRPLIMT(DWP,DWM,DA,DB,DPLO3,DPHI3,DPLO4,DPHI4,KFEL)
 
      IMPLICIT DOUBLE PRECISION (D)
 
      KFEL=0
      DS = DWP*DWM
      DTM1 = (DS + DA - DB)
      DTM2 = (DTM1**2-4.D0*DA*DS)
            IF(DTM2.LT.0) THEN
      KFEL=3
      RETURN
            ENDIF
      DTM2 = DSQRT( DTM2 )
      DPLO3 = (DTM1 - DTM2)/(2.D0*DWP)
      DPHI3 = (DTM1 + DTM2)/(2.D0*DWP)
      DPLO4 = (DB-DA+ DWP*DPLO3)/DWM
      DPHI4 = (DB-DA+ DWP*DPHI3)/DWM
 
      RETURN
      END
