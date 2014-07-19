*
* $Id: frbeleo.F,v 1.1.1.1 1996/01/11 14:05:21 mclareni Exp $
*
* $Log: frbeleo.F,v $
* Revision 1.1.1.1  1996/01/11 14:05:21  mclareni
* Fritiof
*
*
C********************************* END FRTORST **************************
 
C********************************* FRBELEO ******************************
 
 
      SUBROUTINE FRBELEO(IFLA,IFLB,KF)
 
C-----------------------------------------------------------------------
C     GIVING SPIN AND QUARKFLAVOUR TO THE ENDS OF THE EXCITED STRINGS.
C     FOR MESONS, THE ORDER OF THE END FLAVORS IS RANDOMLY GIVEN;
C     FOR BARYONS, where a quark-diquark combination, the diquark is
C     always assigned to IFLB.
C     IFLA AND IFLB ARE ADAPTED TO THE STANDARD KF CODES
C-----------------------------------------------------------------------
 
      PARAMETER (KSZ1=20)
      COMMON/FRPARA1/KFR(KSZ1),VFR(KSZ1)
      SAVE /FRPARA1/
      INTEGER IFRKFC
 
      SPIN=RLU(0)
      J = ABS(KF)
 
      IF(J.LT.1000) THEN
C...   identify the quark and antiquark in mesons:
      J100=  J/100
      J10 = (J-J100*100)/10
      ISGN = (-1)**MAX(J100, J10)
      IF(KF.LT.0) ISGN = -ISGN
      IF(ISGN.GT.0) J10 = -J10
      IF(ISGN.LT.0) J100 = -J100
 
        IF(SPIN.LT..5) THEN
          IFLA=J100
          IFLB=J10
        ELSE
          IFLA=J10
          IFLB=J100
        ENDIF
 
      ELSEIF(J.LT.10000) THEN
        J1000=  J/1000
      J100 = (J-J1000*1000)/100
      J10 = (J-J1000*1000-J100*100)/10
      IF(KF.LT.0) THEN
        J1000=  -J1000
      J100 = -J100
      J10 = -J10
      ENDIF
        IF(SPIN.LT.VFR(13)) THEN
          IFLA=J1000
          IFLB=IFRKFC(J100,J10,0,1.)
        ELSEIF(SPIN.LT.VFR(13)+VFR(14)) THEN
          IFLA=J10
          IFLB=IFRKFC(J1000,J100,0,1.)
        ELSEIF(SPIN.LT.VFR(13)+VFR(14)+VFR(15)) THEN
          IFLA=J100
          IFLB=IFRKFC(J1000,J10,0,0.)
        ENDIF
C...Certain Lambda-like hadrons have two lightest quarks in spin-0:
        IF(ABS(J100).LT.ABS(J10)) THEN
        IF(SPIN.LT.VFR(13)) THEN
          IFLA=J1000
          IFLB=IFRKFC(J100,J10,0,0.)
        ELSEIF(SPIN.LT.VFR(13)+VFR(14)) THEN
          IFLA=J10
          IFLB=IFRKFC(J1000,J100,0,1.)
        ELSEIF(SPIN.LT.VFR(13)+VFR(14)+VFR(15)) THEN
          IFLA=J100
          IFLB=IFRKFC(J1000,J10,0,1.)
        ENDIF
        ENDIF
      
      ELSE
 
        CALL FRMGOUT(0,0,'Unrecognized particle KF code',
     >      real(KF),0.,0.,0.,0.)
 
      ENDIF
 
 
      RETURN
      END
