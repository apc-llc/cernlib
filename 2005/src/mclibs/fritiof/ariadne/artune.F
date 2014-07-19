*
* $Id: artune.F,v 1.1.1.1 1996/01/11 14:05:19 mclareni Exp $
*
* $Log: artune.F,v $
* Revision 1.1.1.1  1996/01/11 14:05:19  mclareni
* Fritiof
*
*
C***********************************************************************
C $Id: artune.F,v 1.1.1.1 1996/01/11 14:05:19 mclareni Exp $
 
      SUBROUTINE ARTUNE(SET)
 
C...ARiadne subroutine TUNE
 
C...Sets parameters and switches in Ariadne and other programs which
C...Ariadne runs with.
 
 
      PARAMETER(MAXDIP=500,MAXPAR=500,MAXSTR=100)
 
      IMPLICIT DOUBLE PRECISION (D)
      IMPLICIT DOUBLE PRECISION (B)
      IMPLICIT LOGICAL (Q)
 
      COMMON /ARDAT1/ PARA(40),MSTA(40)
      SAVE /ARDAT1/
 
      COMMON /LUDAT1/ MSTU(200),PARU(200),MSTJ(200),PARJ(200)
      SAVE /LUDAT1/
 
      COMMON /LEPTOU/ CUT(14),LST(40),PARL(30),X,Y,W2,XQ2,U
      SAVE /LEPTOU/
      CHARACTER SET*(*)
 
 
      IF(SET.EQ.'DELPHI') THEN
        PARA(1)=0.22
        PARA(3)=0.6
        PARA(5)=0.6
        MSTJ(11)=1
        PARJ(41)=0.23
        PARJ(42)=0.34
        PARJ(21)=0.405
        WRITE(MSTA(7),1000) SET
      ELSEIF(SET.EQ.'OPAL') THEN
        PARA(1)=0.20
        PARA(3)=1.0
        PARA(5)=1.0
        PARJ(41)=0.18
        PARJ(42)=0.34
        PARJ(21)=0.37
        WRITE(MSTA(7),1000) SET
      ELSE
        WRITE(MSTA(7),1010) SET
      ENDIF
 
 1000 FORMAT('Parameters and switches initialized using the "',A,
     $     '" tuning set')
 1010 FORMAT('Tuning set "',A,'" does not exist. Parameters and',
     $     ' switches retains their default value')
 
      RETURN
 
C**** END OF ARTUNE ****************************************************
      END
