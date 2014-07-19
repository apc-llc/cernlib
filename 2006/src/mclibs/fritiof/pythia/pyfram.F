*
* $Id: pyfram.F,v 1.1.1.1 1996/01/11 14:05:28 mclareni Exp $
*
* $Log: pyfram.F,v $
* Revision 1.1.1.1  1996/01/11 14:05:28  mclareni
* Fritiof
*
*
C*********************************************************************
 
      SUBROUTINE PYFRAM(IFRAME)
 
C...Performs transformations between different coordinate frames.
      COMMON/LUDAT1/MSTU(200),PARU(200),MSTJ(200),PARJ(200)
      COMMON/PYPARS/MSTP(200),PARP(200),MSTI(200),PARI(200)
      COMMON/PYINT1/MINT(400),VINT(400)
      SAVE /LUDAT1/
      SAVE /PYPARS/,/PYINT1/
 
      IF(IFRAME.LT.1.OR.IFRAME.GT.2) THEN
        WRITE(MSTU(11),5000) IFRAME,MINT(6)
        RETURN
      ENDIF
      IF(IFRAME.EQ.MINT(6)) RETURN
 
      IF(MINT(6).EQ.1) THEN
C...Transform from fixed target or user specified frame to
C...CM-frame of incoming particles.
        CALL LUROBO(0.,0.,-VINT(8),-VINT(9),-VINT(10))
        CALL LUROBO(0.,-VINT(7),0.,0.,0.)
        CALL LUROBO(-VINT(6),0.,0.,0.,0.)
        MINT(6)=2
 
      ELSE
C...Transform from particle CM-frame to fixed target or user specified
C...frame.
        CALL LUROBO(VINT(6),VINT(7),VINT(8),VINT(9),VINT(10))
        MINT(6)=1
      ENDIF
      MSTI(6)=MINT(6)
 
 5000 FORMAT(1X,'Error: illegal values in subroutine PYFRAM.',1X,
     &'No transformation performed.'/1X,'IFRAME =',1X,I5,'; MINT(6) =',
     &1X,I5)
 
      RETURN
      END
