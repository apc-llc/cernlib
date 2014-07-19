*
* $Id: frevent.F,v 1.1.1.1 1996/01/11 14:05:19 mclareni Exp $
*
* $Log: frevent.F,v $
* Revision 1.1.1.1  1996/01/11 14:05:19  mclareni
* Fritiof
*
*
C**************************** END FREDITD *******************************
 
 
C**************************** FREVENT ***********************************
 
      SUBROUTINE FREVENT(FRAME,BEAM,TARGET,WIN)
 
C...This is the main routine that initializes and call FRINGEB to
C...administrate the event generation
 
      PARAMETER (KSZJ=4000,KSZ1=20,KSZ2=300)
      CHARACTER*(*) FRAME,BEAM,TARGET
      CHARACTER CFRAME*4,CBEAM*4,CTARG*4,CMEM*4
      COMMON/FRINTN0/PLI0(2,4),AOP(KSZ1),IOP(KSZ1),NFR(KSZ1)
      COMMON/FRINTN3/IDN(2,KSZ2),FMN(2,KSZ2),NUC(2,3000)
      COMMON/FRPARA1/KFR(KSZ1),VFR(KSZ1)
      COMMON/LUDAT1/MSTU(200),PARU(200),MSTJ(200),PARJ(200)
      COMMON/ARDAT1/PARA(40),MSTA(40)
 
      DIMENSION CMEM(3)
      SAVE CMEM,WINM,IFST
      SAVE /FRINTN0/,/FRINTN3/,/FRPARA1/,/LUDAT1/,/ARDAT1/
      DATA WINM,IFST /0.,0/
 
C.....check particles and frame, set initial momenta and write header
 
       CBEAM=BEAM
       CTARG=TARGET
       CFRAME=FRAME
 
       CALL FRUPCAS(CFRAME)
       CALL FRUPCAS(CBEAM)
       CALL FRUPCAS(CTARG)
 
       INEW = 0
       IF((CFRAME.NE.CMEM(1)).or.(CBEAM.NE.CMEM(2)).or.
     >    (CTARG.NE.CMEM(3)).or.(ABS(WIN-WINM).GT.0.1)) INEW=1
 
       IF(INEW.GT.0) THEN
         CMEM(1) = CFRAME
         CMEM(2) = CBEAM
         CMEM(3) = CTARG
         WINM = WIN
         CALL FRINITA(CFRAME,CBEAM,CTARG,WIN)
         IF(KSZJ.NE.MSTU(4)) THEN
         CALL FRMGOUT(0,0,'LUJETS not compatible in FRITIOF and JETSET'
     >                ,float(KSZJ),float(MSTU(4)),0.,0.,0.)
         ENDIF
      ENDIF
 
      IF(IFST.EQ.0) THEN
      DO 12 LO=1, KSZ1
12    NFR(LO)= 0
C.....     set some control parameters for Ariadne and jetset:
      MSTA(7) = MSTU(11)
      MSTA(8) = MSTU(11)
      MSTA(9) = 0
      MSTA(14) = 0
C.....extended partons made massless, for compatibility with AR3.03
      MSTA(31) = 0
 
        IF(KFR(12).EQ.1) THEN
C             Fragmentation parameters set to the Opal optimized values:
        CALL ARTUNE('OPAL')
        ELSEIF(KFR(12).GE.2) THEN
C                               DELPHI optimized values
        CALL ARTUNE('DELPHI')
        ENDIF
      CALL ARINIT('ARIADNE')
      IFST= 1
      ENDIF
 
C.....administrate one event.........................................
 
      CALL FRINGEB
 
      RETURN
      END
