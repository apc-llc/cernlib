*
* $Id: encodi.F,v 1.1.1.1 1996/02/28 16:23:42 mclareni Exp $
*
* $Log: encodi.F,v $
* Revision 1.1.1.1  1996/02/28 16:23:42  mclareni
* Hepdb, cdlib, etc
*
*
      SUBROUTINE ENCODI(NUMB,MODE,STRN,LENR)
*     ---09.10.88-----------------====-====-
      CHARACTER*(*) STRN
*
      CHARACTER ENCOD*11
      DATA LCODE/11/
*
*          MODE = [-](IPL*100+MNL)
*  IPL=1 -force plus
*  LEF>0 -left shift of code( MODE<0 )
*  MNL       -minimal code length to
*        be filled by left zeros
*  MXL       -maximal code length (=len(STRN))
*
      IPL=IABS(MODE)/100
      LEF=MAX0(-MODE,0)
      MNL=MAX0(MOD(IABS(MODE),100),1)
      MXL=LEN(STRN)
*
      LSN=0
      IF(NUMB.LT.0.OR.IPL.NE.0) LSN=1
*
      WRITE(ENCOD,'(I11)') NUMB
      JJ=0
      DO 110 J=1,LCODE
      IF(ENCOD(J:J).NE.' '.AND.JJ.EQ.0) JJ=J
  110 CONTINUE
*
      IF(LSN.NE.0.AND.NUMB.GE.0) THEN
      JJ=JJ-1
      ENCOD(JJ:JJ)='+'
      ENDIF
*
      ENCOD=ENCOD(JJ:LCODE)
      LNM=LCODE-JJ+1
*
      IF(LNM.GT.MXL) THEN
C       IF(MXL.EQ.1) THEN
C         ENCOD(1:1)='*'
C       ELSE
        JJ=MAX0(MXL-1,1)
        LEX=MOD(LNM-MXL+2,10)
        ENCOD(JJ:JJ+1)='!'//CHAR(ICHAR('0')+LEX)
C       ENDIF
      LNM=MXL
      ENDIF
*
      LIZ=MAX0(MIN0(MNL-LNM,MXL-LNM),0)
      JJ=MXL-LNM-LIZ+1
      IF(LEF.GT.0) JJ=1
*
      STRN=' '
      IF(LSN.GT.0) THEN
      STRN(JJ:JJ)=ENCOD(1:1)
      JJ=JJ+1
      ENDIF
*
      DO 155 J=JJ,JJ+LIZ-1
      STRN(J:J)='0'
  155 CONTINUE
*
      JJ=JJ+LIZ
      STRN(JJ:JJ+LNM-LSN-1)=ENCOD(1+LSN:LNM)
      LENR=LNM+LIZ
*
      END
