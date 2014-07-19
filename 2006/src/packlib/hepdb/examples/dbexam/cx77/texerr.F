*
* $Id: texerr.F,v 1.1.1.1 1996/02/28 16:23:42 mclareni Exp $
*
* $Log: texerr.F,v $
* Revision 1.1.1.1  1996/02/28 16:23:42  mclareni
* Hepdb, cdlib, etc
*
*
      SUBROUTINE TEXERR(PRETX,LINTX,IFCL,JPK1,JPK2,POSTX,LERMS,ERRMS)
*      ---------------------------------------------------=====,=====-
*
      CHARACTER*(*) PRETX,LINTX,POSTX,ERRMS
      CHARACTER*10 COLTX
*
C      JPAK=((JTX*200+JNM)*200+JNX)*200+JMX
*
      LLTX=LEN(LINTX)
      LERM=LEN(ERRMS)
*
      ERRMS=PRETX
      LERMS=MIN0(LEN(PRETX),LERM)
      J1=MOD(JPK1/8000000,200)
      IF(J1.GT.0.AND.J1.LE.LLTX) THEN
        J1=MAX0(J1,1)
        IF(JSEARC(' ,/',LINTX(J1:J1)).GT.0) J1=MAX0(J1-1,1)
        J2=MIN0(MAX0(MOD(JPK2/200,200),1),LLTX)
        IF(LINTX(J2:J2).NE.' ') J2=MAX0(J2-1,1)
        J1=MIN0(J1,J2)
        IF(IFCL.NE.0.OR.JSEANC(LINTX(J1:J2),' ,/').LE.0) THEN
          LL=1
          COLTX(LL:LL)='['
          CALL ENCODI(J1,-1,COLTX(LL+1:),LR)
          LL=LL+LR+1
          COLTX(LL:LL)=':'
          IF(IFCL.LT.0) COLTX(LL:LL)='_'
          CALL ENCODI(J2,-1,COLTX(LL+1:),LR)
          LL=LL+LR+1
          COLTX(LL:LL)=']'
          IF(LERMS.LT.LERM) ERRMS(LERMS+1:)=COLTX
          LERMS=MIN0(LERMS+LL,LERM)
        ENDIF
        IF(IFCL.GE.0) THEN
          IF(LERMS.LT.LERM) ERRMS(LERMS+1:)=LINTX(J1:J2)
          LERMS=MAX0(LERMS+1,JLASNC(ERRMS,' '))
        ENDIF
      ELSE
        IF(LERMS.LT.LERM) ERRMS(LERMS+1:)='[eol]'
        LERMS=MIN0(LERMS+5,LERM)
      ENDIF
      IF(LERMS.LT.LERM) THEN
        ERRMS(LERMS+1:)=POSTX
        LERMS=MIN0(LERMS+LEN(POSTX),LERM)
      ENDIF
      END
