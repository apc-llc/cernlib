*
* $Id: jsearx.F,v 1.1.1.1 1996/02/28 16:23:42 mclareni Exp $
*
* $Log: jsearx.F,v $
* Revision 1.1.1.1  1996/02/28 16:23:42  mclareni
* Hepdb, cdlib, etc
*
*
      FUNCTION JSEARC(STRN,CHRS)
*     ---21.01.89---------------
      CHARACTER*(*) STRN,CHRS
*
      JSEARC=0
      DO 111 J=1,LEN(STRN)
      IF(INDEX(CHRS,STRN(J:J)).GT.0) THEN
        JSEARC=J
        RETURN
      ENDIF
  111 CONTINUE
      END
*
*
      FUNCTION JSEANC(STRN,CHRS)
*     ---21.01.89---------------
      CHARACTER*(*) STRN,CHRS
*
      JSEANC=0
      DO 111 J=1,LEN(STRN)
      IF(INDEX(CHRS,STRN(J:J)).EQ.0) THEN
        JSEANC=J
        RETURN
      ENDIF
  111 CONTINUE
      END
*
*
      FUNCTION JSEARI(STRN,CHRS)
*     ---21.01.89---------------
      CHARACTER*(*) STRN,CHRS
      CHARACTER CH
*
      NCHR=LEN(CHRS)
      JSEARI=0
      IF(NCHR.LT.2) RETURN
*
      DO 112 J=1,LEN(STRN)
      CH=STRN(J:J)
      DO 111 K=1,NCHR-1,2
       IF(CH.GE.CHRS(K:K).AND.CH.LE.CHRS(K+1:K+1)) THEN
          JSEARI=J
          RETURN
       ENDIF
  111      CONTINUE
  112 CONTINUE
      END
*
*
      FUNCTION JSEANI(STRN,CHRS)
*     ---21.01.89---------------
      CHARACTER*(*) STRN,CHRS
      CHARACTER CH
*
      NCHR=LEN(CHRS)
      JSEANI=0
      IF(NCHR.LT.2) RETURN
      DO 112 J=1,LEN(STRN)
      CH=STRN(J:J)
      IFEQ=0
      DO 111 K=1,NCHR-1,2
       IF(CH.GE.CHRS(K:K).AND.CH.LE.CHRS(K+1:K+1)) IFEQ=1
  111      CONTINUE
      IF(IFEQ.EQ.0) THEN
        JSEANI=J
        RETURN
      ENDIF
  112 CONTINUE
      END
