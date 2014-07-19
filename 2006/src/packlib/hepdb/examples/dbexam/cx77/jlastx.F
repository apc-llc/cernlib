*
* $Id: jlastx.F,v 1.1.1.1 1996/02/28 16:23:42 mclareni Exp $
*
* $Log: jlastx.F,v $
* Revision 1.1.1.1  1996/02/28 16:23:42  mclareni
* Hepdb, cdlib, etc
*
*
      FUNCTION JLASTC(STRN,CHRS)
*     ---06.02.89---------------
      CHARACTER*(*) STRN,CHRS
*
      JLASTC=0
      DO 111 J=LEN(STRN),1,-1
      IF(INDEX(CHRS,STRN(J:J)).NE.0) THEN
        JLASTC=J
        RETURN
      ENDIF
  111 CONTINUE
      END
*
*
      FUNCTION JLASNC(STRN,CHRS)
*     ---06.02.89---------------
      CHARACTER*(*) STRN,CHRS
*
      JLASNC=0
      DO 111 J=LEN(STRN),1,-1
      IF(INDEX(CHRS,STRN(J:J)).EQ.0) THEN
        JLASNC=J
        RETURN
      ENDIF
  111 CONTINUE
      END
*
*
      FUNCTION JLASTI(STRN,CHRS)
*     ---06.02.89---------------
      CHARACTER*(*) STRN,CHRS
      CHARACTER*1 C
*
      JLASTI=0
      LCH0=LEN(CHRS)-1
      IF(LCH0.LT.1) RETURN
*
      DO 111 J=LEN(STRN),1,-1
      C=STRN(J:J)
      DO 112 K=1,LCH0,2
        IF(C.GE.CHRS(K:K).AND.C.LE.CHRS(K+1:K+1)) THEN
          JLASTI=J
          RETURN
        ENDIF
  112      CONTINUE
  111 CONTINUE
      END
*
*
      FUNCTION JLASNI(STRN,CHRS)
*     ---06.02.89---------------
      CHARACTER*(*) STRN,CHRS
      CHARACTER*1 C
*
      JLASNI=0
      LCH0=LEN(CHRS)-1
      IF(LCH0.LT.1) RETURN
*
      DO 111 J=LEN(STRN),1,-1
      C=STRN(J:J)
      IFYES=0
      DO 112 K=1,LCH0,2
        IF(C.GE.CHRS(K:K).AND.C.LE.CHRS(K+1:K+1)) IFYES=1
  112      CONTINUE
      IF(IFYES.EQ.0) THEN
        JLASNI=J
        RETURN
      ENDIF
  111 CONTINUE
      END
 
