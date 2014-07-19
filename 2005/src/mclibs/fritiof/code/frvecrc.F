*
* $Id: frvecrc.F,v 1.1.1.1 1996/01/11 14:05:21 mclareni Exp $
*
* $Log: frvecrc.F,v $
* Revision 1.1.1.1  1996/01/11 14:05:21  mclareni
* Fritiof
*
*
C********************************* END FRSETPY ***************************
 
C********************************* FRVECRC *******************************
 
C...TO SET VECTORS BETWEEN PJ(JF,), PR(JF,), AND PP(L,)
 
      SUBROUTINE FRVECRC(JF,L,IQ)
 
      COMMON/FRJETS/NJ,KJ(100,5),PJ(100,5)
      COMMON/FRCNUT/NR,KR(10,5),PR(10,5),NR0
      COMMON/FRPICKJ/NH,KP(100,5),PP(100,5)
      SAVE /FRJETS/,/FRCNUT/,/FRPICKJ/
 
      IF(IQ.EQ.1) THEN
       DO 11 J=1,5
       KJ(JF,J) = KP(L,J)
11     PJ(JF,J) = PP(L,J)
      ELSEIF(IQ.EQ.-1) THEN
       DO 21 J=1,5
      KP(L,J) = KJ(JF,J)
21    PP(L,J) = PJ(JF,J)
      ELSEIF(IQ.EQ.2) THEN
       DO 31 J=1,5
       KR(JF,J) = KP(L,J)
31     PR(JF,J) = PP(L,J)
      ELSEIF(IQ.EQ.-2) THEN
       DO 41 J=1,5
      KP(L,J) = KR(JF,J)
41    PP(L,J) = PR(JF,J)
      ENDIF
 
      RETURN
      END
