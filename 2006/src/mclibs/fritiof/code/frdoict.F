*
* $Id: frdoict.F,v 1.1.1.1 1996/01/11 14:05:22 mclareni Exp $
*
* $Log: frdoict.F,v $
* Revision 1.1.1.1  1996/01/11 14:05:22  mclareni
* Fritiof
*
*
C********************************* END FRMGOUT *************************
 
C********************************* FRDOICT *******************************
 
C.... TO MANAGE COUNTING ......................................
C.... 1 IS ADDED TO ICT(I) IF the argument is > 0.
C.... The record in ICT is cleared if I <= -1; and
C.... (This routine is not listed in the manual)
 
      SUBROUTINE FRDOICT(I)
 
      COMMON/FRCONT2/ICT(10),ICTT(10)
      SAVE /FRCONT2/
 
      IF(I.LE.-1) THEN
      DO 10 L=1, 10
10    ICT(L) = 0
      RETURN
      ENDIF
 
      IF(I.GT.0) THEN
      ICT(I) = ICT(I) + 1
      ICTT(I) = ICTT(I) + 1
      ENDIF
 
      RETURN
      END
