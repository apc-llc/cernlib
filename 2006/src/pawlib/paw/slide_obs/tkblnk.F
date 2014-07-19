*
* $Id: tkblnk.F,v 1.1.1.1 1996/03/01 11:39:29 mclareni Exp $
*
* $Log: tkblnk.F,v $
* Revision 1.1.1.1  1996/03/01 11:39:29  mclareni
* Paw
*
*
*CMZ :  1.10/00 28/09/90  16.32.11  by  O.Couet
*-- Author : A.Risso
      SUBROUTINE TKBLNK(I,J)
      PARAMETER (MGUIDL=199)
      COMMON /KCGUID/ GUID(MGUIDL)
      CHARACTER*80 GUID
      CHARACTER*80 LINECP(99)
      CHARACTER*80 LINE(99)
      EQUIVALENCE (LINE(1),GUID(1)),(LINECP(1),GUID(100))
      COMMON /SLIDE5/CHBUF
      CHARACTER*1 CHBUF
*.______________________________________
*
   10 CHBUF = LINE(I)(J:J)
      IF (CHBUF.EQ.' '.AND.J.GT.1) THEN
         J = J - 1
         GO TO 10
      ENDIF
      END
