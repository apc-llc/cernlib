*
* $Id: deline.F,v 1.1.1.1 1996/03/01 11:39:28 mclareni Exp $
*
* $Log: deline.F,v $
* Revision 1.1.1.1  1996/03/01 11:39:28  mclareni
* Paw
*
*
*CMZ :  1.10/00 28/09/90  16.32.25  by  O.Couet
*-- Author : A.Risso
      SUBROUTINE DELINE(I,NWNBLN)
      PARAMETER (MGUIDL=199)
      COMMON /KCGUID/ GUID(MGUIDL)
      CHARACTER*80 GUID
      CHARACTER*80 LINECP(99)
      CHARACTER*80 LINE(99)
      EQUIVALENCE (LINE(1),GUID(1)),(LINECP(1),GUID(100))
*.______________________________________
*
      IF (NWNBLN.GT.1) THEN
         DO 10 JJ=I,NWNBLN-1
            LINE(JJ)=LINE(JJ+1)
  10     CONTINUE
         LINE(NWNBLN)=' '
      ENDIF
      NWNBLN=NWNBLN-1
      END
