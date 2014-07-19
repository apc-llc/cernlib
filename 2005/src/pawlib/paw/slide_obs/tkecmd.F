*
* $Id: tkecmd.F,v 1.1.1.1 1996/03/01 11:39:29 mclareni Exp $
*
* $Log: tkecmd.F,v $
* Revision 1.1.1.1  1996/03/01 11:39:29  mclareni
* Paw
*
*
*CMZ :  1.10/00 28/09/90  16.31.49  by  O.Couet
*-- Author : A.Risso
      SUBROUTINE TKECMD(I,CMD,IPOS)
      CHARACTER*20 CMD
      INTEGER CMDLNG
      PARAMETER (MGUIDL=199)
      COMMON /KCGUID/ GUID(MGUIDL)
      CHARACTER*80 GUID
      CHARACTER*80 LINECP(99)
      CHARACTER*80 LINE(99)
      EQUIVALENCE (LINE(1),GUID(1)),(LINECP(1),GUID(100))
      CHARACTER*80 BUF0
      CHARACTER*80 BUF1,BUF2
*.______________________________________
*
      CMDLNG=LENOCC(CMD)
      IF (IPOS.EQ.0) THEN
         IP=1
      ELSE
         IP=IPOS
      ENDIF
      BUF0= LINE(I)(IP:)
      IPOS= INDEX(BUF0,CMD(1:CMDLNG))
      IPOS=IP+IPOS-1
      BUF0= LINE(I)
      IF (IPOS.GT.1) THEN
         BUF1 = BUF0(1:IPOS-1)
         BUF2 = BUF0(IPOS+CMDLNG:)
         LINE(I) = BUF1(1:IPOS-1) // BUF2
      ENDIF
      IF (IPOS.EQ.1)  LINE(I) = BUF0(IPOS+CMDLNG:)
      END
