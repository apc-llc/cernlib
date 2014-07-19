*
* $Id: subspc.F,v 1.1.1.1 1996/03/01 11:39:29 mclareni Exp $
*
* $Log: subspc.F,v $
* Revision 1.1.1.1  1996/03/01 11:39:29  mclareni
* Paw
*
*
*CMZ :  1.10/00 28/09/90  16.35.55  by  O.Couet
*-- Author : A.Risso
      SUBROUTINE SUBSPC(I,IPO)
      PARAMETER (MGUIDL=199)
      COMMON /KCGUID/ GUID(MGUIDL)
      CHARACTER*80 GUID
      CHARACTER*80 LINECP(99)
      CHARACTER*80 LINE(99)
      EQUIVALENCE (LINE(1),GUID(1)),(LINECP(1),GUID(100))
      CHARACTER*80 LNBUF1,LNBUF2
      CHARACTER*1 CHBUF
*.______________________________________
*
      JJ=IPO
      IF (IPO.EQ.1) THEN
         LNBUF1=LINE(I)
   10    CHBUF=LNBUF1(JJ:JJ)
         IF (CHBUF.EQ.' '.AND.JJ.LT.80) THEN
            LINE(I)=LNBUF1(JJ+1:)
            JJ=JJ+1
            GO TO 10
         ENDIF
      ELSE
         LNBUF1=LINE(I)(IPO:)
         LNBUF2=LINE(I)(1:IPO-1)
   20    CHBUF=LINE(I)(JJ:JJ)
         IF (CHBUF.EQ.' '.AND.JJ.LT.80) THEN
            JJ=JJ+1
            GO TO 20
         ENDIF
         IPO=JJ
      ENDIF
      END
