*
* $Id: izdatt.F,v 1.1.1.1 1996/02/14 13:11:09 mclareni Exp $
*
* $Log: izdatt.F,v $
* Revision 1.1.1.1  1996/02/14 13:11:09  mclareni
* Higz
*
*
#include "higz/pilot.h"
#if defined(CERNLIB_ZEBRA)
*CMZ :  1.06/05 17/02/89  16.44.54  by  O.Couet
*-- Author :
      SUBROUTINE IZDATT(IADR1,IADR2)
*.===========>
*.
*.   This routine Decodes the attributes in the from the adress IADR1
*. to the adress IADR2 in the bank LNDECO.
*.
*. _Input parameters:
*.
*.  INTEGER IADR1 : Starting adress.
*.  INTEGER IADR2 : End adress.
*.
*..==========> (O.Couet)
#include "higz/hipaw.inc"
#include "higz/hiatnb.inc"
#include "higz/hiatt.inc"
#include "higz/hiatnm.inc"
*.______________________________________
*
      IF(IADR1.LE.9)THEN
         IREPTR=9
      ELSE
         IREPTR=IADR1-1
      ENDIF
      IDIAA=IZGADR(LNDECO,8)
      IDRAA=IZGADR(LNDECO,9)
      INBIAT=IZGCOD(LNDECO,8)
      INBRAT=IZGCOD(LNDECO,9)
      IF(IADR1.GT.10)GOTO 1
*
*              Set the default attributes
*
      DO 3 I=1,INBIAT
         CALL IGSET(CHIATT(I),FLOAT(IQ(LIDECO+IDIAA+I-1)))
 3    CONTINUE
      DO 4 I=1,INBRAT
         CALL IGSET(CHRATT(I),Q(LFDECO+IDRAA+I-1))
 4    CONTINUE
*
*              Decode the LN bank
*
 1    IREPTR=IREPTR+1
      IF(IREPTR.EQ.IQ(LNDECO+4))RETURN
      IF(IREPTR.EQ.IADR2+1)RETURN
      IADRI=ABS(IZGADR(LNDECO,IREPTR))
      ICODE=IZGCOD(LNDECO,IREPTR)
*
      GOTO (2,2,2,2,2
     +,     2,2,2,2,2,2,2,2,2,2,2,2,2,2,480,490
     +,     2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2
     +,     5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5
     +,     8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8
     +      ),ABS(ICODE)
*
 2    CONTINUE
      GOTO 1
*
*              Set color representation
*
 480  IADRF=IQ(LIDECO+IADRI)
      CALL ISCR(IDID
     +,         IQ(LIDECO+IADRI+1)
     +,         Q(LFDECO+IADRF)
     +,         Q(LFDECO+IADRF+1)
     +,         Q(LFDECO+IADRF+2))
      GOTO 1
*
*              Set clipping indicator
*
 490  CALL ISCLIP(IADRI)
      GOTO 1
*
*              Real attributes
*
  5   CALL IGSET(CHRATT(ICODE-49),Q(LFDECO+IADRI))
      GOTO 1
*
*              Integer attributes
*
  8   CALL IGSET(CHIATT(ICODE-79),FLOAT(IADRI-100000))
      GOTO 1
*
      END
#endif
