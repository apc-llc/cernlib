*
* $Id: igiwin.F,v 1.1.1.1 1996/02/14 13:10:35 mclareni Exp $
*
* $Log: igiwin.F,v $
* Revision 1.1.1.1  1996/02/14 13:10:35  mclareni
* Higz
*
*
#include "higz/pilot.h"
*CMZ :  1.09/00 31/10/89  14.50.24  by  O.Couet
*-- Author : O.Couet
      FUNCTION IGIWIN(IWKID)
*.===========>
*.   IG Inquire Workstation INdex
*..==========>
#include "higz/hiatt.inc"
      IGIWIN=0
      DO 10 I=1,INOPWK
         IF(IWKID.EQ.ILOPWK(I))THEN
            IGIWIN=I
            RETURN
         ENDIF
   10 CONTINUE
*
      END
