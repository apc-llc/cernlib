*
* $Id: cqbkin.F,v 1.1.1.1 1996/03/06 10:47:00 mclareni Exp $
*
* $Log: cqbkin.F,v $
* Revision 1.1.1.1  1996/03/06 10:47:00  mclareni
* Zebra
*
*
#include "test_include/pilot.h"
      SUBROUTINE CQBKIN

C-    Initialize bank descriptors

#include "zebra/zbcd.inc"
#include "zebra/cqbkc.inc"
#include "test_include/cqbkceq.inc"
C--------------    End CDE                             --------------


      CALL VZERO (MMBANK,20)
      CALL UCTOH ('A1  A2  A3  A4  A5  A6  ', IDAV, 4, 24)
      CALL UCTOH ('B1  B2  B3  B4  ',         IDBV, 4, 16)
      CALL UCTOH ('C4  C3  C2  C1  D1  ',     IDCV, 4, 20)
      CALL UCTOH ('LOW1LOW2LOW3LOW4LOW5LOW6', IDLV, 4, 24)

      MMBKD(1) = IDCV(5)
      MMBKD(2) =  6
      MMBKD(3) =  0
      MMBKD(4) = 76

      CALL MZIOBK (MMBANK, 9, '*H *I -F')
      CALL MZIOBK (MMBKD,  9, '1I 2H 12I 35F 10D 10F / *S')
      RETURN
      END
*      ==================================================
#include "zebra/qcardl.inc"
