*
* $Id: fpsetufcn.F,v 1.4 1996/06/04 15:53:50 couet Exp $
*
* $Log: fpsetufcn.F,v $
* Revision 1.4  1996/06/04 15:53:50  couet
* - Mods to handel properly the FILECASE KEEP
*
* Revision 1.3  1996/05/31 16:24:39  couet
* - Cleaned up useless commented lines.
*
* Revision 1.2  1996/05/31 16:07:02  couet
* - pilot.h was missing
*
* Revision 1.1.1.1  1996/03/01 11:39:10  mclareni
* Paw
*
*
#include "paw/pilot.h"
*CMZ :  2.07/00 09/04/95  23.28.21  by  Gregory Kozlovsky
*-- Author :    Gregory Kozlovsky   09/04/95
      SUBROUTINE FPSETUFCN(FEXPR,UFCN,NP,IERFLG)
      CHARACTER*(*) FEXPR
      INTEGER UFCN
      INTEGER NP,IERFLG
*.===========> Author: G. Kozlovsky, 1994
*.
*. Set user function, return its pointer and number of parameters
*. if error, return IERFLG=1, 0 otherwise
*.
*..==========>
*
#include "hbook/hcbits.inc"
#include "paw/pcfitf.inc"
#include "paw/pcbuff.inc"
*
      COMMON/PAWPAR/PAR(200)
*
      DIMENSION VALP(50),SIGPAR(50)
      DIMENSION COV(1000),PMIN(50),PMAX(50),STEP(50)
      EQUIVALENCE (PAWBUF(1),VALP(1)),(PAWBUF(101),SIGPAR(1))
      EQUIVALENCE (PAWBUF(501),COV(1)),(PAWBUF(1501),PMIN(1))
      EQUIVALENCE (PAWBUF(1601),PMAX(1)),(PAWBUF(1701),STEP(1))
      EXTERNAL PAWFUN,PAWFUY,PAWSIM
*
      IERFLG = 0
*
CCC      CALL CLTOU(FEXPR)
*
* Parse expression and return NP (param. number) and NOPER oper. numb.
      CALL PAFITF(FEXPR,NP)
* If NOPER==0 it is either a COMIS function or a file name
      IF(NOPER.EQ.0)THEN
         NFE = LENOCC(FEXPR)
         CALL PAWFCA(FEXPR,NFE,JAD,1)
         IF(JAD.EQ.0)THEN
            IERFLG = 1
            RETURN
         ENDIF
      ELSE
         JAD=0
      ENDIF

      IF(JAD.NE.0)THEN
         IF(I1.NE.0)THEN
            UFCN = JUMPAD(PAWFUN)
         ELSE
            UFCN = JUMPAD(PAWFUY)
         ENDIF
      ELSE
         UFCN = JUMPAD(PAWSIM)
      ENDIF

      END
