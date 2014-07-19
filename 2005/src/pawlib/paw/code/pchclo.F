*
* $Id: pchclo.F,v 1.1.1.1 1996/03/01 11:38:44 mclareni Exp $
*
* $Log: pchclo.F,v $
* Revision 1.1.1.1  1996/03/01 11:38:44  mclareni
* Paw
*
*
#include "paw/pilot.h"
*CMZ :  2.02/02 24/05/93  10.33.34  by  Alfred Nathaniel
*-- Author :    Fons Rademakers   25/03/93
      SUBROUTINE PCHCLO(CHAIN)
******************************************************************
*                                                                *
*  Close chain CHAIN. If CHAIN=' ' then close also chain unit.   *
*                                                                *
******************************************************************
*
#include "hbook/hcdire.inc"
#include "paw/pawlun.inc"
*
      CHARACTER*(*)   CHAIN
      CHARACTER*80    UCHAIN, CHDIR
      INTEGER         PISCHN
*
      CALL HCDIR(CHDIR, 'R')
*
      IF (CHAIN .EQ. ' ') THEN
*
*-- if current directory in a chain then CD to //PAWC
*
         IF (PISCHN(CHDIR, LENOCC(CHDIR)) .EQ. 1) THEN
            CALL HCDIR('//PAWC',' ')
         ENDIF
*
*-- disconnect the open file from the chain unit and free the chain unit
*
         CALL PCHROP(CHAIN, ' ', IER)
         IF (IER .EQ. 1) RETURN
*
         DO 20 I = 2, NCHTOP
            IF (HFNAME(I)(1:5) .EQ. 'Chain') THEN
               DO 10 J = I+1, NCHTOP
                  ICHTOP(J-1) = ICHTOP(J)
                  ICHLUN(J-1) = ICHLUN(J)
                  ICHTYP(J-1) = ICHTYP(J)
                  CHTOP(J-1)  = CHTOP(J)
                  HFNAME(J-1) = HFNAME(J)
  10           CONTINUE
               NCHTOP = NCHTOP - 1
               LUNIT(LUNCHN) = 0
               LUNCHN = 0
            ENDIF
  20     CONTINUE
      ELSE
*
*-- if current directory in chain to be closed then CD to //PAWC
*
         UCHAIN = CHAIN
         CALL CLTOU(UCHAIN)
         LC = LENOCC(UCHAIN)
         CALL HCDIR(CHDIR, 'R')
         IF (UCHAIN .EQ. CHDIR(3:LC+2)) THEN
            CALL HCDIR('//PAWC',' ')
         ENDIF
*
*-- disconnect the open file from the chain unit
*
         CALL PCHROP(CHAIN, ' ', IER)
      ENDIF
*
      END
