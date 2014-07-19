*
* $Id: sinstr.F,v 1.1.1.1 1995/12/12 14:36:19 mclareni Exp $
*
* $Log: sinstr.F,v $
* Revision 1.1.1.1  1995/12/12 14:36:19  mclareni
* Imported sources
*
*
#include "sigma/pilot.h"
*CMZ :  1.09/04 26/10/93  09.49.19  by  Carlo E. Vandoni
*-- Author :
         CHARACTER FUNCTION SINSTR(NN)
#include "sigma/sicsig.inc"
#include "sigma/sigkq.inc"
#include "sigma/sicst4.inc"
      CALL SITRAX('SINSTR    ')
C---  FINAL CODING AS COMMENT
      SINSTR=KOLTEX(LINE)(NN:NN)
      IF (NN .GT. LENSTR) SINSTR=KQDOLL
 999  END
