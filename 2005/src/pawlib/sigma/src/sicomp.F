*
* $Id: sicomp.F,v 1.1.1.1 1995/12/12 14:36:16 mclareni Exp $
*
* $Log: sicomp.F,v $
* Revision 1.1.1.1  1995/12/12 14:36:16  mclareni
* Imported sources
*
*
#include "sigma/pilot.h"
*CMZ :  1.09/00 23/07/93  11.29.21  by  Carlo E. Vandoni
*-- Author :
      LOGICAL FUNCTION SICOMP(N)
#include "sigma/sicph1.inc"
C
C     COMPARES N TO INPUT CHARACTER AND RETURNS
C     AS .TRUE. OR .FALSE. IF TRUE ADVANCES
C     INPUT STRING POINTER IC TO THE NEXT CHARACTER.
C     INPUT STRING IN ARRAY INSTR - RIGHT JUSTIFIED
C
C
      CHARACTER N*1,SINSTR*1

      SICOMP=.FALSE.
      IF( N .NE. ISGN) RETURN
      SICOMP=.TRUE.
      IC=IC+1
      ISGN = SINSTR(IC)
 999  END
