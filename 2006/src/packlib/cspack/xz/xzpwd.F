*
* $Id: xzpwd.F,v 1.1.1.1 1996/03/08 15:44:32 mclareni Exp $
*
* $Log: xzpwd.F,v $
* Revision 1.1.1.1  1996/03/08 15:44:32  mclareni
* Cspack
*
*
#include "cspack/pilot.h"
      SUBROUTINE XZPWD(PATH,IRC)
      CHARACTER*(*) PATH
#include "cspack/czsock.inc"
#include "cspack/hcmail.inc"
#include "cspack/quest.inc"
*
#include "cspack/czopen.inc"
      IRC    = 0
      CHMAIL = 'XZIO :PWD '
      CALL CZPUTA(CHMAIL,ISTAT)
      CALL CZGETA(CHMAIL,ISTAT)
      LPATH  = LENOCC(CHMAIL)
      IF(INDEX(CHMAIL,'Current working directory is').NE.0) THEN
         JPATH = 31
         PATH = CHMAIL(JPATH:LPATH)
      ELSE
         IRC = -1
      ENDIF
#include "cspack/czmess.inc"
   99 END
