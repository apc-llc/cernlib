*
* $Id: zftpwd.F,v 1.1.1.1 1996/03/08 15:44:19 mclareni Exp $
*
* $Log: zftpwd.F,v $
* Revision 1.1.1.1  1996/03/08 15:44:19  mclareni
* Cspack
*
*
#include "cspack/pilot.h"
      SUBROUTINE ZFTPWD
#include "cspack/czsock.inc"
#include "cspack/hcmail.inc"
#include "cspack/quest.inc"
*
#include "cspack/czopen.inc"
      CHMAIL = 'XZIO :PWD '
      CALL CZPUTA(CHMAIL,ISTAT)
#include "cspack/czmess.inc"
  99  END
 
