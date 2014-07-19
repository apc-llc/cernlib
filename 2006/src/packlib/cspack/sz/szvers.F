*
* $Id: szvers.F,v 1.5 1996/04/12 07:40:03 cernlib Exp $
*
* $Log: szvers.F,v $
* Revision 1.5  1996/04/12 07:40:03  cernlib
* new handling of cspack title
*
* Revision 1.4  1996/03/29 11:33:53  jamie
* qftitlch
*
* Revision 1.3  1996/03/29 10:59:27  jamie
* print 'title' more like patchy
*
* Revision 1.2  1996/03/28 11:13:33  jamie
* update idatqq/itimqq
*
* Revision 1.1.1.1  1996/03/08 15:44:26  mclareni
* Cspack
*
*
#include "cspack/pilot.h"
      SUBROUTINE SZVERS(IRC)
#include "cspack/hcmail.inc"
#include "cspack/czsock.inc"
      IDATQQ = 960329
      ITIMQQ = 1200
      IRC = 0
      CALL CFILL(' ',CHMAIL,1,80)
#include "cspack/qftitlch.inc"
      CHMAIL =
     + CspackTitleFortranString
      CALL CZPUTA('2'//CHMAIL,ISTAT)
      WRITE(CHMAIL,'(A,I7,A,I5)') '         This version created on ',
     +               IDATQQ,
     +        ' at ',ITIMQQ
      CALL CZPUTA('2'//CHMAIL,ISTAT)
      END
 
