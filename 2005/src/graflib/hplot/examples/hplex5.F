*
* $Id: hplex5.F,v 1.1.1.1 1996/01/19 10:50:12 mclareni Exp $
*
* $Log: hplex5.F,v $
* Revision 1.1.1.1  1996/01/19 10:50:12  mclareni
* Hplot
*
*
#include "hplot/pilot.h"
*CMZ :  5.13/00 01/07/92  16.13.04  by  O.Couet
*-- Author :
      SUBROUTINE HPLEX5
*.==========>
*.           Examples of HPLOT options (BARS)
*..=========>
*
      DIMENSION XALL(12),XFEM(12)
      DATA XALL/
     +100.,200.,300.,500.,400.,700.,600.,400.,500.,300.,200.,100./
      DATA XFEM/
     + 70.,220.,330.,480.,440.,650.,300.,100.,200.,300.,200.,300./
*
      CALL HTITLE('EXAMPLE NO = 5')
*
      CALL HPLSET('YGTI',0.3)
      CALL HPLSIZ(14.5,21.,' ')
      CALL HPLZON(1,2,1,' ')
      CALL HBOOK1(1,'Distribution of grades (males)',12,2.,14.,0.)
      CALL HPAK(1,XALL)
      CALL HPLOPT('BAR ',1)
      CALL HPLSET('HTYP',188.)
      CALL HPLOT(1,' ',' ',0)
      CALL HRESET(1,'(Males and Females)')
      CALL HPAK(1,XALL)
      CALL HPLSET('BARO',0.)
      CALL HPLSET('BARW',0.3)
      CALL HPLOT(1,' ',' ',0)
      CALL HPLSET('HTYP',211.)
      CALL HPLSET('BARO',0.4)
      CALL HPAK(1,XFEM)
      CALL HPLOT(1,'SAME',' ',0)
      CALL HPLOPT('NBAR',1)
      CALL HDELET(1)
      CALL HPLSET('*',0.)
*
      END
