*
* $Id: hexam7.F,v 1.2 1996/06/06 09:52:45 cernlib Exp $
*
* $Log: hexam7.F,v $
* Revision 1.2  1996/06/06 09:52:45  cernlib
* Remove #ifdef (CERNLIB_EXAMPLES) and pilot.h where not used
*
* Revision 1.1.1.1  1996/01/16 17:07:50  mclareni
* First import
*
*
#include "pilot.h"
*CMZ :  4.10/05 22/11/89  18.48.27  by  Rene Brun
*-- Author :
      SUBROUTINE HEXAM7
*.==========>
*.           Example of N-tuples.
*..=========> ( R.Brun )
      DIMENSION X(3)
      CHARACTER*8 CHTAGS(3)
      DATA CHTAGS/'   X   ','   Y   ','   Z   '/
*.___________________________________________
*.
      CALL HTITLE('EXAMPLE NO = 7')
*
*             Reopen data base
*
      CALL HROPEN(1,'HEXAM7','hexam.dat','U',1024,ISTAT)
      CALL HMDIR('NTUPLES','S')
*
      CALL HBOOK1(10,'TEST1',100,-3.,3.,0.)
      CALL HBOOK2(20,'TEST2',20,-3.,3.,20,-3.,3.,250.)
      CALL HBOOKN(30,'N-TUPLE',3,'//HEXAM7/NTUPLES',1000,CHTAGS)
*
      DO 10 I=1,10000
         CALL RANNOR(A,B)
         X(1)=A
         X(2)=B
         X(3)=A*A+B*B
         CALL HFN(30,X)
  10  CONTINUE
*
      CALL HROUT(30,ICYCLE,' ')
      CALL HPROJ1(10,30,0,0,1,999999,1)
      CALL HPROJ2(20,30,0,0,1,999999,1,2)
      CALL HPRINT(0)
*
      CALL HROUT(10,ICYCLE,' ')
      CALL HROUT(20,ICYCLE,' ')
*
      CALL HLDIR(' ',' ')
*
      CALL HREND('HEXAM7')
      CLOSE (1)
*
      END
