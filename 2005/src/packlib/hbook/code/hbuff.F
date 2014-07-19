*
* $Id: hbuff.F,v 1.1.1.1 1996/01/16 17:07:33 mclareni Exp $
*
* $Log: hbuff.F,v $
* Revision 1.1.1.1  1996/01/16 17:07:33  mclareni
* First import
*
*
#include "hbook/pilot.h"
*CMZ :  4.10/05 07/03/89  11.05.36  by  Rene Brun
*-- Author :
      SUBROUTINE HBUFF(IERR,ILABEL,ID)
*.==========>
*.           This routine prints error messages on file LERR
*.
*.              111   histogram not referenced
*.              212   number of channels in x .gt.nclim
*.              213   number of channels in y .gt.nclim
*.              214   abscissa of origin .gt.abscissa of extremity
*.              323   command already executed
*.              324   this command must appear before filling
*.              112   ID is already referenced
*.              321   command not available for a 1-dim
*.              322   ...........                 2-dim
*.              811   buffer area too short
*.              711   operation between hist with different specifica
*.              541   comparison .........................
*.              542   normalisation factor=0.
*.              543   bad scale factor
*.              721   degree of polynomial .le.0  in hfitpo
*.              722   number of channels .lt.degree of polynomial
*.              311   first location of buffer set .le.o by histgo
*.              211   number of channels in x or y .le.o
*.              113   ID=0 illegal booking identifier
*.              215   packing not permitted with automatic binning
*.              712   operand histograms must resid in memory
*.              114   illegal parameter
*.              115   this band or slice number does not exist
*.              325   option not existing
*..=========> ( R.Brun )
      COMMON/HFORM/IA(127),IDU
      DIMENSION ILABEL(1)
*.___________________________________________
      CALL HFORMA(5)
      CALL VBLANK(IA,127)
      CALL UCTOH('****** HBOOK ERROR NO =',IA,1,23)
      CALL HBCDI(IERR,3,IA(24))
      CALL UCTOH('   CALLED FROM ',IA(27),1,15)
      CALL UBLOW(ILABEL,IA(42),6)
      IF(ID.NE.0)CALL UCTOH('ID =',IA(52),1,4)
      CALL HBCDI(ID,8,IA(56))
      CALL HFORMA(4)
      END
