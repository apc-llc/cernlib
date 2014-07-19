*
* $Id: m442t.F,v 1.1.1.1 1996/02/15 17:47:55 mclareni Exp $
*
* $Log: m442t.F,v $
* Revision 1.1.1.1  1996/02/15 17:47:55  mclareni
* Kernlib
*
*
#include "kernbit/pilot.h"
      SUBROUTINE M442T
*
*     Test of FORTRAN emulation of VM NAMEFIND command
*
      CHARACTER*20  CHIN(2,10)
      CHARACTER*255 CHOUT(2,10)
*
      CALL KBHDR('M442 NAMEFD')
*
*     Get the list of FATSERVERS...
*
      CHIN(1,1)  = ':nick'
      CHIN(2,1)  = 'FATSERVERS'

      CHOUT(1,1) = ':list'

      NIN        = 1
      NOUT       = 1
      LUN        = 1
      CALL NAMEFD(LUN,'fatmen.names',CHIN,NIN,CHOUT,NOUT,IRC)
*
      PRINT 1000,(CHOUT(K,1),K=1,2)
1000  FORMAT(6X,'Extraction from fatmen.names: ',2A20)
      RETURN
      END
