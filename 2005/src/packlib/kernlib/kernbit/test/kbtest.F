*
* $Id: kbtest.F,v 1.2 1996/12/12 13:50:34 cernlib Exp $
*
* $Log: kbtest.F,v $
* Revision 1.2  1996/12/12 13:50:34  cernlib
* Add test for caldat z009
*
* Revision 1.1.1.1  1996/02/15 17:47:55  mclareni
* Kernlib
*
*
#include "kernbit/pilot.h"
      CALL M433T
      CALL M442T
      CALL M443T
#if defined(CERNLIB_IBMVM)
      CALL M444T
#endif
      CALL Z009T
      CALL Z100T
      CALL Z265T
#if (defined(CERNLIB_UNIX))&&(!defined(CERNLIB_OS9))
      CALL Z268T
#endif
#if defined(CERNLIB_IBMVM)
      CALL Z305T
      CALL Z307T
#endif
#if defined(CERNLIB_IBMVM)||defined(CERNLIB_VAXVMS)
      CALL Z313T
#endif
      END
