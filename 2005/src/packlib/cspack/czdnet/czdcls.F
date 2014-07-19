*
* $Id: czdcls.F,v 1.1.1.1 1996/03/08 15:44:25 mclareni Exp $
*
* $Log: czdcls.F,v $
* Revision 1.1.1.1  1996/03/08 15:44:25  mclareni
* Cspack
*
*
#include "cspack/pilot.h"
      SUBROUTINE CZDCLS( ISTAT )
************************************************************************
*.
*... CZCLOS   to close connection with remote host.
*.
*.
*. Author  : B.Holl
*. Version : 1.00
*. Created : 05-Apr-1990
*. Last Mod:
*.
*. Modification Log.
*.
*.**********************************************************************
*
#include "cspack/czdecnet.inc"
#include "cspack/czsock.inc"
*
      INTEGER ISTAT
*
*.......................................................................
*
      CLOSE( UNIT=LUNDEC )
      ISKIN = 0
      ISKOUT= 0
      ISTAT = 0
      END
 
