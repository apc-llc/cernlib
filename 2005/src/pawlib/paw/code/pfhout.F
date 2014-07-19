*
* $Id: pfhout.F,v 1.1.1.1 1996/03/01 11:38:45 mclareni Exp $
*
* $Log: pfhout.F,v $
* Revision 1.1.1.1  1996/03/01 11:38:45  mclareni
* Paw
*
*
#include "paw/pilot.h"
#if defined(CERNLIB_CZ)
*CMZ :  2.03/07 22/08/93  11.47.32  by  Fons Rademakers
*-- Author :    Alfred Nathaniel   13/04/93
      SUBROUTINE PFHOUT(IDH,ISTAT)
*
* Export a histogram, either from client to server or from server to client.
*
      CHARACTER CHMAIL*80

      WRITE(CHMAIL,'(A,I12)') 'HINPF ',IDH
      CALL CZPUTA(CHMAIL,ISTAT)
      CALL HOUTPF(IDH)

      END
#endif
