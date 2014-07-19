*
* $Id: hplpto.F,v 1.2 2001/12/07 15:19:56 couet Exp $
*
* $Log: hplpto.F,v $
* Revision 1.2  2001/12/07 15:19:56  couet
* - use higz include files from the higz include file directory.
*   option Z is now working in hplaer.F
*   option Z has been implemented in hplsym.F
*
* Revision 1.1.1.1  1996/01/19 10:50:11  mclareni
* Hplot
*
*
#include "hplot/pilot.h"
*CMZ :  5.12/00 26/02/92  16.46.39  by  O.Couet
*-- Author :
      SUBROUTINE HPLPTO(TEXTIN,TEXTUT)
*.==========>
*.            Request string (eg. <CR>)
*.            Subroutine to allow users to pause with a prompt and
*             receive a reply in interactive systems.
*..=========>
#include "higz/himeta.inc"
      CHARACTER*(*) TEXTIN,TEXTUT
*._____________________________
*
      IF(.NOT.TEKACT)RETURN
      IWKID=1
      ISTDNR=1
      TEXTUT=' '
      CALL IRQST(IWKID,ISTDNR,ISTAT,NCH,TEXTUT)
      END

