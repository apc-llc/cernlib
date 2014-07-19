*
* $Id: dzezer.F,v 1.2 1996/05/09 14:40:55 cernlib Exp $
*
* $Log: dzezer.F,v $
* Revision 1.2  1996/05/09 14:40:55  cernlib
* correct path for include file
*
* Revision 1.1.1.1  1996/03/04 16:13:20  mclareni
* Dzdoc/Zebpack
*
*
#include "dzdoc/pilot.h"
      SUBROUTINE DZEZER
#include "dzeditkeep.inc"
#include "dzdraw/dzdprm.inc"
      INTEGER INIFLG
      SAVE  INIFLG
      DATA  INIFLG/0/
*
      IF(INIFLG.NE.0)GOTO 999
      INIFLG=1
      LUNOUT=LUPOUT
      LUNMET=0
      LUNSGM=0
      LUNLST=6
      LUNCOM=0
      LUNLAS=0
      LUNRZ=0
      LUNCAR=0
      LUNGKS=0
      LUNLIS=0
      LUNSCR=0
      LUNPOS=0
      LUNPIC=0
      LUNEXE=0
      LUNSC1=0
      LUNPLO=0
      IWKID = 1
      ICONID = 6
      IWMETA = 2
      INTWST = 4
      ITYMET = 4
*      ITYPLO = 4
      ILOCNR = 1
      ITIMCH = 0
      IDATCH = 0
      IBCCOL=7
      IBDCOL=3
*      IHATCH = -5
      LISTAC = .FALSE.
      LISTSG = .FALSE.
      LISTTO = .FALSE.
      CHDZED = ' '
*      LOGLV = -2
      ISGMOP = 0
      ILSTOP = 0
      ISCROP = 0
      IPOSTF = 0
      CHLIST =' '
      CHBSBK= '    '
      CHBSUP= '****'
      CHSUBD = ' '
      CHMETA = ' '
      CHSGML = ' '
      CHCOMP = ' '
      CTITLE = 'ZEBRA-Datastructures'
      IC1000 = 1000000
999   END
***********************************************************************
