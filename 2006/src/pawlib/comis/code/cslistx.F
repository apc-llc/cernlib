*
* $Id: cslistx.F,v 1.1.1.1 1996/02/26 17:16:30 mclareni Exp $
*
* $Log: cslistx.F,v $
* Revision 1.1.1.1  1996/02/26 17:16:30  mclareni
* Comis
*
*
#include "comis/pilot.h"
*CMZ :  1.18/14 28/09/94  19.44.02  by  Vladimir Berezhnoi
*-- Author :    Vladimir Berezhnoi   14/12/93
      SUBROUTINE CSLISTX(CNAME,PROADDR,IOFFS,NEL)
***--------------------------------------------------------------
*    returns the offset of var used in routines
*    for given common block
*
*         CALL CSLISTX('cbname',ioffs,nel)
*    where
*      ioffs is  variable's displacement(starting from 0 )
*      nel   is  number of elements
*
*   for information about first variable call CSLISTX with ioffs=-1
*   if no more variables in the list  CSLISTX sets           nel= 0
*   to clear the list call CSLISTX with                    ioffs=-2
*   if CSLISTX was not called with ioffs=-1                  nel=-1
***----------------------------------------------------------------
      CHARACTER *(*) CNAME, LASTNM*32
      INTEGER PROADDR
#include "comis/cspar.inc"
#include "comis/mdpool.inc"
#include "comis/cstab.inc"
#include "comis/cscbwl.inc"
      DATA IPVARL/0/, LASTPA/0/ LASTNM/' '/
      SAVE IPVARL,    LASTPA,   LASTNM
*
*-  comelem=( next, prolist, baseaddress, nchs, name )
*-  proelem=( next, varlist, proaddress )
*-  varelem=( next, offset, nel, lenel )
*
      NEL=0
 1    IF(IOFFS.GE.0)THEN
        IF(CNAME.NE.LASTNM .OR. PROADDR.NE.LASTPA)THEN
          NEL=-1
          RETURN
        ENDIF
        IF(IPVARL.EQ.0)RETURN
        IOFFS=IQ(IPVARL+1)
        NEL  =IQ(IPVARL+2)
        IPVARL=IQ(IPVARL)
      ELSE
        IPVARL=0
        I=ICBWL
   2    IF(I.EQ.0)THEN
          LASTNM=' '
          LASTPA=0
          RETURN
        ENDIF
        LASTNM=' '
        NCHS=IQ(I+3)
        CALL UHTOC(IQ(I+4),4,LASTNM,NCHS)
***         print *,' cbname=',lastnm
        IF(LASTNM.NE.CNAME)THEN
         I=IQ(I)
         GO TO 2
        ENDIF
        IPROL=IQ(I+1)
  21    IF(IPROL.EQ.0)THEN
          LASTNM=' '
          LASTPA=0
          RETURN
        ENDIF
        IF(IQ(IPROL+2).NE.PROADDR)THEN
          IPROL=IQ(IPROL)
          GO TO 21
        ENDIF
        IPVARL=IQ(IPROL+1)
        IF(IOFFS.EQ.-1)THEN
          LASTPA=PROADDR
          IOFFS=0
          GO TO 1
        ELSEIF(IOFFS.EQ.-2)THEN
          IQ(IPROL+1)=0
 3        IF(IPVARL.EQ.0)RETURN
          I=IPVARL
          IPVARL=IQ(I)
          CALL MHFREE(I)
          GO TO 3
        ELSE
*         print *' CSlistx called with ioffs=',ioffs
          NEL=-2
          LASTNM=' '
          IPVARL=0
        ENDIF
      ENDIF
      END
