*
* $Id: dzdshd.F,v 1.1.1.1 1996/03/04 16:13:03 mclareni Exp $
*
* $Log: dzdshd.F,v $
* Revision 1.1.1.1  1996/03/04 16:13:03  mclareni
* Dzdoc/Zebpack
*
*
#include "dzdoc/pilot.h"
      SUBROUTINE DZDSHD
#include "dzdzbrinc.inc"
      INTEGER NCH, IST, IXDIV, NPAR
      CHARACTER*80 CTEMP
      CHARACTER*4 CHOPT
      CHARACTER*80 CMD
*--
      IF(CTDDOC.NE.' ')CALL RZCDIR(CWDSAV,'R')
      CALL DZDIBR
      CALL KUPATL(CMD,NPAR)
      CALL KUGETC(CTEMP,NCH)
      IST=IACTST
      IXDIV=2
 
      IF(NCH.NE.0)THEN
         IF (INDEX(CTEMP,'DIV').NE.0)THEN
            IFC=INDEX(CTEMP,'DIV')+3
            READ(CTEMP(IFC:IFC+1),'(I2)') IXDIV
         ENDIF
         IF (INDEX(CTEMP,'STORE').NE.0)THEN
            IFC=INDEX(CTEMP,'STORE')+5
            READ(CTEMP(IFC:IFC+1),'(I2)') IST
         ENDIF
      ENDIF
      CALL SBYT(IST,IXDIV,27,6)
      IF      (CMD.EQ.'SHOW_STORE')THEN
         CALL DZSTOR('------ Zbrowse------',IXDIV)
      ELSE IF(CMD.EQ.'SNAP_DIV')THEN
         CALL DZSNAP('------ Zbrowse------',IXDIV,' ')
      ELSE IF(CMD.EQ.'VERIFY_DIV')THEN
         CALL  KUGETC(CHOPT,NCH)
         CALL DZVERI('------ Zbrowse------',IXDIV,CHOPT)
      ELSE IF(CMD.EQ.'SHOW_DIV')THEN
         CALL IZPICT('DZDISP','SQ')
         CALL IZPICT('DZDISP','M')
         CALl ISTXFP(6,0)
         CALL DZDDIV(IXDIV,LBANKI,CTDDOC,'D'//CCOL//CCACT,
     &   IWKZEB,IWMZEB,ILOZEB, 0)
      ELSE IF(CMD.EQ.'COL_GARB')THEN
         CALL MZGARB(IXDIV,0)
      ENDIF
      IF(CTDDOC.NE.' ')CALL RZCDIR(CWDSAV,' ')
      END
***********************************************************************
