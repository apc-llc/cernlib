*
* $Id: dzdini.F,v 1.2 1996/04/02 22:24:29 thakulin Exp $
*
* $Log: dzdini.F,v $
* Revision 1.2  1996/04/02 22:24:29  thakulin
* Remove extra SAVE statement (all variables already under generic
* SAVE).
*
* Revision 1.1.1.1  1996/03/04 16:12:54  mclareni
* Dzdoc/Zebpack
*
*
#include "dzdoc/pilot.h"
      SUBROUTINE DZDINI
************************************************************************
*.                                                                     *
*...DZDINI initializes the DZDOC package                               *
*.                                                                     *
*.    SUBROUTINE DZDINI                                                *
*.                                                                     *
*.                                                                     *
*.             THE FOLLOWING LOGICAL UNITS MUST BE DEFINED             *
*.             ===========================================             *
*.             Input on cards              on unit LUNIN               *
*.             Output for logging          on unit LUNOUT              *
*.             User defined unit           on unit LUNUSR              *
*.             Can contain the bank and program description, tables,...*
*.                                                                     *
*. CALLS    : MZIOBK,MZLIFT,MZLINK,UCTOH                               *
*. CALLED   : DZDOCM                                                   *
*. COMMON   : DZDBSC,DZDBSN,DZDOCC,DZDPAC,DZDPRC,DZDRPC,DZDRPN,DZDSOC  *
*. COMMON   : DZDSON,DZDTAP,DZDTIC,DZDTIN,DZDTYP,DZDUSC,DZDUSN         *
*.                                                                     *
*. AUTHOR   : M. Goossens DD/US                                        *
*. VERSION  : 2.01(17)  /  1 Sep 1986                                  *
*.                                                                     *
*.**********************************************************************
      SAVE
 
#include "dzdoc/bkwrp.inc"
#include "dzdoc/tapes.inc"
#include "dzdoc/title.inc"
#include "dzdoc/docparq.inc"
      DOUBLE PRECISION DATE,TIME
      INTEGER INIFLG
      DATA INIFLG/0/
 
 
*----OTTO
      IF(INIFLG.NE.0)GOTO 999
      INIFLG=1
      LUNOUT =  6
      IF (LUNUSR.EQ.0) LUNUSR = LUNOUT
 
*--     Initialize COMMON  DZDTYP
 
      ISTATE = 0
      IDTAG  = 0
      ICONT  = 0
      ISLASH = 0
      IREPLV = 0
      CALL UCTOH (')()(',IDEMTX,4,4)
      CALL UCTOH ('$LF/',IILFLF,4,4)
      INUM1  = 0
      INUM2  = 0
      NBNKS  = 0
      NWTAG  = 0
      NHTAG  = 0
      NWTXT  = 0
      NHTXT  = 0
 
*--     Go to the primary store and declare the DZDOC link area
 
      CALL MZSDIV (0,-1)
      CALL MZLINK (0,'/DZDOCC/',LDQBKD,LQBKD,LDUMMY)
 
      CALL DATIMH(DATE,TIME)
      CALL UHTOC(DATE,4,CDATE,8)
      CALL UHTOC(TIME,4,CTIME,8)
 
  999 END
