*
* $Id: irqst.F,v 1.2 1996/06/05 10:20:39 cernlib Exp $
*
* $Log: irqst.F,v $
* Revision 1.2  1996/06/05 10:20:39  cernlib
* Move pilot.h before the ifdef for GKS
*
* Revision 1.1.1.1  1996/02/14 13:10:44  mclareni
* Higz
*
*
#include "higz/pilot.h"
#if defined(CERNLIB_GKS)
*CMZ :  1.19/14 05/10/93  16.00.03  by  O.Couet
*-- Author :
      SUBROUTINE IRQST(IWKID,ISTD,ISTAT,L,STR)
*.===========>
*.
*.   This routine returns a character string typed on the keyboard .
*. An initialise string is done with the parameters RQUEST(81),
*. RQUEST(82), RQUEST(91), and RQUEST(92) given via the QUEST COMMON
*. if IWKID is less than 0, Otherwise the initialise string is not done
*.
*. _Input parameters:
*.
*.  INTEGER WKID : Workstation identifier .
*.  INTEGER ISTD : Locator device .
*.
*. _Output parameters:
*.
*.  INTEGER ISTAT : Return status : 0=BREAK, 1=OK .
*.  INTEGER L : Number of characters returned .
*.  CHARACTER STR : Character string returned .
*.
*..==========> (O.Couet)
#include "higz/hiques.inc"
#if defined(CERNLIB_MAIL)
#include "higz/himail.inc"
#endif
#if defined(CERNLIB_MAIL)
#include "higz/hiflag.inc"
#endif
      CHARACTER*(*) STR
#if defined(CERNLIB_GKS)
      CHARACTER*80 STRGKS(4)
#endif
#if defined(CERNLIB_SUNGKS)
      DIMENSION IA(10), RA(10)
      CHARACTER CA*1
#endif
*.______________________________________
*
#if defined(CERNLIB_MAIL)
*
*              MAIL option
*
      IF(MFLAG)THEN
         WRITE (CHMAIL,'(3I3)') 556,ABS(IWKID),ISTD
         CALL IMWRIT(1)
         READ (5,'(2I5,A)') ISTAT,L,STR
         CALL IMWRIT(5)
         RETURN
      ENDIF
*
#endif
      CALL IGSG(0)
      IF(IWKID.LT.0)THEN
         IIWKID=ABS(IWKID)
      ELSE
         IIWKID=IWKID
         GOTO 10
      ENDIF
      ILEN=LENOCC(STR)
      INIPOS=MAX(1,ILEN)
#if (defined(CERNLIB_GKS))&&(!defined(CERNLIB_SUNGKS))
      CALL GINST(IIWKID,1,ILEN,STR,1
     +,          RQUEST(81),RQUEST(82),RQUEST(91),RQUEST(92)
     +,          32,INIPOS,0,STRGKS)
#endif
#if (defined(CERNLIB_GKS))&&(defined(CERNLIB_SUNGKS))
      IA(1)=1
      CALL GPREC(1,IA,0,RA,0,0,CA,4,IERR,LSTGKS,STRGKS)
      CALL GINST(IIWKID,1,ILEN,STR,1
     +,          RQUEST(81),RQUEST(82),RQUEST(91),RQUEST(92)
     +,          32,INIPOS,LSTGKS,STRGKS)
#endif
#if defined(CERNLIB_GKS)
 10   STR=' '
      CALL GRQST(IIWKID,ISTD,ISTAT,L,STR)
#endif
*
      END
#endif
