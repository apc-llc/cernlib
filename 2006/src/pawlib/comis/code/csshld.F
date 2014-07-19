*
* $Id: csshld.F,v 1.1.1.1 1996/02/26 17:16:31 mclareni Exp $
*
* $Log: csshld.F,v $
* Revision 1.1.1.1  1996/02/26 17:16:31  mclareni
* Comis
*
*
#include "comis/pilot.h"
*CMZ :  1.19/00 07/04/95  14.24.32  by  Unknown
*-- Author :    Vladimir Berezhnoi   07/01/94
      SUBROUTINE CSSHLD
#if defined(CERNLIB_SHL)
#include "comis/cshlnm.inc"
      COMMON /CSTBCS/ ITBC,ITBS
      CHARACTER *8 CPID
      CHARACTER *80 CHLINE,LIBNAME
      INTEGER  SYSTEMF
      CALL GETPIDF(IPID)
      CPID=' '
      WRITE(CPID,'(I8)')IPID
      LP=LOG10(REAL(IPID))+1
      CPID=CPID(9-LP:)
      IF(ITBS.EQ.0)GO TO 2
      N=0
 1    CALL CS_SHL_GET(N,LIBNAME)
      IF(LIBNAME.NE.' ')THEN
        LL=LENOCC(LIBNAME)
***        IF(     INDEX(LIBNAME(:LL),  '/tmp/').NE.0
***     +    .AND. INDEX(LIBNAME(:LL),CPID(:LP)).NE.0)
***     +  THEN
        IF(INDEX(LIBNAME(:LL),CPID(:LP)).NE.0)THEN
          CALL CS_SHL_UNLOAD(LIBNAME(:LL))
          ITBS=ITBS-1
          CHLINE='/bin/rm -f '//LIBNAME(:LL)
          L=LENOCC(CHLINE)
          IERR=SYSTEMF(CHLINE(:L))
*          print *,'csshlDELl: ',chline(:l)
        ELSE
*          print *,'csshlkeep: ',libname(:ll)
          N=N+1
        ENDIF
        GO TO 1
      ENDIF
*
*-- delete .f files
*
 2    LPATH  = LENOCC(CHPATH)
      CHLINE = '/bin/rm -f '//CHPATH(:LPATH)//'*_'//CPID(:LP)//'.f'
#endif
#if (defined(CERNLIB_SGI)||defined(CERNLIB_ALPHA_OSF))&&(defined(CERNLIB_SHL))
     +         //' '//CHPATH(:LPATH)//'so_locations'
#endif
#if defined(CERNLIB_SHL)
      L = LENOCC(CHLINE)
      IERR = SYSTEMF(CHLINE(:L))
#endif
      END
