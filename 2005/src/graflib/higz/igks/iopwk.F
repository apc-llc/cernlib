*
* $Id: iopwk.F,v 1.2 1996/06/05 10:20:36 cernlib Exp $
*
* $Log: iopwk.F,v $
* Revision 1.2  1996/06/05 10:20:36  cernlib
* Move pilot.h before the ifdef for GKS
*
* Revision 1.1.1.1  1996/02/14 13:10:44  mclareni
* Higz
*
*
#include "higz/pilot.h"
#if defined(CERNLIB_GKS)
*CMZ :  1.18/00 16/04/93  09.54.05  by  O.Couet
*-- Author :
      SUBROUTINE IOPWK(WKID,CONID,WTYPE)
*.===========>
*.
*.   This routine opens an HIGZ workstation .
*.
*. _Input parameters:
*.
*.  INTEGER WKID : Workstation identifier .
*.  INTEGER CONID : Connection identifier .
*.  INTEGER WTYPE : Workstation type .
*.
*..==========> (O.Couet)
#include "higz/hiflag.inc"
#include "higz/hiatt.inc"
#if defined(CERNLIB_UNIGKS)
#include "higz/hilun.inc"
      CHARACTER*80 MSG
#endif
#if defined(CERNLIB_SUNGKS)
      LOGICAL OPENED
#endif
      INTEGER WKID,CONID,WTYPE
*.______________________________________
*
*
      IF(WKID.EQ.0)THEN
         CALL IGERR('Workstation identifier equal 0','IOPWK')
         RETURN
      ENDIF
      INOPWK=INOPWK+1
      ILOPWK(INOPWK)=WKID
      IWTYL(INOPWK)=WTYPE
      ACWKFL(INOPWK)=.FALSE.
      ICONID=CONID
#if defined(CERNLIB_PSCRIPT)
*
*              PostScript Metafile
*
      IF(WTYPE.LT.0)THEN
         CALL IPINIT(WKID,ABS(CONID),WTYPE)
         RETURN
      ENDIF
*
#endif
#if (defined(CERNLIB_PLOT10GKS))&&(defined(CERNLIB_GKS))&&(!defined(CERNLIB_IBM))
      IF(WKID.LT.2)THEN
         CALL HGTCID('TT',IERROR,ICONID)
      ENDIF
#endif
#if (defined(CERNLIB_GKS))&&(defined(CERNLIB_UNIGKS))
*
*              Is the workstation available ?
*
      I=1
      CALL GQEWK(I,IERR,NUMB,IWTYP)
      MSG= ' HIGZ    -  ERROR XXX IN GQEWK CALL'
      WRITE(UNIT=MSG(19:21),FMT='(I3)') IERR
      IF(IERR.NE.0)GOTO 20
*
      DO 10 I=1,NUMB
         CALL GQEWK(I,IERR,N,IWTYP)
         IF(IERR.NE.0)GOTO 20
         IF(WTYPE.EQ.IWTYP)GOTO 30
   10 CONTINUE
*
      MSG= ' HIGZ    -  WORKSTATION XXXXXX NOT AVAILABLE'
      WRITE(UNIT=MSG(25:30),FMT='(I6)') WTYPE
   20 WRITE(UNIT=LUNERR,FMT='(A)') MSG
      CALL GECLKS
      STOP 12
*
*              Is the workstation 'OUTIN' ?
*
   30 CALL GQWKCA(WTYPE,IERR,ICAT)
      IF (ICAT.NE.2) THEN
         MSG= ' HIGZ    -  WORKSTATION XXXXXX NOT OUTIN CATEGORY'
         WRITE(UNIT=MSG(25:30),FMT='(I6)') WTYPE
         WRITE(UNIT=LUNERR,FMT='(A)') MSG
         CALL GECLKS
         STOP 12
      ENDIF
*
#endif
#if defined(CERNLIB_SUNGKS)
      IF(WTYPE.EQ.3)THEN
         INQUIRE(ICONID,OPENED=OPENED)
         IF(.NOT.OPENED)OPEN(ICONID,FILE='SunGKS.moasc')
      ELSEIF(WTYPE.EQ.6)THEN
         INQUIRE(ICONID,OPENED=OPENED)
         IF(.NOT.OPENED)OPEN(ICONID,FILE='SunGKS.hpgl')
      ELSEIF(WTYPE.EQ.7)THEN
         INQUIRE(ICONID,OPENED=OPENED)
         IF(.NOT.OPENED)OPEN(ICONID,FILE='SunGKS.postscript')
      ELSEIF(WTYPE.EQ.8)THEN
         INQUIRE(ICONID,OPENED=OPENED)
         IF(.NOT.OPENED)OPEN(ICONID,FILE='SunGKS.cgmo')
      ELSEIF(WTYPE.EQ.10)THEN
         INQUIRE(ICONID,OPENED=OPENED)
         IF(.NOT.OPENED)OPEN(ICONID,FILE='SunGKS.mobin')
      ENDIF
#endif
#if defined(CERNLIB_GKS)
      IF(GFLAG)THEN
         CALL GOPWK(WKID,ICONID,WTYPE)
         CALL GQCF(WTYPE,IERR,INBCOL,ICOLA,NPCI)
      ENDIF
#endif
#if (defined(CERNLIB_GKS))&&(defined(CERNLIB_DECGKS))
      IF(GFLAG.AND.WTYPE.GE.200
     +        .AND.WTYPE.LE.100000) CALL GSWKVP(WKID,0.,0.2,0.,0.2)
#endif
*
      END
#endif
