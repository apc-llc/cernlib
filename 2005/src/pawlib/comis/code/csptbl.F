*
* $Id: csptbl.F,v 1.2 1996/02/29 09:46:30 cernlib Exp $
*
* $Log: csptbl.F,v $
* Revision 1.2  1996/02/29 09:46:30  cernlib
* Avoid a trigraph
*
* Revision 1.1.1.1  1996/02/26 17:16:16  mclareni
* Comis
*
*
#include "comis/pilot.h"
*CMZU:  1.16/16 01/10/93  12.02.34  by  Vladimir Berezhnoi
*-- Author : V.Berezhnoi
      SUBROUTINE CSPTBL(NAME)
***-----------------------------------------------
*     csptbl outputs to terminal list of variables
*     the common block /NAME/
***-----------------------------------------------
#include "comis/cspar.inc"
#include "comis/mdpool.inc"
#include "comis/cstabps.inc"
#include "comis/cstab.inc"
#include "comis/cspnts.inc"
       INTEGER CSLTGB
       CHARACTER NAME*(*), VDESC*28, BUFF*72, TYPV(7)*6
*  to avoid the trigraph, put the second ? of *?? later
       DATA TYPV/'int','real','*','log','dp','*? ','cx'/
      CALL CSCHID(NAME)
      TYPV(6)(3:3)='?'
      I=CSLTGB(IPVS)
      IF(I.EQ.0)THEN
        BUFF='  no block with name '//NAME(1:NCIDEN)
        PRINT *,BUFF
        RETURN
      ENDIF
      IPOS=3
      BUFF= ' '
      IF(ICGB.EQ.-1.OR.ICGB.EQ.1.OR.ICGB.EQ.3)THEN
        BUFF(IPOS:)='COMMON'
        IPOS=IPOS+7
      ELSEIF(ICGB.EQ.2.OR.ICGB.EQ.4)THEN
        BUFF(IPOS:)='GLOBAL'
        IPOS=IPOS+7
      ENDIF
      BUFF(IPOS:)='/' // NAME(1:NCIDEN) // '/'
      IPOS=IPOS+NCIDEN+2
      PRINT *,BUFF(1:IPOS),' -- List of variables:'
      IT=IPGI
      NVAR=0
      IPOS=1
      BUFF=' '
      JVNM=MJSCHA(VDESC(3:))
  1   IF(IT.EQ.0)GO TO 999
      VDESC=' '
      CALL CCOPYA(IQ(IT+1),NCIDGI,KSLENI)
      IF(IQ(NUMGI).NE.IADGB)THEN
         IT=IQ(IT)
         GO TO 1
      ENDIF
      J=MJCHAR(IQ(IT+KSIDG))
      CALL CCOPYS(J,JVNM,NCIDGI)
      IF(MODEGI.GT.1)THEN
         NDIM=IQ(MODEGI+3)
         L=IQ(MODEGI+NDIM+3)
         WRITE(VDESC(19:27),72)L
 72      FORMAT('(',I7,')')
         I1=20
         DO 11 I=20,27
           IF(VDESC(I:I).NE.' ')THEN
              VDESC(I1:I1)=VDESC(I:I)
              I1=I1+1
           ENDIF
 11      CONTINUE
         IF(I1.LT.27)VDESC(I1:27)=' '
      ENDIF
      VDESC(12:17)=TYPV(IABS(ITYPGI))
      IF(IABS(ITYPGI).EQ.3)THEN
         WRITE(VDESC(13:17),73)LENEGI
 73      FORMAT(I5)
      ENDIF
      BUFF(IPOS:)=VDESC
      IPOS=IPOS+28
      NVAR=NVAR+1
      IF(MOD(NVAR,2).EQ.0)THEN
         PRINT 10,BUFF
         BUFF=' '
         IPOS=1
      ENDIF
 10   FORMAT(A)
      IT=IQ(IT)
      GO TO 1
 999  IF(IPOS.GT.1)PRINT 10,BUFF
      END
