*
* $Id: dzdpuv.F,v 1.1.1.1 1996/03/04 16:13:04 mclareni Exp $
*
* $Log: dzdpuv.F,v $
* Revision 1.1.1.1  1996/03/04 16:13:04  mclareni
* Dzdoc/Zebpack
*
*
#include "dzdoc/pilot.h"
      SUBROUTINE DZDPUV(IXDIV,LIN)
#include "zebra/mqsys.inc"
*--
      CHARACTER CQSTAK*13,CQINFO*40
      PARAMETER (NLICHQ=130,NSTCHQ=8,NDVCHQ=8,NBKCHQ=4 )
      CHARACTER CQLINE*(NLICHQ),CQMAP(10)*(NLICHQ)
      CHARACTER CQSTOR*(NSTCHQ),CQDIV*(NDVCHQ),CQID*(NBKCHQ)
      COMMON /DZC1CH/ CQSTOR,CQDIV,CQID,CQMAP,CQSTAK,CQINFO
      EQUIVALENCE (CQLINE,CQMAP)
*--
      INTEGER IWORDF, IWORDL, IXDIV, LIN, IDHIST
      CHARACTER*8 CNAME, SPACES, CDHIST
      CHARACTER*4 CHOPT
      CHARACTER*1 CTYPE
*--
      CALL MZSDIV(IXDIV,-7)
      CALL KUGETI(IDHIST)
      IF(IDHIST.LE.0)IDHIST=12345
      WRITE(CDHIST,'(I8)')IDHIST
      CALL KUGETC(CNAME,NCH)
      CALL KUGETI(IWORDF)
      CALL KUGETI(IWORDL)
      CALL KUGETC(CHOPT,NCH)
      CALL CLTOU(CHOPT)
 
      IF(IWORDF.LE.0)IWORDF=1
      IF(IWORDL.LE.0)IWORDL=IQ(KQS+LIN-1)
      IF(CNAME.EQ.' ')THEN
         CALL UHTOC(IQ(KQS+LIN-4),4,CNAME,4)
         WRITE(CNAME(5:8),'(I4)',ERR=10)IQ(KQS+LIN-5)
         GOTO 11
10       CONTINUE
         CNAME(5:8)='9999'
11       CONTINUE
         CNAME=SPACES(CNAME,0)
      ENDIF
*     get type
      CALL DZSHOW(' ',IXDIV, LIN,'Q',1,0,IWORDF,IWORDF)
      IF     (INDEX(CQLINE(13:24),'"').NE.0)THEN
         CTYPE='H'
         CHOPT=' '
         WRITE(*,*)'Warning: Type is hollerith, dont draw or plot it'
      ELSE IF(INDEX(CQLINE(13:24),'.').NE.0)THEN
         CTYPE=' '
      ELSE
         CTYPE='I'
      ENDIF
 
*     check type of last
 
      CALL DZSHOW(' ',IXDIV, LIN,'Q',1,0,IWORDL,IWORDL)
      IF     (INDEX(CQLINE(13:24),'"').NE.0)THEN
         IF(CTYPE.NE.'H')GOTO 888
      ELSE IF(INDEX(CQLINE(13:24),'.').NE.0)THEN
         IF(CTYPE.NE.' ')GOTO 888
      ELSE
         IF(CTYPE.NE.'I')GOTO 888
      ENDIF
 
      IF(CTYPE.NE.' ')THEN
         CALL KUVEC(CNAME,IQ(KQS+LIN+IWORDF),IWORDL-IWORDF+1,'W'//CTYPE)
      ELSE
         CALL KUVEC(CNAME,Q(KQS+LIN+IWORDF),IWORDL-IWORDF+1,'W')
      ENDIF
      IF(CHOPT.EQ.'PLOT') CALL KUEXEL('VECT/PLOT '//CNAME//CDHIST)
      IF(CHOPT.EQ.'DRAW') CALL KUEXEL('VECT/DRAW '//CNAME//CDHIST)
      GOTO 999
888   WRITE(*,*)'Warning: Type of first and last word differ, refused'
999   END
***********************************************************************
