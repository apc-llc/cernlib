*
* $Id: csptli.F,v 1.1.1.1 1996/02/26 17:16:16 mclareni Exp $
*
* $Log: csptli.F,v $
* Revision 1.1.1.1  1996/02/26 17:16:16  mclareni
* Comis
*
*
#include "comis/pilot.h"
*CMZ :  1.18/14 28/09/94  17.54.34  by  Vladimir Berezhnoi
*-- Author : V.Berezhnoi
      SUBROUTINE CSPTLI(KPRO,IP,IND,PROADDR)
***------------------------------------
*      sets address to comis arrays
***------------------------------------
      INTEGER CSITGI, PROADDR
*d      character *32 name
#include "comis/cspar.inc"
#include "comis/mdpool.inc"
#include "comis/cstabps.inc"
#include "comis/cstab.inc"
#include "comis/cspnts.inc"
#include "comis/cssysd.inc"
#include "comis/cskucs.inc"
#include "comis/cscbwl.inc"
      IF(KPRO.EQ.3)THEN
        IPVS=IPGI
        J=IPGI
  1     IF(J.NE.0)THEN
          IPVS=J
          J=IQ(J)
          GO TO 1
        ENDIF
      ENDIF
      I=IPLI
      IPLI=0
      IPDAR=IP+2
  2   IF(I.EQ.0)RETURN
      CALL CSLDLI(I)
*d      call csgtidl(i,name,nc)
*d      if(lxxgli.ne.0)then
*d        print *,' +name+ ',name(:nc),numgi
*d      else
*d        print *,' -name- ',name(:nc)
*d      endif
      IF(ICBWL.GT.0 .AND. NUMGI.GT.0 .AND. LXXGLI.NE.0)THEN
*       try add var to used list
        IF(IQ(NUMGI+1).LE.2)CALL CSTADV(PROADDR)
      ENDIF
      ITYPE=IABS(ITYPGI)
      IF(MODEGI.GT.1)THEN
        IF(NUMGI.LE.0)THEN
          IQ(MODEGI-1)=IQ(IPDAR)
          IQ(IPDAR)=MODEGI-1
          IF(NUMGI.EQ.0)THEN
            IN=IND+ISHGI
            J=LOCF(IQ(IN))
            IF(ITYPE.EQ.3)J=MJCHAR(IQ(IN))
          ENDIF
        ELSE
          IADGB=IQ(NUMGI)
          ICGB=IQ(NUMGI+1)
          J=IADGB+ISHGI
          IF(ITYPE.EQ.3)J=J*NBYTPW
          IF(ICGB.GT.2)THEN
            IG=IADGB-LOCF(IQ(1))
            IQ(MODEGI-1)=IQ(IG)
            IQ(IG)=MODEGI-1
          ELSEIF(KPRO.NE.3 .OR. IADGB.EQ.JKUVBS)THEN
            IQ(MODEGI-1)=IQ(IPDAR)
            IQ(IPDAR)=MODEGI-1
          ELSE
            IQ(MODEGI-1)=IPMCAD
            IPMCAD=MODEGI-1
          ENDIF
        ENDIF
        IQ(MODEGI+1)=J
      ENDIF
      IF(NUMGI.GT.0 .AND. KPRO.EQ.3 .AND. IADGB.NE.JKUVBS) THEN
        NCIDEN=NCIDGI
        NWIDEN=(NCIDEN+3)/4
        CALL CCOPYA(IQ(I+KSIDL),IDEN,NWIDEN)
        J=CSITGI(IPVS)
        IPVS=J
      ENDIF
      IF(MODEGI.EQ.-1 .AND. ITYPE.EQ.3)THEN
        CALL MHFREE(IVPAR)
      ENDIF
      I1=IQ(I)
      CALL MHFREE(I)
      I=I1
      GO TO 2
      END
