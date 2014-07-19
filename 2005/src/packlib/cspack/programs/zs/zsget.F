*
* $Id: zsget.F,v 1.1.1.1 1996/03/08 15:44:21 mclareni Exp $
*
* $Log: zsget.F,v $
* Revision 1.1.1.1  1996/03/08 15:44:21  mclareni
* Cspack
*
*
#include "cspack/pilot.h"
#if defined(CERNLIB_OS9)
      SUBROUTINE ZSGET(ID,CHOPT)
*
*         Gets HBOOK/ZEBRA data structure for histogram ID
*         in OS9 process and sends it to the client
*
#include "cspack/hos9c.inc"
#include "cspack/czdir.inc"
#include "cspack/czunit.inc"
      TEMPLATE PAWM
      COMMON/PAWM/NM(10000)
      INTEGER F_LINK
*
#include "cspack/pawc.inc"
      DIMENSION IQ(1),Q(1),LQ(8000)
      EQUIVALENCE (LQ(1),LMAIN),(Q(1),LQ(9)),(IQ(1),Q(1))
      COMMON/HCBOOK/HVERSN,IHWORK,LHBOOK,LHPLOT,LGTIT,LHWORK,
     +LCDIR,LSDIR,LIDS,LTAB,LCID,LCONT,LSCAT,LPROX,LPROY,LSLIX,
     +LSLIY,LBANX,LBANY,LPRX,LPRY,LFIX,LLID,LR1,LR2,LCIDN
      COMMON/QUEST/IQUEST(100)
      DIMENSION IQSAVE(100)
      CHARACTER*8 CHOPT
      parameter (kntot=2)
      save iq40
*
      LCID=0
      CALL VZERO(IQUEST,100)
*
*           Remote CPU
*
      if(lunrz(idir).lt.0)then
         if(chopt(1:2).eq.'ND')then
            call Net_Hcopym(id,istat)
            iquest(1)=istat
            iquest(40)=iq40
            iquest(41)=iq40
         elseif(chopt(1:2).eq.'NC') then
            do 5 i=1,nhist
               if(ihead(kid,i).eq.id)then
                  iquest(12)=ihead(kntot,i)
                  if(i.lt.nhist)then
                     iquest(40)=ihead(kid,i+1)
                     iquest(41)=iquest(40)
                     iq40=iquest(40)
                  else
                     iquest(40)=0
                     iq40=0
                  endif
                  go to 10
               endif
   5        continue
            iquest(1)=1
         else
            if(nhist.gt.0)then
               iquest(31)=ihead(kid,1)
            else
               iquest(31)=9999
            endif
         endif
         go to 10
      endif
*
      IF(CHOPT(1:2).EQ.'N ')THEN
         CALL HNEAR(0,IDC,IDNEXT)
         IF(IDNEXT.EQ.0)THEN
            IDNEXT=9999
            IQUEST(1)=1
            GO TO 10
         ENDIF
         IQUEST(31)=IDNEXT
         IQUEST(40)=IDNEXT
      ELSEIF(CHOPT(1:2).EQ.'NC') THEN
         CALL HNEAR(ID,IDC,IDNEXT)
         IF(IDC.EQ.0)IQUEST(1)=1
         IQUEST(40)=IDNEXT
         IQUEST(41)=IDNEXT
         iq40=idnext
      ELSE
*
*          Read ZEBRA structure from PAWM module
*
         CALL HCOPYM(ID,NM,0)
         iquest(40)=iq40
         iquest(41)=iq40
      ENDIF
*
*          Send the IQUEST vector
*
  10  CALL UCOPY(IQUEST,IQSAVE,100)
*
*          Transfer Data structure to client
*
      IF(LCID.NE.0)THEN
         CALL FZOUT(LUNFZO,IHDIV,LCID,1,' ',2,100,IQSAVE)
         CALL HDELET(0)
      ELSE
         CALL FZOUT(LUNFZO,0,0,1,'Z',2,100,IQSAVE)
      ENDIF
*
  99  END
 
#endif
