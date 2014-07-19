*
* $Id: palogs.F,v 1.2 1996/06/03 14:03:24 couet Exp $
*
* $Log: palogs.F,v $
* Revision 1.2  1996/06/03 14:03:24  couet
* - paw version is now 2.08/00
*
* Revision 1.1.1.1  1996/03/01 11:38:41  mclareni
* Paw
*
*
#include "paw/pilot.h"
*CMZ :  2.07/02 17/05/95  11.58.51  by  O.Couet
*-- Author :    Rene Brun   03/01/89
      SUBROUTINE PALOGS(ICASE)
*
*             Routine for PAW monitoring
*
#include "paw/rzcxio.inc"
#include "hbook/hcpiaf.inc"
#include "paw/pcwk.inc"
*
      CHARACTER*80 PAWMES
      SAVE IWK1,IH1,IM1,TIM1
*
*________________________________________________________
*
*
*-- Do not report anything in case of a piaf slave server
*
      IF (SLAVPF) RETURN
*
      PAWMES=' '
      IF(ICASE.EQ.1)THEN
         CALL TIMEX(TIM1)
         IWK1=IWK
         CALL DATIME(IDAT1,ITIM1)
         IH1=ITIM1/100
         IM1=MOD(ITIM1,100)
         WRITE(PAWMES,1000)IWK
      ELSE
         CALL TIMEX(TIMNOW)
         CPTIME=TIMNOW-TIM1
         CALL DATIME(IDAT2,ITIM2)
         IH2=ITIM2/100
         IM2=MOD(ITIM2,100)
         IF(IH2.LT.IH1)IH2=IH2+24
         IF(IM2.LT.IM1)THEN
            IH2=IH2-1
            IM2=IM2+60
         ENDIF
         IHD=IH2-IH1
         IMD=IM2-IM1
         IF(IHD.EQ.0.AND.IMD.EQ.0)IMD=1
         IRTIME=60*IHD+IMD
*
*-- get memory usage
*
         TOTIO = RZXIN + RZXOUT
         CALL HBVM(WORDS)
*
*-- In case of the master server add the IO and CPU usage of the slaves
*
         IF (MASTPF) THEN
            TOTIO  = TOTIO  + SRIN   + SROUT
            WORDS  = WORDS  + SVMIO
            CPTIME = CPTIME + STUSER + STCPU
         ENDIF
*
         IRTIME=MIN(999 ,IRTIME)
         CPTIME=MIN(99999.,CPTIME)
*
*-- get number of KUIP commands
*--     NCTOT= Total number of commands executed (calls to action routines)
*--     NCOMD= Total number of different commands invoked
*
         NCTOT=KUSTAT(0)
         NCOMD=KUSTAT(1)
*
         IF(CPTIME.GT.9999.)THEN
            WRITE(PAWMES,3010)IWK1,IRTIME,CPTIME
         ELSEIF(CPTIME.GT.999.)THEN
            WRITE(PAWMES,3000)IWK1,IRTIME,CPTIME
         ELSE
            WRITE(PAWMES,2000)IWK1,IRTIME,CPTIME
         ENDIF
         CALL UMLOG('PAWMONIT',PAWMES)
         VIO=4.*WORDS*1.E-6
         RZIO=4.*(TOTIO)*1.E-6
         PAWMES=' '
         IF(RZIO.GT.99999..OR.VIO.GT.99999.)THEN
            WRITE(PAWMES,4010)NCTOT,NCOMD,RZIO,VIO
         ELSE
            WRITE(PAWMES,4000)NCTOT,NCOMD,RZIO,VIO
         ENDIF
      ENDIF
*
      CALL UMLOG('PAWMONIT',PAWMES)
*
 1000 FORMAT('LOG208 WTYP=',I6)
 2000 FORMAT('LOGOUT WTYP=',I6,' RT=',I3,' min   CP=',F7.3,' sec')
 3000 FORMAT('LOGOUT WTYP=',I6,' RT=',I3,' min   CP',F8.3,' sec')
 3010 FORMAT('LOGOUT WTYP=',I6,' RT=',I3,' min   C',F9.3,' sec')
 4000 FORMAT('LOGXIO KUIP=',I6,'   :',I3,' IO=',F9.3,':',F9.3,'MB')
 4010 FORMAT('LOGXIO KUIP=',I6,'   :',I3,' IO',F10.3,F10.3,'MB')
*
      END

