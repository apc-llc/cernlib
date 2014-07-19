*
* $Id: dzdgrc.F,v 1.1.1.1 1996/03/04 16:13:18 mclareni Exp $
*
* $Log: dzdgrc.F,v $
* Revision 1.1.1.1  1996/03/04 16:13:18  mclareni
* Dzdoc/Zebpack
*
*
#include "dzdoc/pilot.h"
      SUBROUTINE DZDGRC(IVDOC,IOFF,IVAL,IREPC,NEXTRA,IVR,MV,NV)
 
      INTEGER IVDOC(2),IVAL,IREPC,NEXTRA,IVR(2),MV,NV
      CHARACTER*80 CL
 
*     IVDOC contains next sector of bank doc
*     IOFF  offset where expression might start (0, for *REP, 2 N:NREP
*     IVAL  value of last documented word, possibly
*           containing the repetition count
*     IREPC returned value
 
      IF(MV.GT.0)NV=0
      IREPC = -1
      IFLEXT=1
      NEXTRA=0
*     is it holl section?
      IT = MOD(IVDOC(1),16)
      IF(IT.NE.5)GOTO 999
      NC = IVDOC(1)/16*4
      IF(NC.GT.80)GOTO 999
      CALL UHTOC(IVDOC(2+IOFF),4,CL,NC)
      IF(CL(1:4).NE.'BITS')GOTO 999
      READ(CL(5:6),'(I2)',ERR=999)IB1
      IF(IB1.GT.31)GOTO 999
      READ(CL(7:8),'(I2)',ERR=999)IB2
      IF(IB2.GT.31 .OR. IB2.LT.IB1)GOTO 999
      IREPC = JBYT(IVAL,IB1+1,IB2-IB1+1)
      IF(MV.GE.1)THEN
         NV=NV+1
         IVR(NV)=IB1
         CALL SBYT(IB2,IVR(NV),17,16)
      ENDIF
*     only single value given
      IF(NC.LE.8)GOTO 999
*     decode expression
      IVAL=IREPC
      CALL DZDGR1(CL(9:),IVAL,IREPC,NEXTRA,IVR,MV,NV)
  999 END
************************************************************************
