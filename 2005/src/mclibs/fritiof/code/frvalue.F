*
* $Id: frvalue.F,v 1.1.1.1 1996/01/11 14:05:22 mclareni Exp $
*
* $Log: frvalue.F,v $
* Revision 1.1.1.1  1996/01/11 14:05:22  mclareni
* Fritiof
*
*
C***************************** END FRLOOPU ***************************
 
C*************************** FRVALUE *********************************
 
C...To output the values of FRITIOF parameters ...................
C... IF IQ=0, THE OUTPUT IS WRITTEN ON MSTU(11);
C... IF IQ>0, IT WILL WRITES ON AN AUX FILE NAMED 'Oxchk', which
C... is refreshed at an interval of every INUM events.
C... IF IQ<0, as in IQ>0 but "Execution completed" will ALSO be printed.
 
      SUBROUTINE FRVALUE(iq)
 
      parameter (KSZ1=20)
      COMMON/FRPARA1/KFR(KSZ1),VFR(KSZ1)
      COMMON/FRINTN0/PLI0(2,4),AOP(KSZ1),IOP(KSZ1),NFR(KSZ1)
      COMMON/FRCONT2/ICT(10),ICTT(10)
      COMMON/LUDAT1/MSTU(200),PARU(200),MSTJ(200),PARJ(200)
      COMMON/FRMGOV/MGO(-5:10)
 
      SAVE ICO
      SAVE /FRPARA1/,/FRINTN0/,/FRCONT2/,/LUDAT1/,/FRMGOV/
      DATA ICO,INUM /0,20/
 
      IF(IQ.EQ.0) THEN
      IFILE = MSTU(11)
      ELSE
      ICO = ICO + 1
      IFILE = 111
       IF(ICO.EQ.1) THEN
       OPEN(IFILE,FILE='Oxchk',STATUS='UNKNOWN')
       ELSEIF(MOD(ICO,INUM).EQ.1) THEN
       OPEN(IFILE,FILE='Oxchk',STATUS='OLD')
       ENDIF
      ENDIF
 
      WRITE(IFILE,100)
100      FORMAT(/,79('='),/2x,'Fritiof Status Report',/)
 
      WRITE(IFILE,106)
      WRITE(IFILE,166)(KFR(L),L=1,15),(VFR(L),L=1,15),(IOP(L),L=1,15)
 
      WRITE(IFILE,103)(NFR(L),L=1,10),(ICT(L),L=1,10),(ICTT(L),L=1,10)
103   FORMAT(79('-'),/,
     >   4X,7X,'1|',6X,'2|',6X,'3|',6X,'4|',6X,'5|',5X,'6|',5X,'7|',
     >     5x,'8|',5X,'9|',4X,'10|',/,79('-'),/,
     >   1X,'NFR',5I8,5I7,/,
     >   1X,'ICT',5I8,5I7,/,'ICTT',5I8,5I7,/,79('-'),/ )
 
          ratio1 = 0.
          ratio2 = 0.
          if(NFR(3).GT.0) RATIO1 = FLOAT(NFR(4))/FLOAT(NFR(3))
          if(NFR(1).GT.0) RATIO2 = FLOAT(NFR(5))/FLOAT(NFR(1))
 
      WRITE(IFILE,*)' Percent of collisions having hard scattering: ',
     >     ratio1
      WRITE(IFILE,*)' Percent of events having hard scattering: ',
     >     ratio2
 
      IF(MGO(1).GT.0) WRITE(IFILE, 171) MGO(1)
      IF(MGO(2).GT.0) WRITE(IFILE, 172) MGO(2)
 
CC      call datecpu(IFILE)
 
      IF(IQ.LT.0) WRITE(IFILE, *) ' &&& EXECUTION COMPLETES &&&&'
 
        IF(IQ.NE.0.AND.MOD(ICO,INUM).EQ.0) CLOSE(IFILE)
 
106      FORMAT(20X,'KFR(L), VFR(L), IOP(L)  ',30X,/,78('-') )
166      FORMAT(
     >   7X,'1|',3X,'2|',3X,'3|',3X,'4|',3X,'5|',3X,'6|',3X,'7|',
     >     3x,'8|',3X,'9|',2X,'10|',2X,'11|',2X,'12|',2X,'13|',
     >     2X,'14|',2X,'15|',/,78('-'),/,
     >   'KFR', 15I5,/,'VFR', 15F5.2,/,'IOP', 15I5,/,78('-'),/ )
 
171   FORMAT(/,2X,'No. of errors - energy nonconserv. in FRRINGO:',I6)
172   FORMAT(/,2X,'No. of errors - energy nonconserv. in FRPPART:',I6,/)
 
      return
      end
