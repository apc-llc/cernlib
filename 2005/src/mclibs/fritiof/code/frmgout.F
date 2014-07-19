*
* $Id: frmgout.F,v 1.1.1.1 1996/01/11 14:05:22 mclareni Exp $
*
* $Log: frmgout.F,v $
* Revision 1.1.1.1  1996/01/11 14:05:22  mclareni
* Fritiof
*
*
C********************************* END FRBOOT1 ************************
 
 
C********************************* FRMGOUT *****************************
 
      SUBROUTINE FRMGOUT(ID,ILIST,MESG,A,B,C,D,E)
 
C...FOR GENERAL MESSAGE PRINT OUT ....................................
C...ID:  -50 - 50  ID number for the error:
C.....   If ID=0, the execution will be stopped upon MESG printout;
C.....   If ID>0, the execution will continue but the MESG printout
C.....     is limited to NTERM times;
C.....   If ID<0, the execution will continue with the MESG printout
C......     but execution stops if it repeats NTERM times.
C......A,B,C,D,E some variables to be printed out for inspection,
C.........          however, the will not be printed if they are all 0.
C...ILIST=1: a full list including FR-status, event list, hard parton list
C........          is given.
C........      =0: full list suppressed.
C..
C...MGO(1) - energy nonconservation in FRRINGO;
C...MGO(2) - energy nonconservation in FRPPART;
 
 
      PARAMETER (KSZ1=20)
      COMMON/FRINTN0/PLI0(2,4),AOP(KSZ1),IOP(KSZ1),NFR(KSZ1)
      COMMON/LUDAT1/MSTU(200),PARU(200),MSTJ(200),PARJ(200)
      COMMON/FRJETS/NJ,KJ(100,5),PJ(100,5)
 
      COMMON/FRMGOV/MGO(-5:10)
 
      CHARACTER *(*) MESG
 
      SAVE /FRINTN0/,/LUDAT1/,/FRJETS/,/FRMGOV/
      DATA NTERM/10/
 
   2  FORMAT(2X, A,3X,'Error ID=',I3,1x,'Count=',I3, /)
 
      MGO(ID) = MGO(ID)+1
      IF(MGO(ID).LE.NTERM) IOP(16)=1
      IF(ID.GT.0.AND.MGO(ID).GT.NTERM) RETURN
 
      IQAB = 1
      IF(ABS(A)+ABS(B)+ABS(C)+ABS(D)+ABS(E).EQ.0.) THEN
      IQAB = -1
      ENDIF
      WRITE(MSTU(11),10) NFR(1), IOP(1)
 
      WRITE(MSTU(11),2) MESG, ID, MGO(ID)
 
      IF(IQAB.GT.0) WRITE(MSTU(11),*) A,B,C,D,E
 
      CALL FRVALUE(0)
 
      IF(ILIST.EQ.1) THEN
      CALL LULIST(2)
      IF(NJ.GT.0) CALL FRHPLIS
      ENDIF
 
      WRITE(MSTU(11),20)
 
      IF(ID.EQ.0.OR.(ID.LT.0.AND.MGO(ID).GE.NTERM)) THEN
      WRITE(MSTU(11),*) ' Severe! EXECUTION STOPPED BY FRMGOUT'
      STOP 'FRMGOUT:'
      ENDIF
 
10    FORMAT(/72('*') /72('?')/,' POSSIBLY AN ERROR! AT EVENT NO. ',I7,
     > 3X,'SUBCOLLISION ',I4)
20    format(72('|')/72('*'),/)
 
      RETURN
      END
