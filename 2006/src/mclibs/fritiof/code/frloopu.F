*
* $Id: frloopu.F,v 1.1.1.1 1996/01/11 14:05:22 mclareni Exp $
*
* $Log: frloopu.F,v $
* Revision 1.1.1.1  1996/01/11 14:05:22  mclareni
* Fritiof
*
*
C********************************* END FRUPCAS ***********************
 
C********************************* FRLOOPU ***************************
 
      SUBROUTINE FRLOOPU(*,I,IMAX,MESSAGE)
 
C....Handle loops to avoid infinite loop.
C....I=loop index;  IMAX=maximum number of looping;
C....MESSAGE = character code to mark the loop.
 
      PARAMETER (KSZ1=20)
      CHARACTER*(*) MESSAGE
      COMMON/LUDAT1/MSTU(200),PARU(200),MSTJ(200),PARJ(200)
      COMMON/FRINTN0/PLI0(2,4),AOP(KSZ1),IOP(KSZ1),NFR(KSZ1)
      SAVE /LUDAT1/,/FRINTN0/
 
      IF(I.LE.IMAX) THEN
      RETURN 1
      ELSE
      WRITE(MSTU(11),110) NFR(1),IOP(1),MESSAGE
      RETURN
      ENDIF
 
110   FORMAT(/,15('?'),2x,'Event-subcollision number:',I6,'_',I4,
     >      10('?')/,4X,'Loop aborted at ', A,/)
 
      END
