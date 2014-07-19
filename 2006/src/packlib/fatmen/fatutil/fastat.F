*
* $Id: fastat.F,v 1.1.1.1 1996/03/07 15:18:05 mclareni Exp $
*
* $Log: fastat.F,v $
* Revision 1.1.1.1  1996/03/07 15:18:05  mclareni
* Fatmen
*
*
#include "fatmen/pilot.h"
      SUBROUTINE FASTAT(LUNI)
*
************************************************************************
*
*         SUBR. FASTAT(LUNI)
*
*        print a summary of the status of the files managed by FATMEN
*
*        LUNI   output unit
*
*
************************************************************************
*
#include "fatmen/fatbank.inc"
#include "fatmen/fatpara.inc"
*
      CHARACTER       GENEN*255,  PATHN*255, PATH*255,
     +                PATHX*255,  FNAME*20
      DIMENSION       KEYS(10)
*
*
*______________________________________________________________________
*
*
* *** Loop over top directories
*
      WRITE(LUNI,'(//,'' FATMEN status '',//)')
      LSAVFA = LTOPFA
      IF (LSAVFA.EQ.0) THEN
        WRITE(LUNI,'('' No Database open '',//)')
        GO TO 999
      ENDIF
   10 CONTINUE
        NCHR   = IQ(KOFUFA+LSAVFA+MNCHFA)
        CALL UHTOC (IQ(KOFUFA+LSAVFA+MCHRFA), 4, PATHX, NCHR)
        WRITE(LUNI,'('' Database '',A,'' open '',/)') PATHX(1:NCHR)
*
        LBBKFA = LQ(KOFUFA+LSAVFA-KLBKFA)
        LBGNFA = LQ(KOFUFA+LSAVFA-KLGNFA)
*        Loop over open files
        NOPF = IQ(KOFUFA+LSAVFA+MNOPFA)
        IF (NOPF.EQ.0) THEN
          WRITE(LUNI,'('' No open files'',//)')
          GO TO 11
        ELSE
          WRITE(LUNI,'(1X,I5,'' open files'',/)')
        ENDIF
        LB = KOFUFA+LBBKFA
        DO 20 I=1,NOPF
          NCH = IQ(LB+MCGNFA)
          IPT = IQ(LB+MPNTFA)
          IST = IQ(LB+MSTAFA)
          CALL UHTOC (IQ(KOFUFA+LBGNFA+IPT), 4, GENEN, NCH)
          ICH =  INDEXB(GENEN(1:NCH-1),'/')
          PATHN = GENEN(1:ICH-1)
          FNAME = GENEN(ICH+1:NCH)
          WRITE(LUNI,'(1X,A)') GENEN(1:NCH)
          WRITE(LUNI,'('' status'',I10)') IST
          LB = LB + NWBKFA
20      CONTINUE
11      LSAVFA = LQ(KOFUFA+LSAVFA)
      IF (LSAVFA.NE.0)  GO TO 10
*                                                            END FASTAT
  999 END
 
