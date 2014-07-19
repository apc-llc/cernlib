*
* $Id: psplot.F,v 1.1.1.1 1996/03/01 11:39:28 mclareni Exp $
*
* $Log: psplot.F,v $
* Revision 1.1.1.1  1996/03/01 11:39:28  mclareni
* Paw
*
*
#include "paw/pilot.h"
*CMZ :  2.03/07 26/08/93  14.57.51  by  Fons Rademakers
*-- Author :    Alfred Nathaniel   02/05/93
      SUBROUTINE PSPLOT
      CHARACTER*(*) CHOPT, CHCASE
*
* simple HPLOT and HIGZ remote calls with a single message
*
      CHARACTER CHMAIL*80

      RETURN

      ENTRY IGPID(ILEVI,CHCASE,IPID,CHOPT)
      WRITE(CHMAIL,'(A,2I12)') 'IGPID ',ILEVI,IPID
      L=LENOCC(CHMAIL)+1
      CHMAIL(L:)=CHOPT
      CHMAIL(L+16:)=CHCASE
      GOTO 1

      ENTRY IGSET(CHCASE,RVAL)
      WRITE(CHMAIL,'(A,E16.8,A)') 'IGSET ',RVAL,CHCASE
      GOTO 1

      ENTRY ISPMCI(ICOLI)
      WRITE(CHMAIL,'(A,I12)') 'ISPMCI',ICOLI
      GOTO 1

      ENTRY HPLOT(ID,CHOPT,CHCASE,NUMM)
      CALL PFHOUT(ID,ISTAT)
      IF(ISTAT.NE.0) RETURN
      WRITE(CHMAIL,'(A,2I12,2A16)') 'HPLOT ',ID,NUMM,CHOPT,CHCASE
      GOTO 1

      ENTRY HPRINT(ID)
      CALL PFHOUT(ID,ISTAT)
      IF (ISTAT .NE. 0) RETURN
      WRITE(CHMAIL,'(A,I12)') 'HPRINT', ID
      GOTO 1

      ENTRY HPLSTA(ID,CHCASE,NUMM)
      CALL PFHOUT(ID,ISTAT)
      IF(ISTAT.NE.0) RETURN
      WRITE(CHMAIL,'(A,2I12,A)') 'HPLSTA',ID,NUMM,CHCASE
      GOTO 1

      ENTRY HPLTIT(CHCASE)
      CHMAIL='HPLTIT'//CHCASE
      GOTO 1

 1    CONTINUE
      CALL CZPUTA(CHMAIL,ISTAT)

      END
