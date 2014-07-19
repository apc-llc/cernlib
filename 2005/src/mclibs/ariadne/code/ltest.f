      PROGRAM HERA

      call ludebug

C...Call a user supplied routine setting
C...the parameters and switches in LEPTO
      CALL LEPSET


C...Call a user supplied routine setting
C...the parameters and switches in Ariadne
      CALL ARISET


C...Initialize Ariadne to run with LEPTO
      CALL ARINIT('LEPTO')


C...Initialize LEPTO for HERA
      CALL LINIT(0,11,-26.5,820.0,4)


C...Loop over a number of events
      DO 100 IEVE=1,1000


C...Call generate an event with LEPTO
        CALL LEPTO


C...Apply the Dipole Cascade
        CALL AREXEC


C...Call a user supplied analysis routine
C        CALL HERANA


 100  CONTINUE


      END

      SUBROUTINE LEPSET

      COMMON /LEPTOU/ CUT(14),LST(40),PARL(30),X,Y,W2,XQ2,U
      SAVE /LEPTOU/


C...Use EHLQ1 structure functions
      LST(15)=1

C...Suppress printouts from LEPTO
      LST(3)=1

      RETURN
      END

      SUBROUTINE ARISET

      COMMON /ARDAT1/ PARA(40),MSTA(40)
      SAVE /ARDAT1/


C...Set default parameters
      CALL ARTUNE('EMC')

C...Use constant mu (independant of x) in Ariadne
      MSTA(31)=0

      RETURN
      END

      SUBROUTINE HERANA

C...Trivial analysis - print the event
      CALL LULIST(1)

      RETURN
      END
