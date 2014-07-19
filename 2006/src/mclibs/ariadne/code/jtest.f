      PROGRAM LEP


C...Call a user supplied routine setting
C...the parameters and switches in JETSET
      CALL SETJET


C...Call a user supplied routine setting
C...the parameters and switches in Ariadne
      CALL ARISET


C...Initialize Ariadne to run with JETSET
      CALL ARINIT('JETSET')


C...Loop over a number of events
      DO 100 IEVE=1,10


C...Generate an LEP event with JETSET
        CALL LUEEVT(0,91.0)


C...Apply the Dipole Cascade
        CALL AREXEC


C...Call a user supplied analysis routine
        CALL LEPANA


 100  CONTINUE


      END

      SUBROUTINE SETJET

      COMMON /LUDAT3/ MDCY(500,3),MDME(2000,2),BRAT(2000),KFDP(2000,5)
      SAVE /LUDAT3/


C...Set pi0 stable
      MDCY(LUCOMP(111),1)=0

      RETURN 
      END

      SUBROUTINE ARISET

      COMMON /ARDAT1/ PARA(40),MSTA(40)
      SAVE /ARDAT1/


C...Allow for photon radiation off quarks
      MSTA(20)=1

      RETURN
      END

      SUBROUTINE LEPANA

C...Perform Jet clustering
      CALL ARCLUS(NJET)

C...Trivial analysis - print the event
      CALL LULIST(1)

      RETURN
      END
