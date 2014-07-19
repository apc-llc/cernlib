      PROGRAM LHC

C...Call a user supplied routine setting 
C...the parameters and switches in PYTHIA
      CALL PYTSET

C...Call a user supplied routine setting 
C...the parameters and switches in Ariadne
      CALL ARISET

C...Initialize Ariadne to run with PYTHIA
      CALL ARINIT('PYTHIA')

C...Initialize PYTHIA for LHC
      CALL PYINIT('CMS','p+','p+',17000.0)

C...Loop over a number of events
      DO 100 IEVE=1,10

C...Call generate an event with PYTHIA
        CALL PYEVNT

C...Apply the Dipole Cascade
        CALL AREXEC

C...Call a user supplied analysis routine
        CALL LHCANA

100   CONTINUE

      END

      SUBROUTINE PYTSET

      COMMON /PYSUBS/ MSEL,MSUB(200),KFIN(2,-40:40),CKIN(200)
      SAVE /PYSUBS/
      COMMON /PYPARS/ MSTP(200),PARP(200),MSTI(200),PARI(200)
      SAVE /PYPARS/


C...Select QCD high-p_T processes
      MSEL=1

C...Set cutoff in p_T-hat
      CKIN(3)=10.0

C...Switch off fragmentation
      MSTP(111)=0

      RETURN
      END

      SUBROUTINE ARISET

C...Use default settings in Ariadne

      CALL ARTUNE('EMC')

      RETURN
      END

      SUBROUTINE LHCANA

C...Trivial analysis - print the event
      CALL LULIST(1)

      RETURN
      END
