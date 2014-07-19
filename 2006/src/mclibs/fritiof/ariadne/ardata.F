*
* $Id: ardata.F,v 1.1.1.1 1996/01/11 14:05:16 mclareni Exp $
*
* $Log: ardata.F,v $
* Revision 1.1.1.1  1996/01/11 14:05:16  mclareni
* Fritiof
*
*
C***********************************************************************
 
      BLOCK DATA ARDATA
 
C...ARiadne block DATA statements
 
C...Initialization of the common blocks used in Ariadne
 
 
      PARAMETER(MAXDIP=500,MAXPAR=500,MAXSTR=100)
 
      IMPLICIT DOUBLE PRECISION (D)
      IMPLICIT DOUBLE PRECISION (B)
      IMPLICIT LOGICAL (Q)
 
      COMMON /ARDAT1/ PARA(40),MSTA(40)
      SAVE /ARDAT1/
 
      COMMON /ARDAT2/ PQMAS(10)
      SAVE /ARDAT2/
 
      COMMON /ARDAT3/ IWRN(40)
      SAVE /ARDAT3/
 
C...Breif explanation of parameters and switches:
C...
C...
C...Parameters:
C...
C...PARA(1) (D=0.200) lambda_QCD
C...PARA(2) (D=0.200) Constant alpha_QCD for MSTA(12)=0
C...PARA(3) (D=1.000) Cutoff in invariant p_t for QCD emission
C...PARA(4) (D=1/137) Constant alpha_EM
C...PARA(5) (D=1.000) Cutoff in invariant p_t for EM emission
C...PARA(6) (D=-1.00) Maximum allowed invariant p_t (if >0)
C...PARA(7) (D=0.000) Maximum invariant mass (if >0)
C...PARA(8-9) not used
C...PARA(10)(D=1.000) Power in soft suppression (dimnsionality of
C...                  the extended source)
C...PARA(11)(D=0.938) Soft suppression parameter for code 1
C...PARA(12)(D=0.938) Soft suppression parameter for code 2
C...PARA(13)(D=0.938) Soft suppression parameter for code 3
C...PARA(14-19) not used
C...PARA(20)(D=1.000) Minimum p_t^2/Q^2 of q-qbar pair in boson-gluon
C...                  fusion to be allowed to be treated as such. Else
C...                  treated as sea-quark interaction. Only used when
C...                  running with LEPTO version 6.0 or higher
C...PARA(21-30) not used
C...PARA(31)(D=1.000) Maximum invariant p_t^2 for clustering three jets
C...                  into two in ARCLUS
C...PARA(32-38) not used
C...PARA(39)(D=0.001) Tolerance factor for momentum conservation
C...PARA(40)(D=1E32)  Maximum allowed floating point number ("minimum"
C...                  is 1/PARA(40)
C...
C...Switches:
C...
C...MSTA(1) (R)       Ariadne mode (set by ARINIT) for treatment of
C...                  incomming events.
C...         0 =>      No special treatment
C...         1 =>      as if produced by JETSET
C...         2 =>      as if produced by PYTHIA
C...         3 =>      as if produced by LEPTO
C...MSTA(2) (I)       Initialization done and headers written
C...MSTA(3) (D=0)     Setting of parameters in  Ariadne, JETSET,
C...                  PYTHIA and LEPTO to suitable values.
C...         0 =>      off
C...         1 =>      on
C...MSTA(4) (I)       Number of calls made to AREXEC
C...MSTA(5) (D=0)     Perform fragmentation at the end of AREXEC
C...         0 =>      off
C...         1 =>      on
C...                  When running with JETSET, PYTHIA or LEPTO this
C...                  switch is set to the value of the corresponding
C...                  switch in these programs.
C...MSTA(6) (D=-1)    Maximum number of emission (per string) in a
C...                  AREXEC call (if <0 - no limit)
C...MSTA(7) (D=6)     File number for output (stdout) from Ariadne
C...                  set to MSTU(11) by ARINIT
C...MSTA(8) (D=6)     File number for error messages (stdout) from
C...                  Ariadne
C...MSTA(9) (D=1)     Debug mode
C...         0 =>      debug off
C...         1 =>      check that energy and momentum is conserved after
C...                   each call to AREXEC produce. Warns if change
C...                   in momentum is larger a factor PARA(39)
C...         2 =>      as for 1 but check every emission
C...         3 =>      as for 2 but dump string to /LUJETS/ after each
C...                   emission
C...MSTA(10)(D=5)     Maximum number of warnings (of each kind) issued
C...                  by Ariadne
C...MSTA(11)(D=0)     Phase space restrictions. The maximum p_t of an
C...                  emission is set to the p_t of the last emission
C...                  (otherwise no restrictions) for:
C...                    gluon  q-qbar  photon  emissions
C...         0 =>        yes     yes     yes
C...         1 =>        yes     yes     no
C...         2 =>        yes     no      yes
C...         3 =>        yes     no      no
C...         4 =>        no      no      no
C...MSTA(12)(D=1)     Running alpha_QCD
C...         0 =>      off
C...         1 =>      on
C...MSTA(13) (R)      Error experienced by Ariadne in last call to
C...                  AREXECc. Reset to 0 at each call to AREXEC
C...MSTA(14)(D=1)     The maximum allowed p_t is set to the minimum
C...                  invariant p_t of all gluons in an incomming
C...                  string
C...         0 =>      off
C...         1 =>      on
C...MSTA(15)(D=5)     Number of flavours allowed in q-qbar emission
C...MSTA(16)(D=2)     Recoil treatment
C...         0 =>      minimize p_t1^2 + p_t3^2
C...         1 =>      as for 0 but pointlike string ends takes
C...                   all recoil
C...         2=>       as for 0 but also extended string ends which
C...                   have a>0 takes full recoil
C...MSTA(17)(D=2)     Recoil treatment for extended dipoles
C...         0 =>      no special treatment (but cf. MSTA(16))
C...         1 =>      emit recoil gluon (except if pointlike quark
C...                   in other dipole end for MSTA(16)=1)
C...         2 =>      emit recoilgluon according to new strategy
C...MSTA(18)(D=1)     P_t ordering of recoil gluons
C...         0 =>      off
C...         1 =>      on
C...MSTA(19)(D=1)     Correct or quick treatment of emissions from
C...                  heavy quarks
C...         0 =>      quick
C...         1 =>      correct
C...MSTA(20)(D=0)     Final state photon radiation
C...         0 =>      off
C...         1 =>      on
C...         2 =>      on but turned off at the first occurence of
C...                   q-qbar emission in a string.
C...MSTA(21)(D=0)     Photon radiation when run with PYTHIA or LEPTO
C...         0 =>      off
C...         1 =>      on
C...MSTA(22)(D=0)     Transfer of recoils in Drell-Yan processes
C...         0 =>      off
C...         1 =>      on
C...MSTA(23)(I)       Line number of particle to transfer recoil to
C...                  for MSTA(22) > 0
C...MSTA(24)(D=2)     Quark masses to be used in q-qbar emissions
C...         0 =>      as specified in PMAS(1-8) in /ARDAT2/
C...         1 =>      "bare" quark masses as specified in PMAS(1-8)
C...                   in /LUDAT2/
C...         2 =>      "constituent" quark masses as specified in
C...                   PARF(101-108) in /LUDAT2/
C...MSTA(25-29) not used
C...MSTA(30)(D=1)    various options for running with Lepto
C...         0 =>      Stuck quark point like, remnant extended with PARA(11)
C...         1 =>      as 0 but remnant extended with PARA(11)/(1-x)
C...         2 =>      as 1 bur struck quark extended with Q
C...MSTA(31)(D=1)    mass of extended partons
C...         0 =>      set to zero for backward compatibility
C...         1 =>      keeps value given
C...MSTA(32-40) not used
C...
C...End of description
 
      DATA PARA/0.2,0.2,1.0,0.007297353,1.0,-1.0,0.0,0.0,0.0,1.0,
     $          0.938,0.938,0.938,6*0.0,1.0,
     $          10*0.0,
     $          1.0,7*0.0,0.001,1.0E32/
      DATA MSTA/0,0,0,0,0,-1,6,6,1,5,
     $          0,1,0,1,5,2,2,1,1,0,
     $          0,0,0,2,5*0,1,
     $          1,9*0/
      DATA PQMAS/10*0.0/
      DATA IWRN/40*0/
 
 
C**** END OF ARDATA ****************************************************
      END
