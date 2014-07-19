C######################################################################C
C                                                                      C
C                          L E P T O                                   C
C                                                                      C
C                  A Monte Carlo Generator for                         C
C            Deep Inelastic Lepton-Nucleon Scattering                  C
C                                                                      C
C                  version 6.5.1,  October 31, 1996                    C
C                                                                      C
C   Authors:                                                           C
C   Gunnar Ingelman, DESY theory group     Dept. of Radiation Sciences C
C                    (room 202  bldg 2a)   Uppsala University          C
C                    D-22603 Hamburg, FRG  S-751 21 Uppsala, Sweden    C
C             phone: +49(40)8998-2795      +46(18)18-3884              C
C               fax:            -2777               -3833              C
C            e-mail: ingelman@desy.de      (ingelman@tsl.uu.se)        C
C   Anders Edin, Dept. of Radiation Sciences, edin@tsl.uu.se           C
C   Johan Rathsman, Dept. of Radiation Sciences, rathsman@tsl.uu.se    C
C                                                                      C
C   Contributions on parton cascades: M. Bengtsson, T. Sjostrand       C
C                                                                      C
C   Home page: http://www3.tsl.uu.se/thep/lepto with code and manual   C
C                                                                      C
C   Manual: G.Ingelman, A.Edin, J.Rathsman, DESY 96-057                C
C                                                                      C
C   Please report any problems or suggestions for improvements.        C
C                                                                      C
C######################################################################C

      SUBROUTINE LTIMEX(TIME)
C...Interface routine to transfer a call to some machine-dependent
C...routine to get the execution time used since job started.
C...Nice, but not necessary information. Can also be called by user.

      TIME=0.
C...Use of CERN library routine Z007, replace/delete if not available.
      CALL TIMEX(TIME)
      RETURN
      END
