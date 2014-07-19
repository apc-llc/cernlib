*
* $Id: hdiffb.F,v 1.1.1.1 1996/01/16 17:07:56 mclareni Exp $
*
* $Log: hdiffb.F,v $
* Revision 1.1.1.1  1996/01/16 17:07:56  mclareni
* First import
*
*
#include "hbook/pilot.h"
*CMZ :  4.20/00 11/06/93  08.28.24  by  Rene Brun
*-- Author :    R. J. Genik II   23/10/92
      SUBROUTINE HDIFFB(ID1,ID2,TOL,NBINS,CHOPT,NBAD,DIFFS)
C------------------------------------------------------------------------
C-
C-   Purpose and Methods : A bin-by-bin comparator for the HBOOK utility.
C-
C-   Inputs  : ID1         First histogram to compare (Reference)
C-             ID2         Second histogram ID (Data)
C-             TOL         Tolerence for passing a test
C-             NBINS       Number of bins in comparison
C-             CHOPT       String holding the function options
C-                              N  Use absolute contents of histogram
C-                              D  Debug printout
C-                              O  Use overflow bins
C-                              U  Use underflow bins
C-                              S  Statistical comparison
C-                              C  Compatibility test
C-                              A  Absolute test
C-                              Z  Zero supress and pass
C-                              R  Right overflow bin
C-                              L  Left underflow bin
C-                              T  Top overflow bin
C-                              B  Bottom underflow bin
C-
C-   Outputs : NBAD        Number of bins failing test
C-             DIFFS       Array holding the individual bin results
C-   Controls: None
C-
C-   Created  30-JUN-1989   Jason McCampbell
C-                          Michigan State University, USA
C-   MODIFIED 20-AUG-1990   James T. McKinley
C-                          Michigan State University, USA
C-   MODIFIED 23-AUG-1992   R. J. Genik II
C-                          Michigan State University, USA
C-   MODIFIED  8-MAR-1993   R. J. Genik II
C-                          Michigan State University, USA
C-
C-    A-OPTION Now returns signed DIFFS values
C-
C----------------------------------------------------------------------
C   NOTE: DIFFS is treated as a 1-d array, even with 2-d output.
C----------------------------------------------------------------------
C
C
C   NOTES ON USE: production release October 1992, D-zero version.
C
C   The 4 libraries required are: HBOOK4 (Or later version), GENLIB,
C   KERNLIB, and PACKLIB.
C
C----------------------------------------------------------------------
C
C   Local variable declarations for top HDIFFB routine
C
C----------------------------------------------------------------------
C
#if defined(CERNLIB_HDIFFBDOC)
      --------------------
 
      CALL HDIFFB(ID1,ID2,TOL,NBINS,CHOPT,NBAD*,DIFFS*)
 
 
Action: Compare two histograms, bin by bin. For each bin, return the
        probability that the contents are from the same distribution.  For
        details of the method see below.
 
        The comparison may be done between two 1-dimensional histograms, two
        2-dimensional histograms, or between two profile histograms.
 
Input Parameters:
 
         ID1 the first histogram to be compared.  The "reference" histogram
             in options A and C.
 
         ID2 the second histogram to be compared.  The "data" histogram in
             options A and C.
 
             ID1, ID2 are a pair of histograms, scatterplots, or profile
             histograms booked with the same number of bins.
 
         TOL is the tolerance for a passing the test.
 
             Under options S and C, TOL is a number between 0 and 1 which
             represents the smallest probability considered as an acceptable
             match. TOL = 0.05 will cause DIFFS to reject the bin as bad
             if there is less than a 5% probability the two bins came
             from the same distribution.
 
             Under option A, TOL is the degree of precision  of match
             required for the test to be considered as passed. TOL=2.0
             means that a data bin differing from the reference mean by
             less than 2.0 times the reference error is compatible.
 
       NBINS is the number of bins in the comparison. For a 1-dimensional
             histogram, this is the number of bins plus 0, 1 or 2, depending
             on whether the overflow and underflow channels are included.
             For a 2-dimensional histogram, this will have the total number of
             bins plus room for overflow bins along any of the axes requested.
             For more detail, see the discussion of DIFFS below.
 
       CHOPT is a string allowing specification of the following options:
 
             N  Use the absolute contents of each histogram, thus including the
                normalization of the histogram as well as its shape in the
                comparison.  By default, for the S and C options, in 1- and
                2-dimensional histograms, the means are adjusted for the
                relative numbers of entries (including any overflow or
                underflow bins requested) in ID1 and ID2.  No adjustment is
                ever made for profile histograms.
 
             O  Overflow, requests that overflow bins be taken into account.
 
             U  Underflow, requests that underflow bins be taken into account.
 
             R  Right overflow bin. For a 2-dimensional histogram, it includes
                the X-Axis overflow bin in the comparisons.  If the O option
                is used, this is automatic.
 
             L  Left underflow bin.  Same as above, but the X-Axis underflow is
                used.  The U option uses this automatically.
 
             T  Top overflow bin.  Same as R but for the Y-Axis
 
             B  Bottom underflow bin.  Option L for the Y-Axis
 
             S  Statistical comparison. Calculates the probability that both
                bins were produced from a distribution with the same
                mean. This probability is referred to in TOL and DIFFS.
 
             C  Compatibility test.  Considers bins of the reference histogram
                (ID1) as perfectly describing the true distribution.
                Calculates the probability that the data (from ID2) was
                produced from that distribution.   For 1- or 2-dimensional
                histograms, the Poisson mean is deduced from ID1.  For profile
                histograms, the test assumes a Gaussian with mean and standard
                deviation given by the ID1. The C option should be used when
                comparing data to a function, a well-known reference, or a
                calibration distribution.
 
             A  Absolute test. Like the C test, except that TOL and DIFFS are in
                terms of the number of standard deviations, rather probability.
                The test is on the number of standard deviations by which the
                data from ID2 deviates from the mean.  Both the mean and the
                standard deviation are deduced from ID1 .
 
                Error bars must be on for this option.  This forbids overflow
                bins, underflow bins, and 2-dimensional histograms.  The A
                option ignores bins with zero contents in reference histogram.
 
             Z  Ignores bins with zero contents in the comparison.  For the S
                option, ignores bins with zero contents in either histogram.
                For the C and A option, ignores bins with zero contents in the
                reference histogram.  The default action is to consider all
                bins as significant.
 
             D  Debug printout, dumps the critical variables in the comparisons,
                along with indicators of its weight, etc.
 
             The default (no options selected) does the S option (statistical
             comparison), ignores underflow and overflow bins, and automatically
             corrects for the difference in entries between ID1 and ID2.
 
 
Output Parameters:
 
       NBAD* is the number of bins failing the comparison according to the
             criteria defined by TOL and CHOPT.
 
      DIFFS* is an array of length the number of bins being compared, which
             gives the results of the test bin by bin (confidence levels for
             options S and C, deviations for A).  The results are passed back
             in the form:
 
             1-dimensional:
                   DIFFS(NX) for no over or under flow, or for profile histogram
                   DIFFS(0:NX), for underflow
                   DIFFS(NX+1), for overflow
                   DIFFS(0:NX+1), for overflow and underflow
              2-dimensional:
                   DIFFS(NX,NY) or similarly to above depending on
                   which overflow/underflow options selected
 
             The input array must be dimensioned this way in order to be able to
             find the results for each bin.
 
             Note: The calculation of DIFFS is dependent upon the choice of TOL,
                   and the contents of each bin, in addition to the type of
                   histograms and the test selected (see technical notes).
 
 
When to use HDIFFB instead of HDIFF
 
    HDIFFB treats the histogram bins individually, while HDIFF treats the
histogram as a whole.  In HDIFF, one is comparing the overall shapes of a
probability distribution. Typically, an event is entered only in one
channel, and the choice of channel depends on a measured value of a continuous
coordinate, so that it makes sense for downward fluctuations in one bin to be
considered as compensated by upward fluctuations in another bin.  In HDIFFB,
each bin is considered independently, except, perhaps, for an overall
normalization factor which is the sum over all bins.
 
Thus HDIFFB is appropriate when
      - it makes sense to identify a single channel as "bad", for example if the
          bin contents correspond to hits in a given detector element
      - the data is heterogeneous, for example if the contents are counts vs
          trigger bit
      - you have already found a discrepancy on a shape with HDIFF and wish to
          focus on where disagreement is worst.
 
    A plot of hits vs detector element, where the detector elements cover some
angular range is an example of a histogram which might be considered with either
comparison utility.  The choice depends on the question you wish to answer:
 
   - If you want to know if the angular distribution looks the same, use HDIFF.
   - If you want a report on bad detector elements, use HDIFFB.
 
 
Choice of TOL
 
    If you choose .05 for TOL, you should expect 5 or so bad bins per trial
from a histogram with 100 channels.  For monitoring, you must compromise
between the number of false messages you can tolerate (based on the total
number of channels you monitor), and the amount of data you will need to
collect to claim a channel is bad.  In general, a somewhat smaller fraction of
channels than TOL will be flagged as bad, since for discrete distributions
(Poisson statistics), the probability is quantized.  For example, the
probability might be .053 for 4 entries, and .021 for 3.  If TOL=0.05,
only bins with 3 or fewer entries would be flagged as bad.
 
When to use the S option
 
    The S option should be used when both histograms are filled with statistical
data, for example a momentum distribution from two successive data runs.  Using
the S option when comparing data to a function or known reference yields poor
results because it attributes errors to both histograms. In this case, the
C option should be selected.
 
When to use the C  option
 
    The C option assumes that the reference histogram contains the theoretically
expected values with no (or negligible) errors.  Examples might be a flat
distribution hand-inserted as the expectation for a phi distribution, or
a long data run to be compared with shorter data runs.
 
When to use the A option
 
    The A option can be used as an equivalent to the C test by choosing TOL
in terms of standard deviations instead of probability, and returns z values
in DIFFS for each bin.
 
    The A option is intended for setting by hand absolute minima and maxima.
To restrict an efficiency between 80 and 100 %, load the reference histogram
with a mean of .9 (by HPAK) and the error bar of .1 (via HPAKE), and use HDIFFB
with TOL = 1.0 and the A option.  The N option should also be selected for this
application.
 
Comparison of Weighted vs Unweighted events
 
    This is in general undesirable, as it forces you into the less accurate
    Gaussian approximation.  Thus it is preferable, for example, to have
    unweighted Monte Carlo events if you need to use HDIFFB to compare with
    data.  The only useful case is if the weighted histogram is the reference
    histogram in the C comparison, which only makes sense if you have much
    better accuracy than your data.
 
Using Profile histograms
 
    The N option is irrelevant for profile histograms.  The overflow/underflow
    options are illegal for profile histograms because insufficient information
    is stored to calculate the error bars.  None of the test options (S, C, or
    A) check on the number of entries in a profile histogram bin.  To do that,
    make a separate 1-dimensional histogram.  This has an unexpected effect
    when the number of entries are small.  Bins with no entries always pass the
    S and C options (no data is compatible with any distribution), so in such
    cases more bins pass than called for by TOL.
 
Values of DIFFS
 
    The value of DIFFS may depend somewhat on the value of TOL chosen, as
    the approximation chosen to calculate DIFFS depends on both the number of
    entries and on the size of TOL (how accurately DIFFS must be calcuated).
 
    The S-option sometimes returns a confidence level of 1.0 in the small
    statistics calculation, i.e. there is no probability that the two numbers
    came from different distributions.  This is due to finite precision.
    Values slightly higher than 1.0 will be returned when the two content
    values are identical, since no statistical test could claim they came from
    different distributions.
 
Other notes
 
    The normalization scaling (used unless N option selected) is based on
    channel contents for all channels requested (including overflow/underflow),
    provided you select one of the overflow/underflow options.
 
    Negative bin contents are flagged as bad bins in S, C options.
 
Statistical methods and numerical notes:
 
    (For simplicity, this is written as if the N option were in effect.)
 
    The methods used for the S and C mode are correct for unweighted events and
    Poisson statistics for 1- or 2- dimensional histograms.  Errors may result
    in either the S and C options for small tolerances if bin contents are
    greater than the largest allowed integer.
 
        For the S test with unweighted events, the test (which is uniformly most
    powerful) treats  N = sum of the two bin contents as having chosen via a
    binomial distibution which histogram to enter.  The binomial parameter p is
    given by the relative normalization of the histograms (.5 if the total
    number of entries in each histogram was the same). For DIFFS values greater
    than TOL, the first two digits are correct. For values less than TOL, the
    two digits to the right of the first non-zero TOL digit are significant.
    i.e. for  TOL=0.0001, 0.000xxx are significant.  One can force higher
    accuracy by setting TOL smaller (or even 0), but calculation time will
    increase, and warning messages will be issued.  A Gaussian approximation is
    used when there are 25 or more events in each bin, and TOL > .001 .
 
        The C test for unweighted events in the data histogram simply calculates
    the Poisson probability of finding n, the ID2 bin value,  given a mean
    equal to the bin value of ID1.  A Gaussian approximation is used when the
    the mean is 10**6 or larger, and TOL is .001 or larger.  Given the
    expected mean, the choice of TOL implies bounds (n<,n>) on n.  An error
    occurs when the approximations in calculating DIFFS give an incorrect value
    for n< or n>.  No such errors occur for mean < 10**5 and TOL > 10**-15.
    The errors in n< or n> are less than 2 for mean < 10**6, TOL > 10**-6, or
    mean < 10**7, TOL > 10**-5.  There is a maximum n beyond which DIFFS
    returns zero, so bins with n > nmax always pass.  For mean < 10**7, this
    is irrelevant for values of TOL > 10**-9 .
 
        For the profile histogram S test, HDIFFB calculates the t test
    probability that both bin means were produced from a population with the
    same mean.  The C test calculates the probability of finding the
    value in ID1 given a Gaussian with mean and sigma given by the ID2 contents.
    Small numbers of entries for either test give DIFFS values which are
    too large, and HDIFFB will reject too many events in profile histograms.
 
        For weighted events, the S and C tests use a Gaussian approximation.
    This results in DIFFS values which are too low.  HDIFFB rejects too many
    bins for weighted events, particularly for small numbers of equivalent
    events.
 
 
 
 
 
Errors reported by HDIFFB:
 
Warning: Zero tolerance.
    The passed value TOL is less than or equal to 0. TOL = 0. can be
    used to force highest accuracy in the S-option.
 
Warning: Only one comparison at a time, please.
    More than one type of comparison was selected.  Only one of options S, C,
    and A may be used. The defaults S option will be used.
 
Warning: Different binning.
    The XMin values for a 1-dimensional histogram or the XMin and/or YMin
    values on a 2-dimensional histogram are different.  This may give
    inaccurate results.
 
Warning: Weighted or saturated events in 2-dimensions.
    HBOOK does not compute error bars for two dimensional histograms, thus
    weighted event are not allowed, and HDIFFB can not compute the correct
    statistics.  An answer is still given, but it is probably not right.
    The only reliable case is a weighted 2-dimenension histogram as the
    reference histogram for the C test.
 
Sum of histogram contents is zero!
    The sum of the content bins is zero.
 
Both histograms must be the same dimension.
    A 1-dimensional and a 2-dimensional histogram have been specified.  In
    order for the routine to work, both must be the same dimensionality.
 
Both histograms must be the standard or profile type.
    Two different types of histograms have been specified.  Both must be
    profile or non-profile.  You cannot mix types.
 
Not enough bins DIFF to hold result.
    The parameter NBINS is less that the number of bins in the histograms.
 
Number of channels is different.
    The number of channels in the two histograms to compare are different. They
    must be the same before the routine will process the data.
 
U/O/L/R/T/B Option with weighted events.
    HBOOK  does not compute an error bar for over-/under-flow bins, thus it may
    not be used with weighted events.
 
U/O/L/R/T/B Option with profile histograms.
    HBOOK  does not compute an error bar for over-/under-flow bins, thus it may
    not be used with profile histograms.
 
Weighted options and no HBARX.
    The user had not told HBOOK to figure the error bars for the histograms.
    Therefore, the operations will not be valid.
 
A-option with no error bars on reference histogram.
    The user has not told HBOOK to compute error bars for the reference
    histogram. This error is also returned when the user attempts to select
    A-option to compare 2-dimensional histograms.
 
 
Authors:
 
        R. J. Genik II, J. McKinley, J. McCampbell, J. Linnemann, D. Gilliland
        Michigan State University.  J. Linnemann (LINNEMAN@MSUPA is the contact
        person)
#endif
#include "hbook/hcdifb.inc"
      INTEGER ID1, ID2, NBAD, NBINS
      REAL    TOL, DIFFS(NBINS)
      CHARACTER*(*) CHOPT
      LOGICAL ERRORS
C
C
C=====================================================================
C Call initialization and error checking routine HDBINI
C=====================================================================
C
C
C
      CALL HDBINI(ID1,ID2,TOL,NBINS,CHOPT,ERRORS)
      IF(ERRORS) GOTO 999
C
C
C=====================================================================
C   Do actual comparisons.
C=====================================================================
C
      IF (PROFIL) THEN
        CALL HDBPRF(TOL,NBINS,NBAD,DIFFS)
C                                                  ! Profile S,C,or A
      ELSEIF (OPTS(COPTN).EQ.1) THEN
C                                                  ! C option
        CALL HDBCOP(TOL,NBINS,NBAD,DIFFS)
      ELSEIF (OPTS(AOPTN).EQ.1) THEN
C                                                  ! A option
        CALL HDBAOP(TOL,NBINS,NBAD,DIFFS)
      ELSE
C                                                  ! S option
        CALL HDBSOP(TOL,NBINS,NBAD,DIFFS)
      ENDIF
C
C=====================================================================
C   Go back from whence we came...
C=====================================================================
  999 RETURN
      END
