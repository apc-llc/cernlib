CDECK  ID>, HELPPR.
      SUBROUTINE HELPPR

C-    Print file-name syntax

      COMMON /QUNIT/ IQREAD,IQPRNT, IQTTIN,IQTYPE, IQOFFL,IQRTTY,IQRSAV
     +,              IQRFD,IQRRD,IQRSIZ, NQLPAT,NQUSED,NQLLBL, NQINIT

      CHARACTER    MPUBLI*(*)
      PARAMETER   (MPUBLI = '/cern/pro/src/car/')


      WRITE (IQTYPE,9001)
      WRITE (IQTYPE,9002) MPUBLI
      WRITE (IQTYPE,9003)
      RETURN

 9001 FORMAT (/' To get program specific help you can give the H option'
     F/' in the calls to the Patchy Auxiliary programs.'
     F/' What follows concerns the parameter acquisition dialog.'/
     F/' For each remaining stream give the File-name/Option-string as'
     F/' parameter value in order. You may give 1 or more parameters'
     F/' on a line; the machine will keep displaying the remaining'
     F/' streams until its list is satisfied. The answer to the final'
     F/' confirmation query is GO, or NO to kill the run.'/
     F/' Parameters must be separated by 1 or more blanks,'
     F/' except: multiple "-" do not need imbedded blanks.' /
     F/' Special parameter values :'
     F/'        - : this stream is not used / option string void,'
     F/'            except READ or PRINT : assume "TTY"'
     F/' . or .go : use "-" for this and all remaining streams'
     F/'      TTY : use standard input/output (only for READ+PRINT),'
     F/'      EOF : void input (only for READ)'
     F/'     HELP : as you guessed.' )

 9002 FORMAT (/' Normal parameters values:'
     F/'  File name (case-sensitive for UNIX):'
     F/'       DIR/FN.EXT  path name; DIR/ or .EXT  may be omitted,'
     F/'                   a default extension is added to the'
     F/'                   path name unless it contains a dot.'
     F/'     /:NAME        use NAME exactly as typed'
     F/'      +DIR/FN.EXT  open output file for Append'
     F/'      =DIR/FN.EXT  the = is ignored for back-compatibility'
     F/'      :DIR/FN.EXT  the : is ignored for back-compatibility'
     F/'     ~/DIR/FN.EXT  = $HOME/DIR/FN.EXT relative to the home dir.'
     F/'    ../DIR/FN.EXT    relative to the current wk directory'
     F/'     _/DIR/FN.EXT  = ',A,'DIR/FN.EXT  public files'
     F/'       (X)/FN.EXT  = ${X}/FN.EXT  env.variable for back-compat.'
     F/'       LNAME.      Link name, no default extension added,'
     F/'                   terminating dot removed')
 9003 FORMAT (
     F/'     ${X} or $X occuring in a file name is replaced by the'
     F/'                contents of environment variable X'/
     F/'  Option string:'
     F/'              OPT  string of characters, maybe prefixed by /:'
     F/'                   each character selecting one option.'/)
      END
