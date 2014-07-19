/*
 * $Id: kmfile.c,v 1.1.1.1 1996/03/08 15:33:08 mclareni Exp $
 *
 * $Log: kmfile.c,v $
 * Revision 1.1.1.1  1996/03/08 15:33:08  mclareni
 * Kuip
 *
 */
/*CMZ :  2.05/00 24/03/94  17.50.15  by  N.Cremel*/
/*-- Author : Nicole Cremel*/
/***********************************************************************
 *                                                                     *
 * Filesystem scanning routines.                                       *
 *                                                                     *
 ***********************************************************************/

#include "kuip/kuip.h"
#include "kuip/mkutfu.h"
#include "mkutda.h"

static char **ftype_list = NULL;
static char **class_list = NULL;
static char **stext_list = NULL;
static int *strip_list = NULL;
static int ntype_list = 0;


static int comp_list( s1, s2 )
     void **s1;
     void **s2;
{
  return strcmp( *s1, *s2 );
}


/*
 * Register the class name and short text for a file name with extension ftype
 */
void km_file_type( ftype, class, stext, strip )
     char *ftype;
     char *class;
     char *stext;
     int strip;
{
  if( ntype_list++ == 0 ) {
    ftype_list = (char**)malloc( sizeof(char*) );
    class_list = (char**)malloc( sizeof(char*) );
    stext_list = (char**)malloc( sizeof(char*) );
    strip_list = (int*)malloc( sizeof(int) );
  }
  else {
    ftype_list = (char**)realloc( (char*)ftype_list,
                                 ntype_list * sizeof(char*) );
    class_list = (char**)realloc( (char*)class_list,
                                 ntype_list * sizeof(char*) );
    stext_list = (char**)realloc( (char*)stext_list,
                                 ntype_list * sizeof(char*) );
    strip_list = (int*)realloc( (char*)strip_list,
                                 ntype_list * sizeof(int) );
  }
  ftype_list[ntype_list-1] = strdup( ftype );
  class_list[ntype_list-1] = strdup( class );
  stext_list[ntype_list-1] = strdup( stext );
  strip_list[ntype_list-1] = strip;
}


/***********************************************************************
 *                                                                     *
 *   Scan directory using filter                                       *
 *                                                                     *
 ***********************************************************************/
char **km_scan_dir( bpath, def_filter, n, strip )
     char *bpath;                /* directory path */
     char *def_filter;
     int n;
     int strip;
{
  static char     *obj_desc[4];
  static char    **dirlist;
  static int       ndlist;
  static char    **filelist;
  static int       nflist;
  static char     *path;
  static char     *filter;
  int i;

  if( n == 0 ) {
#ifdef vms
    char           *p;
#else
    DIR            *dirp;
    struct dirent  *dp;
    char           *pattern;
    char           *re;
#endif
    struct stat     filestatus;

    ndlist     = 0;
    nflist     = 0;

    path = bpath;
    if( path == NULL || *path == '\0' ) {
      static char cwd[256];
      path = getcwd( cwd, (sizeof cwd) );
    }

    filter = km_get_br_filter();
    if( filter == NULL || *filter == '\0' || strip )
      filter = def_filter;

    /* allocate space for the list that will contain all filtered entries */
    dirlist    = (char**)malloc( sizeof(char*) );
    filelist   = (char**)malloc( sizeof(char*) );

    obj_desc[1] = "DirUpFile";
    obj_desc[2] = NULL;
    obj_desc[3] = "(... go up)";

#ifdef vms

    obj_desc[0] = "[-]";

    for( i = 0; i < 2; i++ ) {
      struct dsc$descriptor_s fspec_dsc;
      struct dsc$descriptor_d match_dsc;
      int context = 0;
      int istat;

      fspec_dsc.dsc$b_dtype   = DSC$K_DTYPE_T;
      fspec_dsc.dsc$b_class   = DSC$K_CLASS_S;
      if( i == 0 )
        fspec_dsc.dsc$a_pointer = str2dup( path, "*.DIR" );
      else {
        fspec_dsc.dsc$a_pointer = str2dup( path, filter );
        if( strchr( filter, '.' ) == NULL )
#ifdef ALL_CYCLE /* vms */
          if (km_all_cycles == 1)
              fspec_dsc.dsc$a_pointer =
                    mstrcat( fspec_dsc.dsc$a_pointer, ".*;*" );
          else
#else
          fspec_dsc.dsc$a_pointer = mstrcat( fspec_dsc.dsc$a_pointer, ".*" );
#endif
      }
      fspec_dsc.dsc$w_length  = strlen( fspec_dsc.dsc$a_pointer );

      match_dsc.dsc$b_dtype   = DSC$K_DTYPE_T;
      match_dsc.dsc$b_class   = DSC$K_CLASS_D;
      match_dsc.dsc$w_length  = 0;
      match_dsc.dsc$a_pointer = NULL;

      while( 1 ) {
        istat = lib$find_file( &fspec_dsc, &match_dsc, &context );
        if( istat != RMS$_NORMAL )
          break;

        if( i == 0 ) {
          /* directories first */
          char *dirname = strndup( match_dsc.dsc$a_pointer,
                                  match_dsc.dsc$w_length );
          /* transform "disk:[dir]sub.DIR" into "[.sub] */
          char *p = strrchr( dirname, ']' );
          if( p == NULL )
            p = strrchr( dirname, ':' );
          if( p != NULL )
            strcpy( dirname, p + 1 );
          if( (p = strchr( dirname, '.' )) != NULL )
            *p = '\0';
          dirlist = (char**)realloc( (char*)dirlist,
                                    (ndlist+1) * sizeof(char*) );
          dirlist[ndlist++] = str3dup( "[.", dirname, "]" );
          free( dirname );
        }

        else {
          /* regular files */
          char *filename = strndup( match_dsc.dsc$a_pointer,
                                   match_dsc.dsc$w_length );
          if( strstr( filename, ".DIR;" ) == NULL ) {
            /* it is not a directory again matching the wildcard */
            char *p = strrchr( filename, ';' );
            if( p != NULL )     /* remove version number */
              *p = '\0';
            p = strrchr( filename, ']' );
            if( p == NULL )
              p = strrchr( filename, ':' );
            if( p == NULL )
              p = filename;
            else
              p++;
            filelist = (char**)realloc( (char*)filelist,
                                       (nflist+1) * sizeof(char*) );
            filelist[nflist++] = strdup( p );
          }
          free( filename );
        }
      }
      lib$find_file_end( &context );
      free( fspec_dsc.dsc$a_pointer );
    }

#else /* UNIX */

    /* translate file wildcard characters to regex specials */
    pattern = strdup( "^" );
    for( i = 0; filter[i] != '\0'; i++ ) {
      switch( filter[i] ) {
      case '*':
        pattern = mstrcat( pattern, ".*" );
        break;
      case '?':
        pattern = mstrcat( pattern, "." );
        break;
      case '.':
        pattern = mstrcat( pattern, "\\." );
        break;
      default:
        pattern = mstrccat( pattern, filter[i], 1 );
        break;
      }
    }
    pattern = mstrcat( pattern, "$" );
#ifdef MATCH_RE_COMP
    re = re_comp( pattern );
#else
    re = regcmp( pattern, NULL );
#endif
    free( pattern );

    /*
     * Change directory to path and open the directory.
     * Next read the whole directory and store the matching entries
     * into filelist.
     */
    if( (dirp = opendir( path )) != NULL ) {
      char dirpath[256];
      int lpath = strlen( path );
      strcpy( dirpath, path );
      strcpy( dirpath + lpath, "/" );
      lpath++;

      while( (dp = readdir(dirp)) != NULL ) {

        if( strcmp( dp->d_name, "." ) == 0
           || strcmp( dp->d_name, ".." ) == 0 )
          /* ignore pseudo directories */
          continue;

        strcpy( dirpath + lpath, dp->d_name );
        if( stat( dirpath, &filestatus ) != 0 )
          continue;

        if( (filestatus.st_mode & S_IFMT) == S_IFDIR ) {
          /* directories always go into the list */
          dirlist = (char**)realloc( (char*)dirlist,
                                    (ndlist+1) * sizeof(char*) );
          dirlist[ndlist] = strdup( dp->d_name );
          ndlist++;
        }
        else if( (filestatus.st_mode & S_IFMT) == S_IFREG ) {
          /* regular file: match name against regexp */
#ifdef MATCH_RE_COMP
          int match = re_exec( dp->d_name );
#else
          int match = (regex( re, dp->d_name ) != NULL);
#endif
          if( match == 1 ) {
            filelist = (char**)realloc( (char*)filelist,
                                       (nflist+1) * sizeof(char*) );
            filelist[nflist] = strdup( dp->d_name );
            nflist++;
          }
        }
      }
      closedir( dirp );
    }

#ifndef MATCH_RE_COMP
    free( re );
#endif

    obj_desc[0] = "..";

#endif /* VMS or UNIX */

    /* alphabetically sort the entries */
    qsort( (void*)dirlist, ndlist, sizeof(char*), (int(*)())comp_list );
    qsort( (void*)filelist, nflist, sizeof(char*), (int(*)())comp_list );

    return obj_desc;            /* .. */
  }

  if( --n < ndlist ) {          /* return directories first */
    static char dirpath[256];
    char *dirname = dirlist[n];
    obj_desc[0] = dirname;
    obj_desc[1] = "DirFile";
    strcpy( dirpath, path );
#ifdef vms
    strcat( dirpath, dirname + 2 );
    strcpy( dirpath + strlen( dirpath ) - 1, ".DIR;*" );
#else
    strcat( dirpath, "/" );
    strcat( dirpath, dirname );
#endif
    obj_desc[2] = dirpath;
    obj_desc[3] = "(Directory)";
    return obj_desc;
  }

  else if( n < ndlist + nflist ) { /* regular files next */
    static char filepath[256];
    char *filename = filelist[n-ndlist];
    int lfile = strlen( filename );

    obj_desc[0] = filename;
    strcpy( filepath, path );
#ifdef vms
    strcat( filepath, filename );
#else
    strcat( filepath, "/" );
    strcat( filepath, filename );
#endif
    obj_desc[2] = filepath;

    /* set the default class name depending on file permission */
#ifdef vms
    if( access( filepath, W_OK ) == 0 ) {
      obj_desc[1] = "VmsRwFile";
      obj_desc[3] = "(Read/Write)";
    }
    else if( access( filepath, R_OK ) == 0 ) {
      obj_desc[1] = "VmsRoFile";
      obj_desc[3] = "(Read Only)";
    }
    else {
      obj_desc[1] = "VmsNoFile";
      obj_desc[3] = "(No Access)";
    }
#else
    if( access( filepath, X_OK ) == 0 ) {
      obj_desc[1] = "ExFile";
      obj_desc[3] = "(Executable)";
    }
    else if( access( filepath, W_OK ) == 0 ) {
      obj_desc[1] = "RwFile";
      obj_desc[3] = "(Read/Write)";
    }
    else if( access( filepath, R_OK ) == 0 ) {
      obj_desc[1] = "RoFile";
      obj_desc[3] = "(Read Only)";
    }
    else {
      obj_desc[1] = "NoFile";
      obj_desc[3] = "(No Access)";
    }
#endif

    if( ntype_list == 0 ) {
      km_file_type( ".kumac", "MacFile", "(Kuip Macro)", 1 );
      km_file_type( ".ps", "PSFile", "(PostScript File)", 1 );
      km_file_type( ".eps", "EPSFile", "(Encapsulated PostScript File)", 1 );
#ifdef vms
      km_file_type( ".com", "VmsComFile", "(Command Script)", 0 );
      km_file_type( ".exe", "VmsExeFile", "(Executable)", 0 );
#endif
    }

    /* override default class name depending on file type */
    for( i = 0; i < ntype_list; i++ ) {
      int ltype = strlen( ftype_list[i] );
#ifdef vms
      /* ignore version number in comparsion */
      char *p = strchr( filename, ';' );
      if( p != NULL )
        lfile = p - filename;
#ifdef ALL_CYCLE /* vms */
      if (km_all_cycles == 0)
         filename[lfile] = '\0'; /* chop off version number */
#endif
#endif
      if( strncasecmp( filename + lfile - ltype,
                      ftype_list[i], ltype ) == 0 ) {
        obj_desc[1] = class_list[i];
        obj_desc[3] = stext_list[i];
        if( strip_list[i] && strip )
          filename[lfile-ltype] = '\0'; /* chop off file extension */
        break;
      }
    }
    return obj_desc;
  }

  /* cleanup after end of list */
  for( i = 0; i < ndlist; i++ )
    free( dirlist[i] );
  free( (char*)dirlist );

  for( i = 0; i < nflist; i++ )
    free( filelist[i] );
  free( (char*)filelist );

  return NULL;
}


