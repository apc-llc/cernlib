/*
 * $Id: kicon.c,v 1.1.1.1 1996/03/08 15:32:56 mclareni Exp $
 *
 * $Log: kicon.c,v $
 * Revision 1.1.1.1  1996/03/08 15:32:56  mclareni
 * Kuip
 *
 */
/*CMZ :  2.07/06 14/06/95  15.23.01  by  Gunter Folger*/
/*-- Author :    Alfred Nathaniel   06/11/92*/
/* kicon.c: browser icons */

#include "kuip/kuip.h"
#include "kuip/kfor.h"
#include "kuip/klink.h"
#include "kuip/kmenu.h"
#include "kuip/kflag.h"
#include "kuip/kbrow.h"


static KmIcon   *kmicons;       /* list of icon bitmaps */

EXTERN char cdf_def[80], cdf_value[80];


/*
 * action routine for /MOTIF/ICON
 */
int kxicon()
{
  char *icon_name = ku_gets();
  char *file_name = ku_getf();

  if( strcmp( icon_name, "*" ) == 0 )
    icon_name = NULL;

  if( km_icon( icon_name, file_name ) == 0 )
    return -1;
  else
    return 0;
}


/*
 * read bitmap data from file and store icon definition
 * if icon_name == NULL use name in file
 * return the number of bitmaps read in
 */
int km_icon( icon_name, fname )
     const char *icon_name;
     const char *fname;
{
  FILE *stream;

  if( (stream = fopen( fname, "r" )) == NULL ) {
    printf( " *** /MOTIF/ICON: cannot open file %s\n", fname );
    return -1;
  }
  else {
    KmIcon *icon;
    KmIcon *kmicn;
    char line[1024];
    char *word1;
    char *word2;
    char *word3;
    char *p;
    int nline = 0;
    int nicons = 0;

  again:
    icon = (KmIcon*)malloc( sizeof(KmIcon) );

    while( fgets( line, (sizeof line), stream ) != NULL ) {
      nline++;
      word1 = strtok( line, " " );

      if( word1 != NULL && strcmp( word1, "#define" ) == 0 ) {

        word2 = strtok( NULL, " " );
        word3 = strtok( NULL, " " );

        if( word3 == NULL || (p = strrchr( word2, '_' )) == NULL
           || strcmp( p, "_width" ) != 0
           || (icon->width = atoi( word3 )) <= 0 ) {

          printf( " *** MOTIF/ICON: invalid width in line %d of %s\n",
                 nline, fname );
          free( (char*)icon );
          fclose( stream );
          return nicons;
        }

        if( icon_name == NULL ) {
          *p = '\0';
          icon->name = strdup( word2 );
        }
        else {
          icon->name = strdup( icon_name );
        }

        fgets( line, (sizeof line), stream );
        nline++;
        word1 = strtok( line, " " );
        word2 = strtok( NULL, " " );
        word3 = strtok( NULL, " " );

        if( word3 == NULL || (p = strrchr( word2, '_' )) == NULL
           || strcmp( p, "_height" ) != 0
           || (icon->height = atoi( word3 )) <= 0 ) {

          printf( " *** MOTIF/ICON: invalid height in line %d of %s\n",
                 nline, fname );
          free( icon->name );
          free( (char*)icon );
          fclose( stream );
          return nicons;
        }

        while( fgets( line, (sizeof line), stream ) != NULL ) {
          nline++;
          word1 = strtok( line, " " );
          if( word1 != NULL && strcmp( word1, "static" ) == 0 ) {
            int nbytes = ( ( icon->width - 1 ) / 8 + 1 ) * icon->height;
            int n = 0;

            icon->bitmap = malloc( nbytes );
            p = NULL;
            while( n < nbytes ) {

              if( p != NULL ) {
                p = strtok( NULL, " ,};\n" );
              }
              if( p == NULL ) {
                nline++;
                if( fgets( line, (sizeof line), stream ) == NULL ) {
                  printf( " *** MOTIF/ICON: unexpected EOF in line %d of %s\n",
                         nline, fname );
                  free( icon->name );
                  free( icon->bitmap );
                  free( (char*)icon );
                  fclose( stream );
                  return nicons;
                }
                p = strtok( line, " ,};\n" );
              }
              icon->bitmap[n] = strtol( p, NULL, 0 );
              n++;
            }

            for( kmicn = kmicons; kmicn != NULL; kmicn = kmicn->next ) {
              if( strcasecmp( kmicn->name, icon->name ) == 0 ) {
                /* redefinition of icon */
                kmicn->width = icon->width;
                kmicn->height = icon->height;
                kmicn->bitmap = icon->bitmap;
                kmicn->pix = 0;
                kmicn->hi_pix = 0;
                free( icon->name );
                free( (char*)icon );
                break;
              }
              if( kmicn->next == NULL ) {
                /* add new icon at end of list */
                icon->next = NULL;
                icon->pix = 0;
                icon->hi_pix = 0;
                kmicn->next = icon;
                break;
              }
            }
            nicons++;
            if( icon_name == NULL ) {
              goto again;
            }
            fclose( stream );
            return nicons;
          }
        }
        free( icon->name );
      }
    }
    if( nicons == 0 ) {
      printf( " *** MOTIF/ICON: invalid file content of %s\n", fname );
    }
    free( (char*)icon );
    fclose( stream );
    return nicons;
  }
}


/*
 * add to the list of icons
 */
void klnkicon( KmIcon *kmicn_list,
              int version )
{
  strcpy ( cdf_def, "Icon" );
  strcpy ( cdf_value, kmicn_list->name );
  check_version( version, KUIP_VERSION );

  if( kmicons == NULL )
    kmicons = kmicn_list;
  else {
    KmIcon *kmicn = kmicons;
    while( kmicn->next != NULL )
      kmicn = kmicn->next;
    kmicn->next = kmicn_list;
  }
}


/*
 * return pointer to bitmap data or NULL if name not found
 */
KmIcon *find_kmicon( const char *kmicn_name )
{
  KmIcon *kmicn;

  for( kmicn = kmicons; kmicn != NULL; kmicn = kmicn->next )
    if( strcasecmp( kmicn->name, kmicn_name ) == 0 )
      break;
  return kmicn;
}
