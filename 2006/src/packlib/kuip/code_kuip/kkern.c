/*
 * $Id: kkern.c,v 1.2 1996/05/01 12:08:38 gunter Exp $
 *
 * $Log: kkern.c,v $
 * Revision 1.2  1996/05/01 12:08:38  gunter
 * Replace several strcpy by memmove
 *
 * Revision 1.1.1.1  1996/03/08 15:32:57  mclareni
 * Kuip
 *
 */
/*CMZ :  2.06/03 09/01/95  10.05.13  by  N.Cremel*/
/*-- Author :    Alfred Nathaniel   07/08/92*/
/* kkern.c */

#include "kuip/kuip.h"

/*
 * On some systems, e.g. IBM C/370, realloc() becomes very expensive if
 * there are a lot of allocated blocks and the number of bytes spills into
 * a different bucket size.  We can lessen the effect by allocating for
 * string operations (strdup(), mstrcat()), a larger initial block size
 * in order to make realloc() effectively a null operation.
 */
#ifndef ALLOC_MIN_BYTES
# define ALLOC_MIN_BYTES 1
#endif


/*
 * return length of Fortran string ignoring trailing blanks
 */
size_t fstrlen( const char *str,
               size_t len )
{
  int *p;
  static int blanks = 0;

  if( len >= (sizeof blanks) * 4 ) {

    /* check single characters until next word boundary */
    switch( (int)&str[len] & ( (sizeof blanks) - 1 ) ) {
    case 7:
      if( str[--len] != ' ' ) return len + 1;
    case 6:
      if( str[--len] != ' ' ) return len + 1;
    case 5:
      if( str[--len] != ' ' ) return len + 1;
    case 4:
      if( str[--len] != ' ' ) return len + 1;
    case 3:
      if( str[--len] != ' ' ) return len + 1;
    case 2:
      if( str[--len] != ' ' ) return len + 1;
    case 1:
      if( str[--len] != ' ' ) return len + 1;
    }

    if( blanks == 0 )
      memset( (char*)&blanks, ' ', (sizeof blanks) );

    /* check for full words of blanks */
    for( p = (int*)&str[len]; len >= (sizeof blanks) && *--p == blanks; )
      len -= (sizeof blanks);
  }

  /* check single characters again */
  while( len > 0 && str[len-1] == ' ' )
    len--;

  return len;
}


/*
 * copy Fortran string to allocated memory stripping trailing blanks
 */
char *fstrdup( const char *str,
              size_t len )
{
  return strndup( str, fstrlen( str, len ) );
}


/*
 * like fstrdup() but return NULL if string is empty
 */
char *fstr0dup( const char *str,
               size_t len )
{
  size_t n = fstrlen( str, len );
  if( n == 0 )
    return NULL;
  return strndup( str, n );
}


/*
 * like fstrdup() but trim leading blanks
 */
char *fstrtrim( const char *str,
               size_t len )
{
  while( len > 0 && *str == ' ' ) {
    len--;
    str++;
  }
  return fstrdup( str, len );
}


/*
 * like fstr0dup() but trim leading blanks
 */
char *fstr0trim( const char *str,
                size_t len )
{
  while( len > 0 && *str == ' ' ) {
    len--;
    str++;
  }
  return fstr0dup( str, len );
}


/*
 * assign a Fortran string and return the logical length
 */
size_t fstrset( char *str,
               size_t len,
               const char *cstr )
{
  size_t n = 0;
  size_t i;

  memset( str, ' ', len );
  if( cstr != NULL ) {
    for( i = 0; i < len && cstr[i] != '\0'; i++ ) {
      if( (str[i] = cstr[i]) != ' ' )
        n = i + 1;
    }
#if 0
    Unfortunately we cannot do this test for truncation because KUGETC/S
    is sometimes called with a short dummy string to load things for KUGETL
    if( cstr[i] != '\0' ) {
      while( cstr[i] == ' ' )
        i++;
      if( cstr[i] != '\0' ) {
        printf(
" *** A character parameter has been defined with insufficient length = %d\n",
               len );
      }
    }
#endif
  }
  return n;
}


/*
 * create a Fortran string array from a pointer array
 */
char *fstrvec( char **pstr,
              size_t n,
              size_t *mlen )
{
  char *fstr;

  if( n > 0 ) {
    int *plen = (int*)calloc( n, sizeof(int) );
    int len = 1;
    int nlen;
    int i;

    for( i = 0; i < n; i++ ) {
      if( pstr[i] != NULL ) {
        plen[i] = strlen( pstr[i] );
        if( plen[i] > len )
          len = plen[i];
      }
    }

    /* vector elements are padded to multiple of 8 */
    *mlen = len = ((len - 1) / 8 + 1 ) * 8;
    nlen = n * len;
    fstr = malloc( nlen + 1 );
    memset( fstr, ' ', nlen );
    fstr[nlen] = '\0';

    for( i = 0; i < n; i++ ) {
      if( plen[i] > 0 ) {
        memcpy( fstr + i * len, pstr[i], plen[i] );
      }
    }
    free( (char*)plen );
  }

  else {
    int nlen = 8;
    fstr = malloc( nlen + 1 );
    memset( fstr, ' ', nlen );
    fstr[nlen] = '\0';
  }

  return fstr;
}


/*
 * like strtod() but (*tail != '\0') can be used to test for bad numbers
 * trailing blanks are ignored
 */
double fstrtod( const char *str,
               char **tail )
{
  double value = strtod( str, tail );
  const char *ptail = *tail;

  if( *str == '\0'
     || ( ptail == str + 1 && !isdigit( str[0] ) ) ) {
    /*
     * in case str == ""
     * and stupid VMS RTL accepts "+" and "-" as valid numbers
     */
    *tail = "error";
  }
  else if( ptail != str ) {
    while( **tail == ' ' )
      (*tail)++;                /* ignore trailing blanks */
  }

  return value;
}


/*
 * like strtol() but (*tail != '\0') can be used to test for bad numbers
 * trailing blanks are ignored
 */
int fstrtoi( const char *str,
            char **tail )
{
  int value = strtol( str, tail, 10 );
  const char *ptail = *tail;

  if( *str == '\0' ) {
    *tail = "error";
  }
  else if( ptail != str ) {
    while( **tail == ' ' )
      (*tail)++;                /* ignore trailing blanks */
  }

  return value;
}


/*
 * convert double value into string representation with prec significant digits
 * equivalent to "%G" which behaves improperly on VAX/VMS
 */
char *strfromd( double dval,
               size_t prec )
{
  static char buffer[32];
  char *digit = "0123456789";
  char *p = buffer;
  double round = 5;
  double d;
  int i;

  if( dval == 0 )
    return "0";

  if( prec <= 0 )
    prec = 6;

  for( i = 0; i < prec; i++ )
    round *= 0.1;

  if( dval < 0 ) {
    *p++ = '-';
    dval = -dval;
  }

  d = dval;
  while( d < 1 ) {
    d *= 10;
    round *= 0.1;
  }
  while( d >= 10 ) {
    d *= 0.1;
    round *= 10;
  }
  dval += round;

  if( dval < 1 && dval >= 0.0001 ) {

    *p++ = '0';
    *p++ = '.';
    while( dval < 0.1 ) {
      *p++ = '0';
      dval *= 10;
    }
    for( i = 0; i < prec; i++ ) {
      int ival;
      dval *= 10;
      ival = dval;
      *p++ = digit[ival];
      dval -= ival;
    }
    while( p[-1] == '0' )
      p--;
    if( p[-1] == '.' )
      p--;
  }

  else {
    int nexp = 0;
    int ndot = 0;

    if( dval >= 1000000 ) {
      while( dval >= 10 ) {
        dval *= 0.1;
        nexp++;
      }
    }

    while( dval < 1 ) {
      dval *= 10;
      nexp--;
    }

    while( dval >= 10 ) {
      dval *= 0.1;
      ndot++;
    }

    for( i = 0; i < prec; i++ ) {
      int ival = dval;
      *p++ = digit[ival];
      if( i == ndot )
        *p++ = '.';
      dval = (dval - ival) * 10;
    }

    if( ndot < prec ) {
      while( p[-1] == '0' )
        p--;
    }
    if( p[-1] == '.' )
      p--;

    if( nexp != 0 ) {
      *p++ = 'E';
      if( nexp < 0 ) {
        *p++ = '-';
        nexp = -nexp;
      }
      else
        *p++ = '+';

      if( nexp >= 100 ) {
        *p++ = digit[nexp/100];
        nexp %= 100;
      }
      *p++ = digit[nexp/10];
      *p++ = digit[nexp%10];
    }
  }

  *p = '\0';
  return buffer;
}


/*
 * convert int value into string representation with prec significant digits
 * equivalent to "%.<n>d" which behaves improperly on VAX/VMS
 */
char *strfromi( int ival,
               size_t prec )
{
  static char buffer[3][32];
  static int nbuf = 0;
  char *p;
  int n;
  int i;

  if( ++nbuf >= 3 )
    nbuf = 0;
  p = buffer[nbuf];

  if( ival < 0 ) {
    *p++ = '-';
    ival = -ival;
  }

  n = ival;
  while( n >= 10 ) {
    n /= 10;
    prec--;
  }

  for( i = 0; i < prec - 1; i++ )
    *p++ = '0';

  sprintf( p, "%d", ival );

  return buffer[nbuf];
}


#ifndef HAVE_MEMMOVE
/*
 * memory move with possible overlap
 */
void *memmove( void *vdst,
              const void *vsrc,
              size_t n )
{
  char *dst = (char*)vdst;
  const char *src = (char*)vsrc;
  if( dst != src && n > 0 ) {

    if( dst > src && src + n < dst ) {
      while( n > 0 ) {
        n--;
        dst[n] = src[n];
      }
    }
    else
      memcpy( dst, src, n );
  }
  return dst;
}
#endif


#ifndef HAVE_STRCASECMP
/*
 * case insensitive string compare
 */
int strcasecmp( const char *str1,
               const char *str2 )
{
  return strncasecmp( str1, str2, strlen( str2 ) + 1 );
}


/*
 * case insensitive string compare of n characters
 */
int strncasecmp( const char *str1,
                const char *str2,
                size_t n )
{
  while( n > 0 ) {
    int c1 = *str1;
    int c2 = *str2;

    if( isupper( c1 ) )
      c1 = tolower( c1 );

    if( isupper( c2 ) )
      c2 = tolower( c2 );

    if( c1 != c2 )
      return c1 - c2;

    str1++;
    str2++;
    n--;
  }
  return 0;
}
#endif


/*
 * find last occurence of anyone character
 */
char *strrpbrk( const char *str1,
               const char *str2 )
{
  char *p = strpbrk( str1, str2 );
  if( p != NULL ) {
    char *s;
    while( (s = strpbrk( p + 1, str2 )) != NULL )
      p = s;
  }
  return p;
}


#ifndef HAVE_STRRSTR
/*
 * find last occurence of substring
 */
char *strrstr( const char *str1,
              const char *str2 )
{
  if( str1 == NULL || str2 == NULL )
    return NULL;

  else if( *str2 == '\0' )
    return (char*)str1;

  else {
    char *p = strstr( str1, str2 );
    if( p != NULL ) {
      char *s;
      while( (s = strstr( p + 1, str2 )) != NULL )
        p = s;
    }
    return p;
  }
}
#endif


/*
 * copy string to allocated memory
 */
char *Strdup( const char *str )
{
  size_t bytes = strlen( str ) + 1;
  if( bytes < ALLOC_MIN_BYTES )
    bytes = ALLOC_MIN_BYTES;
  return strcpy( malloc( bytes ), str );
}


/*
 * like strdup() but return NULL if string is NULL or ""
 */
char *str0dup( const char *str )
{
  if( str == NULL || str[0] == '\0' )
    return NULL;
  return Strdup( str );
}


/*
 * like strdup() but string is n characters long and not terminated
 */
char *strndup( const char *str,
              size_t len )
{
  size_t bytes = len + 1;
  char *p;

  if( bytes < ALLOC_MIN_BYTES )
    bytes = ALLOC_MIN_BYTES;

  p = strncpy( malloc( bytes ), str, len );
  p[len] = '\0';
  return p;
}


/*
 * strdup() of character representation of integer n
 */
char *stridup( int n )
{
  char buf[16];
  sprintf( buf, "%d", n );
  return Strdup( buf );
}


/*
 * like strdup() but concatenate two strings
 */
char *str2dup( const char *str1,
              const char *str2 )
{
  return mstrcat( Strdup( str1 ), str2 );
}


/*
 * like strdup() but concatenate three strings
 */
char *str3dup( const char *str1,
              const char *str2,
              const char *str3 )
{
  return mstr2cat( Strdup( str1 ), str2, str3 );
}


/*
 * like strdup() but concatenate four strings
 */
char *str4dup( const char *str1,
              const char *str2,
              const char *str3,
              const char *str4 )
{
  return mstr3cat( Strdup( str1 ), str2, str3, str4 );
}


/*
 * like strdup() but concatenate five strings
 */
char *str5dup( const char *str1,
              const char *str2,
              const char *str3,
              const char *str4,
              const char *str5 )
{
  return mstr4cat( Strdup( str1 ), str2, str3, str4, str5 );
}


/*
 * concatenate two strings if the first one is allocated
 */
char *mstrcat( char *str1,
              const char *str2 )
{
  if( str1 == NULL )
    return Strdup( str2 );
  else {
    size_t len1 = strlen( str1 );
    size_t len2 = strlen( str2 );
    size_t bytes = len1 + len2 + 1;
    if( bytes < ALLOC_MIN_BYTES )
      bytes = ALLOC_MIN_BYTES;
    str1 = realloc( str1, bytes );
    strcpy( &str1[len1], str2 );
    return str1;
  }
}


/*
 * like mstrcat() but concatenate three strings
 */
char *mstr2cat( char *str1,
               const char *str2,
               const char *str3 )
{
  return mstrcat( mstrcat( str1, str2 ), str3 );
}


/*
 * like mstrcat() but concatenate four strings
 */
char *mstr3cat( char *str1,
               const char *str2,
               const char *str3,
               const char *str4 )
{
  return mstr2cat( mstrcat( str1, str2 ), str3, str4 );
}


/*
 * like mstrcat() but concatenate five strings
 */
char *mstr4cat( char *str1,
               const char *str2,
               const char *str3,
               const char *str4,
               const char *str5 )
{
  return mstr3cat( mstrcat( str1, str2 ), str3, str4, str5 );
}


/*
 * like mstrcat() but the second string is not terminated
 */
char *mstrncat( char *str1,
               const char *str2,
               size_t len2 )
{
  if( len2 > 0 ) {
    size_t len1 = strlen( str1 );
    size_t bytes = len1 + len2 + 1;
    if( bytes < ALLOC_MIN_BYTES )
      bytes = ALLOC_MIN_BYTES;
    str1 = realloc( str1, bytes );
    strncpy( &str1[len1], str2, len2 );
    str1[len1+len2] = '\0';
  }
  return str1;
}


/*
 * append n times the character c to the allocated string
 */
char *mstrccat( char *str,
               char c,
               size_t n )
{
  if( n > 0 ) {
    size_t len = strlen( str );
    size_t bytes = len + n + 1;
    if( bytes < ALLOC_MIN_BYTES )
      bytes = ALLOC_MIN_BYTES;
    str = realloc( str, bytes );
    while( n-- > 0 )
      str[len++] = c;
    str[len] = '\0';
  }
  return str;
}


/*
 * append the string representation of an integer to the allocated string
 */
char *mstricat( char *str,
               int n )
{
  char nstr[16];

  sprintf( nstr, "%d", n );
  return mstrcat( str, nstr );
}


/*
 * find the maximum string length in an array of strings
 * if n == 0 the array is NULL terminated
 */
size_t mstrlen( char **str,
               size_t n )
{
  size_t nmax = (n != 0) ? n : (~0 >> 1);
  size_t mlen = 0;
  int i;

  for( i = 0; i < nmax; i++ ) {
    if( str[i] != NULL ) {
      size_t len = strlen( str[i] );
      if( len > mlen )
        mlen = len;
    }
    else if( n == 0 )
      break;
  }

  return mlen;
}


/*
 * protect blanks inside quoted strings and do strtok( str, " " )
 */
char *strqtok( char *str )
{
  char *tok;
  char *p;

  if( str != NULL ) {
    int quoted = 0;
    for( p = str; *p != '\0'; p++ ) {
      if( *p == '\'' )
        quoted = !quoted;
      else if( *p == ' ' && quoted )
        *p = '\n';
    }
  }

  tok = strtok( str, " " );
  if( tok != NULL ) {
    for( p = tok; *p != '\0'; p++ ) {
      if( *p == '\n' )
        *p = ' ';
    }
  }
  return tok;
}


/*
 * convert a string to lowercase in place
 */
char *strlower( char *str )
{
  if( str != NULL ) {
    char *p;
    for( p = str; *p != '\0'; p++ )
      if( isupper( *p ) )
        *p = tolower( *p );
  }
  return str;
}


/*
 * convert a string to uppercase in place
 */
char *strupper( char *str )
{
  if( str != NULL ) {
    char *p;
    for( p = str; *p != '\0'; p++ )
      if( islower( *p ) )
        *p = toupper( *p );
  }
  return str;
}


/*
 * trim leading and trailing blanks
 */
char *strtrim( char *str )
{
  int n = strlen( str );
  while( n > 0 && str[n-1] == ' ' )
    n--;
  str[n] = '\0';
  for( n = 0; str[n] == ' '; )
    n++;
  if( n > 0 )  memmove( str, str + n, strlen(str+n) + 1);
/*  Overlapping, use memmove    strcpy( str, str + n );*/
  return str;
}


/*
 * replace tabs by an equivalent number of blanks and remove trailing blanks
 * argument string must be malloced
 */
char *struntab( char *line )
{
  char *p;
  int i;

  while( (p = strchr( line, '\t' )) != NULL ) {
    int n = p - line;
    char *newline = strndup( line, n );
    newline = mstrccat( newline, ' ', 8 - n % 8 );
    newline = mstrcat( newline, p + 1 );
    free( line );
    line = newline;
  }

  for( i = strlen( line ); i > 0; i-- ) {
    if( line[i-1] != ' ' && line[i-1] != '\n' ) {
      break;
    }
  }
  line[i] = '\0';

  return line;
}

/*
 * replace blanks inside a string with '0'
 */
char *strblk0( char *str )
{
  int i;

  for( i = 0; i < strlen( str ); i++) {
     if (str[i] == ' ') str[i] = '0';
  }

  return str;
}


/*
 * Calculate a 32-bit checksum for the file content.
 */
int checksum( const char *path )
{
  static int bogus_crc = 0x12345678;
  FILE *stream = fopen( path, "r" );

  if( stream != NULL ) {
    unsigned crc = 0;
    int c;
    while( (c = getc( stream )) != EOF ) {
      int carry = crc & 1;
      crc = ( ( crc >> 1 ) & 0x7FFFFFFF ) + ( c & 0xFF );
      if( carry )
        crc |= 0x80000000;
    }
    fclose( stream );
    return crc;
  }
  return ++bogus_crc;
}


#ifdef IBM370
/*
 * return dummy process id
 */
int getpid()
{
  return 1;
}
#endif


#ifdef IBMVM
/*
 * copy file modification date into stamp buffer
 */
int get_stamp( path, stamp )
     char *path;
     TimeStamp *stamp;
{
  FILE *stream;
  char cmd[120];
  char *p;

  sprintf( cmd, "LISTFILE %s (DATE EXEC", path );
  while( (p = strchr( cmd, '.' )) != NULL )
    *p = ' ';

  if( system( cmd ) == 0
     && (stream = fopen( "CMS.EXEC", "r" )) != NULL
     && fgets( cmd, (sizeof cmd), stream ) != NULL ) {
    fclose( stream );
    remove( "CMS.EXEC" );
    strncpy( *stamp, cmd + 63, 17 );
    (*stamp)[17] = '\0';
    return 0;
  }
  return -1;
}
#endif


#ifdef NEED_STRDUP
# ifdef strdup
#  undef strdup
# endif
char *strdup( const char* str )
{
  return Strdup( str );
}
#endif
